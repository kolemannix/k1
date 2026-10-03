// FIXME Human review of promote.rs
use super::*;

const MAX_SLOTS_PER_ALLOCA: usize = 64;

#[derive(Clone, Copy)]
enum LocalAddress {
    ConstOffset(InstId, u32),
    UnknownOffset(InstId),
}

impl LocalAddress {
    fn alloca(self) -> InstId {
        match self {
            LocalAddress::ConstOffset(alloca, _) | LocalAddress::UnknownOffset(alloca) => alloca,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum AllocaPromotion {
    RemoveAlloca,
    KeepAlloca,
    Rejected,
}

#[derive(Clone, Copy)]
enum LocalAccessKind {
    Load(ScalarType),
    Store(ScalarType, Value),
    Zero {
        size: u32,
    },
    OverwriteFromMemory {
        src_addr: Value,
        size: u32,
        align: u32,
        unaligned: bool,
        src_local: Option<(InstId, u32)>,
    },
}

#[derive(Clone, Copy)]
struct LocalAccess {
    inst: InstId,
    block: BlockId,
    alloca: InstId,
    offset: u32,
    kind: LocalAccessKind,
    first_placeholder: u32,
}

#[derive(Clone, Copy)]
struct LocalCopy {
    dst_alloca: InstId,
    dst_offset: u32,
    src_alloca: InstId,
    src_offset: u32,
    size: u32,
}

#[derive(Clone, Copy, PartialEq, Eq)]
struct ScalarSlot {
    alloca: InstId,
    offset: u32,
    t: ScalarType,
}

impl ScalarSlot {
    fn end(self) -> u32 {
        self.offset + self.t.get_layout().size
    }

    fn sort_key(self) -> u128 {
        (self.alloca.as_u32() as u128) << 64 | (self.offset as u128) << 8 | self.t.to_tag() as u128
    }

    fn is_within(self, offset: u32, size: u32) -> bool {
        self.offset >= offset && self.end() <= offset + size
    }
}

#[derive(Clone, Copy)]
struct PendingLoad {
    placeholder: InstId,
    insert_after: InstId,
    base_addr: Value,
    offset: u32,
    t: ScalarType,
    unaligned: bool,
    is_used: bool,
}

#[derive(Clone, Copy)]
struct PromotedPhi {
    inst: InstId,
    t: ScalarType,
    cases_start: u32,
    cases_end: u32,
    is_trivial: bool,
    is_live: bool,
}

#[derive(Clone, Copy)]
struct PendingPhiCase {
    case_index: u32,
    pred: BlockId,
    slot: u32,
}

#[derive(Default)]
pub struct LocalUses {
    alloca_promotions: IdMap<InstId, AllocaPromotion>,
    accesses: Vec<LocalAccess>,
    offset_insts: Vec<(InstId, InstId)>,
    local_copies: Vec<LocalCopy>,
    inst_addresses: IdMap<InstId, LocalAddress>,
}

#[derive(Default)]
pub struct PromoteState {
    local_uses: LocalUses,
    slots: Vec<ScalarSlot>,
    new_slots: Vec<ScalarSlot>,
    alloca_slot_ranges: IdMap<InstId, (u32, u32)>,
    placeholders: Vec<InstId>,
    pending_load_index: IdMap<InstId, u32>,
    pending_loads: Vec<PendingLoad>,
    block_exit_values: FxHashMap<u64, Value>,
    current_values: Vec<(u32, Value)>,
    current_block_stamp: u32,
    phis: Vec<PromotedPhi>,
    phi_cases: Vec<PhiCase>,
    phi_index_of_inst: IdMap<InstId, u32>,
    phi_users: Vec<Vec<u32>>,
    phi_worklist: Vec<u32>,
    live_worklist: Vec<Value>,
    pending_phi_cases: Vec<PendingPhiCase>,
    walked_blocks: Vec<BlockId>,
    unique_preds: Vec<BlockId>,
}

fn make_block_exit_key(slot: u32, block: BlockId) -> u64 {
    ((slot as u64) << 32) | block.as_u32() as u64
}

fn add_address_offset(
    address: Option<LocalAddress>,
    delta: impl FnOnce() -> Option<u64>,
) -> Option<LocalAddress> {
    match address? {
        LocalAddress::ConstOffset(alloca, offset) => {
            Some(match delta().and_then(|d| u32::try_from(offset as u64 + d).ok()) {
                Some(offset) => LocalAddress::ConstOffset(alloca, offset),
                None => LocalAddress::UnknownOffset(alloca),
            })
        }
        unknown => Some(unknown),
    }
}

fn get_const_array_offset(
    k1: &TypedProgram,
    u: &UnitView,
    subst: &ValueSubst,
    element_t: PhysicalType,
    index: Value,
) -> Option<u64> {
    let index = subst.resolve(index).const_bits(u).filter(|i| *i <= u32::MAX as u64)?;
    Some(index * k1.get_pt_layout(element_t).stride() as u64)
}

fn compute_local_address(
    k1: &TypedProgram,
    u: &UnitView,
    subst: &ValueSubst,
    v: Value,
) -> Option<LocalAddress> {
    let Value::Inst(id) = subst.resolve(v) else { return None };
    match *u.inst(id) {
        Inst::Alloca { returned: false, .. } => Some(LocalAddress::ConstOffset(id, 0)),
        Inst::StructOffset { base, vm_offset, .. } => {
            add_address_offset(compute_local_address(k1, u, subst, base), || Some(vm_offset as u64))
        }
        Inst::ArrayOffset { base, element_t, element_index } => {
            add_address_offset(compute_local_address(k1, u, subst, base), || {
                get_const_array_offset(k1, u, subst, element_t, element_index)
            })
        }
        _ => None,
    }
}

impl LocalUses {
    pub fn clear(&mut self) {
        self.alloca_promotions.clear();
        self.accesses.clear();
        self.offset_insts.clear();
        self.local_copies.clear();
        self.inst_addresses.clear();
    }

    pub fn has_allocas(&self) -> bool {
        !self.alloca_promotions.is_empty()
    }

    fn is_rejected(&self, alloca: InstId) -> bool {
        !matches!(
            self.alloca_promotions.get(alloca),
            Some(AllocaPromotion::RemoveAlloca | AllocaPromotion::KeepAlloca)
        )
    }

    fn has_promotable_alloca(&self) -> bool {
        self.alloca_promotions.keys().iter().any(|a| !self.is_rejected(*a))
    }

    fn reject_address(&mut self, address: Option<LocalAddress>) {
        if let Some(address) = address {
            self.alloca_promotions.insert(address.alloca(), AllocaPromotion::Rejected)
        }
    }

    fn reject_copy_endpoints(&mut self, copy: LocalCopy) {
        self.alloca_promotions.insert(copy.src_alloca, AllocaPromotion::Rejected);
        self.alloca_promotions.insert(copy.dst_alloca, AllocaPromotion::Rejected);
    }

    fn reject_sources_of_rejected_copies(&mut self) {
        let mut changed = true;
        while changed {
            changed = false;
            for i in 0..self.local_copies.len() {
                let copy = self.local_copies[i];
                if self.is_rejected(copy.dst_alloca) && !self.is_rejected(copy.src_alloca) {
                    self.alloca_promotions.insert(copy.src_alloca, AllocaPromotion::Rejected);
                    changed = true;
                }
            }
        }
    }

    fn add_access(
        &mut self,
        address: Option<LocalAddress>,
        inst: InstId,
        block: BlockId,
        make_kind: impl FnOnce() -> LocalAccessKind,
    ) {
        match address {
            Some(LocalAddress::ConstOffset(alloca, offset)) => {
                let kind = make_kind();
                self.accesses.push(LocalAccess {
                    inst,
                    block,
                    alloca,
                    offset,
                    kind,
                    first_placeholder: 0,
                })
            }
            unknown => self.reject_address(unknown),
        }
    }

    fn add_offset_inst(&mut self, id: InstId, address: Option<LocalAddress>) {
        if let Some(address) = address {
            self.offset_insts.push((id, address.alloca()));
            self.inst_addresses.insert(id, address);
        }
    }

    pub fn record_inst(
        &mut self,
        k1: &TypedProgram,
        u: &UnitBuf,
        id: InstId,
        inst: &Inst,
        block: BlockId,
    ) {
        if !self.alloca_promotions.is_empty() || matches!(inst, Inst::Alloca { .. }) {
            let lookup_address = |local_uses: &LocalUses, v: Value| match v {
                Value::Inst(id) => local_uses.inst_addresses.get(id),
                _ => None,
            };
            self.record_inst_with(k1, u, &lookup_address, id, inst, block)
        }
    }

    fn record_inst_with(
        &mut self,
        k1: &TypedProgram,
        u: &UnitBuf,
        get_address: &impl Fn(&LocalUses, Value) -> Option<LocalAddress>,
        id: InstId,
        inst: &Inst,
        block: BlockId,
    ) {
        let reject_value = |local_uses: &mut LocalUses, v: Value| {
            let address = get_address(local_uses, v);
            local_uses.reject_address(address)
        };
        match *inst {
            Inst::Alloca { returned: false, .. } => {
                if !self.alloca_promotions.contains(id) {
                    self.alloca_promotions.insert(id, AllocaPromotion::RemoveAlloca);
                }
                self.inst_addresses.insert(id, LocalAddress::ConstOffset(id, 0));
            }
            Inst::Load { t, src, volatile, .. } => {
                let address = get_address(self, src);
                match t.as_scalar() {
                    Some(t) if !volatile => {
                        self.add_access(address, id, block, || LocalAccessKind::Load(t))
                    }
                    _ => self.reject_address(address),
                }
            }
            Inst::Store { dst, value, t, volatile, .. } => {
                let address = get_address(self, dst);
                if volatile {
                    self.reject_address(address);
                } else {
                    self.add_access(address, id, block, || LocalAccessKind::Store(t, value));
                }
                reject_value(self, value);
            }
            Inst::Zero { dst, vm_size, .. } => {
                let address = get_address(self, dst);
                self.add_access(address, id, block, || LocalAccessKind::Zero { size: vm_size })
            }
            Inst::Copy { dst, src, t, vm_size, volatile, unaligned } => {
                let dst_address = get_address(self, dst);
                let src_address = get_address(self, src);
                if volatile {
                    self.reject_address(dst_address);
                    self.reject_address(src_address);
                    return;
                }
                let src_local = match (src_address, dst_address) {
                    (
                        Some(LocalAddress::ConstOffset(src_alloca, src_offset)),
                        Some(LocalAddress::ConstOffset(dst_alloca, dst_offset)),
                    ) => {
                        self.local_copies.push(LocalCopy {
                            dst_alloca,
                            dst_offset,
                            src_alloca,
                            src_offset,
                            size: vm_size,
                        });
                        Some((src_alloca, src_offset))
                    }
                    (src_address, _) => {
                        self.reject_address(src_address);
                        None
                    }
                };
                self.add_access(dst_address, id, block, || {
                    let align = k1.get_pt_layout(t).align;
                    LocalAccessKind::OverwriteFromMemory {
                        src_addr: src,
                        size: vm_size,
                        align,
                        unaligned,
                        src_local,
                    }
                });
            }
            Inst::Call { call_id } => {
                let call = *u.call(call_id);
                if let IrCallee::Indirect(_, callee) = call.callee {
                    reject_value(self, callee);
                }
                for arg in u.args(call.args) {
                    reject_value(self, *arg);
                }
                if let Some(dst) = call.dst {
                    let address = get_address(self, dst);
                    self.add_access(address, id, block, || {
                        let layout = k1.get_pt_layout(call.ret_type);
                        LocalAccessKind::OverwriteFromMemory {
                            src_addr: dst,
                            size: layout.size,
                            align: layout.align,
                            unaligned: false,
                            src_local: None,
                        }
                    });
                    if let Some(LocalAddress::ConstOffset(alloca, _)) = address
                        && self.alloca_promotions.get(alloca) == Some(AllocaPromotion::RemoveAlloca)
                    {
                        self.alloca_promotions.insert(alloca, AllocaPromotion::KeepAlloca);
                    }
                }
            }
            Inst::StructOffset { base, vm_offset, .. } => {
                let address =
                    add_address_offset(get_address(self, base), || Some(vm_offset as u64));
                self.add_offset_inst(id, address);
            }
            Inst::ArrayOffset { base, element_t, element_index } => {
                reject_value(self, element_index);
                let address = add_address_offset(get_address(self, base), || {
                    get_const_array_offset(k1, &u.view(), &u.subst, element_t, element_index)
                });
                self.add_offset_inst(id, address);
            }
            Inst::Ret { v, .. }
            | Inst::BitCast { v, .. }
            | Inst::PtrToWord { v, .. }
            | Inst::WordToPtr { v }
            | Inst::BakeStaticValue { value: v, .. }
            | Inst::AtomicLoad { src: v, .. } => reject_value(self, v),
            Inst::IntCmp { lhs, rhs, .. }
            | Inst::AtomicStore { dst: lhs, value: rhs, .. }
            | Inst::AtomicRmw { dst: lhs, operand: rhs, .. }
            | Inst::Select { cons: lhs, alt: rhs, .. } => {
                reject_value(self, lhs);
                reject_value(self, rhs);
            }
            Inst::Alloca { .. }
            | Inst::Unreachable
            | Inst::Fence { .. }
            | Inst::ReloadGlobalAddr { .. }
            | Inst::Data(_)
            | Inst::Jump(_)
            | Inst::JumpIf { .. }
            | Inst::Switch { .. }
            | Inst::BoolNegate { .. }
            | Inst::BitNot { .. }
            | Inst::FloatNeg { .. }
            | Inst::IntTrunc { .. }
            | Inst::IntExtU { .. }
            | Inst::IntExtS { .. }
            | Inst::FloatTrunc { .. }
            | Inst::FloatExt { .. }
            | Inst::Float32ToIntUnsigned { .. }
            | Inst::Float64ToIntUnsigned { .. }
            | Inst::Float32ToIntSigned { .. }
            | Inst::Float64ToIntSigned { .. }
            | Inst::IntToFloatUnsigned { .. }
            | Inst::IntToFloatSigned { .. }
            | Inst::IntAdd { .. }
            | Inst::IntSub { .. }
            | Inst::IntMul { .. }
            | Inst::IntDivUnsigned { .. }
            | Inst::IntDivSigned { .. }
            | Inst::IntRemUnsigned { .. }
            | Inst::IntRemSigned { .. }
            | Inst::FloatAdd { .. }
            | Inst::FloatSub { .. }
            | Inst::FloatMul { .. }
            | Inst::FloatDiv { .. }
            | Inst::FloatRem { .. }
            | Inst::FloatCmp { .. }
            | Inst::BitAnd { .. }
            | Inst::BitOr { .. }
            | Inst::BitXor { .. }
            | Inst::BitShiftLeft { .. }
            | Inst::BitUnsignedShiftRight { .. }
            | Inst::BitSignedShiftRight { .. } => {}
            _ => visit_inst_values(&u.view(), inst, &mut |v| reject_value(self, v)),
        }
    }
}

impl PromoteState {
    fn sort_and_index_slots(&mut self) {
        self.slots.sort_by_key(|s| s.sort_key());
        self.slots.dedup();
        self.alloca_slot_ranges.clear();
        let mut start = 0;
        while start < self.slots.len() {
            let alloca = self.slots[start].alloca;
            let mut end = start + 1;
            while end < self.slots.len() && self.slots[end].alloca == alloca {
                end += 1;
            }
            self.alloca_slot_ranges.insert(alloca, (start as u32, end as u32));
            start = end;
        }
    }

    fn is_any_slot_partially_within(&self, alloca: InstId, offset: u32, size: u32) -> bool {
        let Some((start, end)) = self.alloca_slot_ranges.get(alloca) else { return false };
        self.slots[start as usize..end as usize]
            .iter()
            .any(|s| !s.is_within(offset, size) && s.offset < offset + size && s.end() > offset)
    }

    fn get_slot_index(&self, alloca: InstId, offset: u32, t: ScalarType) -> u32 {
        let (start, end) = self.alloca_slot_ranges.get(alloca).unwrap();
        let slots = &self.slots[start as usize..end as usize];
        let at = slots.binary_search_by_key(&(offset, t.to_tag()), |s| (s.offset, s.t.to_tag()));
        start + at.expect("access without a slot") as u32
    }

    fn get_slot_range_within(&self, alloca: InstId, offset: u32, size: u32) -> (u32, u32) {
        let (start, end) = self.alloca_slot_ranges.get(alloca).unwrap_or((0, 0));
        let mut first = start;
        while first < end && self.slots[first as usize].offset < offset {
            first += 1;
        }
        let mut last = first;
        while last < end && self.slots[last as usize].is_within(offset, size) {
            last += 1;
        }
        (first, last)
    }

    fn set_current_value(&mut self, slot: u32, value: Value) {
        self.current_values[slot as usize] = (self.current_block_stamp, value);
    }

    fn resolve_current_value(&mut self, u: &mut UnitBuf, slot: u32, block: BlockId) -> Value {
        let (stamp, value) = self.current_values[slot as usize];
        if stamp == self.current_block_stamp {
            return value;
        }
        let value = self.resolve_entry_value(u, slot, block);
        self.set_current_value(slot, value);
        value
    }

    fn resolve_entry_value(&mut self, u: &mut UnitBuf, slot: u32, block: BlockId) -> Value {
        self.walked_blocks.clear();
        let mut b = block;
        let value = loop {
            self.walked_blocks.push(b);
            self.unique_preds.clear();
            for pred in u.preds(b) {
                if !self.unique_preds.contains(&pred) {
                    self.unique_preds.push(pred);
                }
            }
            match self.unique_preds.len() {
                0 => break Value::zero(self.slots[slot as usize].t),
                1 => {
                    let pred = self.unique_preds[0];
                    if let Some(v) = self.block_exit_values.get(&make_block_exit_key(slot, pred)) {
                        break *v;
                    }
                    b = pred;
                }
                _ => {
                    let t = self.slots[slot as usize].t;
                    let inst = u.new_inst(
                        Inst::Phi { t: PhysicalType::scalar(t), incomings: IrRange::EMPTY },
                        Span::NONE,
                        IrComment::PromotedPhi,
                    );
                    u.push_inst_front(b, inst);
                    let cases_start = self.phi_cases.len() as u32;
                    for pred in &self.unique_preds {
                        self.pending_phi_cases.push(PendingPhiCase {
                            case_index: self.phi_cases.len() as u32,
                            pred: *pred,
                            slot,
                        });
                        self.phi_cases.push(PhiCase { from: *pred, value: Value::Empty });
                    }
                    let cases_end = self.phi_cases.len() as u32;
                    self.phi_index_of_inst.insert(inst, self.phis.len() as u32);
                    self.phis.push(PromotedPhi {
                        inst,
                        t,
                        cases_start,
                        cases_end,
                        is_trivial: false,
                        is_live: false,
                    });
                    break Value::Inst(inst);
                }
            }
        };
        for b in &self.walked_blocks {
            self.block_exit_values.entry(make_block_exit_key(slot, *b)).or_insert(value);
        }
        value
    }
}

pub fn promote_unit(
    k1: &mut TypedProgram,
    u: &mut UnitBuf,
    unit_id: IrUnitId,
    recorded: Option<&mut LocalUses>,
) {
    let frame = k1.trace_push_unit(TraceKind::IrPromote, unit_id, None);
    let mut state = k1.ir.opt_buf_promote.take().unwrap_or_default();
    let st = &mut *state;
    let promoted: u64 = 'promote: {
        // Take the uses recorded during lowering, or rescan the unit for them
        match recorded {
            Some(recorded) => std::mem::swap(&mut st.local_uses, recorded),
            None => {
                st.local_uses.clear();
                if let Some(entry) = u.body.first_block
                    && let Some(first) = u.block(entry).first
                    && matches!(u.inst(first), Inst::Alloca { .. })
                {
                    let k1 = &*k1;
                    let view = u.view();
                    let compute_address =
                        |_: &LocalUses, v: Value| compute_local_address(k1, &view, &u.subst, v);
                    for block in view.block_ids() {
                        for id in view.block_insts(block) {
                            let inst = view.inst(id);
                            st.local_uses.record_inst_with(
                                k1,
                                u,
                                &compute_address,
                                id,
                                inst,
                                block,
                            );
                        }
                    }
                }
            }
        }

        // Build the scalar slots that loads and stores of promotable allocas touch
        let Some(entry) = u.body.first_block else { break 'promote 0 };
        if u.has_preds(entry) {
            break 'promote 0;
        }
        st.slots.clear();
        st.local_uses.reject_sources_of_rejected_copies();
        if !st.local_uses.has_promotable_alloca() {
            break 'promote 0;
        }
        for access in &st.local_uses.accesses {
            if let LocalAccessKind::Load(t) | LocalAccessKind::Store(t, _) = access.kind
                && !st.local_uses.is_rejected(access.alloca)
            {
                st.slots.push(ScalarSlot { alloca: access.alloca, offset: access.offset, t });
            }
        }
        st.sort_and_index_slots();

        // Mirror slots into copy sources: a slot inside a copy's destination range needs its counterpart in the source
        let slot_limit = MAX_SLOTS_PER_ALLOCA * st.local_uses.alloca_promotions.keys().len();
        loop {
            st.new_slots.clear();
            for copy in &st.local_uses.local_copies {
                if st.local_uses.is_rejected(copy.dst_alloca)
                    || st.local_uses.is_rejected(copy.src_alloca)
                {
                    continue;
                }
                let (dst_start, dst_end) =
                    st.alloca_slot_ranges.get(copy.dst_alloca).unwrap_or((0, 0));
                let (src_start, src_end) =
                    st.alloca_slot_ranges.get(copy.src_alloca).unwrap_or((0, 0));
                let src_slots = &st.slots[src_start as usize..src_end as usize];
                for dst_slot in &st.slots[dst_start as usize..dst_end as usize] {
                    if !dst_slot.is_within(copy.dst_offset, copy.size) {
                        continue;
                    }
                    let offset = copy.src_offset + dst_slot.offset - copy.dst_offset;
                    let src_slot = ScalarSlot { alloca: copy.src_alloca, offset, t: dst_slot.t };
                    if src_slots
                        .binary_search_by_key(&src_slot.sort_key(), |s| s.sort_key())
                        .is_err()
                    {
                        st.new_slots.push(src_slot);
                    }
                }
            }
            if st.new_slots.is_empty() {
                break;
            }
            st.slots.extend_from_slice(&st.new_slots);
            st.sort_and_index_slots();
            if st.slots.len() > slot_limit {
                for i in 0..st.local_uses.local_copies.len() {
                    let copy = st.local_uses.local_copies[i];
                    st.local_uses.reject_copy_endpoints(copy);
                }
                break;
            }
        }

        // Reject allocas whose slots overlap or overflow, or are split by a zero, overwrite or copy
        for alloca in st.alloca_slot_ranges.keys() {
            let (start, end) = st.alloca_slot_ranges.get(*alloca).unwrap();
            let Inst::Alloca { vm_layout, .. } = *u.inst(*alloca) else { unreachable!() };
            let slots = &st.slots[start as usize..end as usize];
            let mut conflicts =
                slots.len() > MAX_SLOTS_PER_ALLOCA || slots[slots.len() - 1].end() > vm_layout.size;
            for pair in slots.windows(2) {
                conflicts |= pair[0].end() > pair[1].offset;
            }
            if conflicts {
                st.local_uses.alloca_promotions.insert(*alloca, AllocaPromotion::Rejected);
            }
        }
        for i in 0..st.local_uses.accesses.len() {
            let LocalAccess { alloca, offset, kind, .. } = st.local_uses.accesses[i];
            let size = match kind {
                LocalAccessKind::Zero { size }
                | LocalAccessKind::OverwriteFromMemory { size, .. } => size,
                LocalAccessKind::Load(_) | LocalAccessKind::Store(..) => continue,
            };
            if st.is_any_slot_partially_within(alloca, offset, size) {
                st.local_uses.alloca_promotions.insert(alloca, AllocaPromotion::Rejected);
            }
        }
        for i in 0..st.local_uses.local_copies.len() {
            let copy = st.local_uses.local_copies[i];
            if st.is_any_slot_partially_within(copy.src_alloca, copy.src_offset, copy.size)
                || st.is_any_slot_partially_within(copy.dst_alloca, copy.dst_offset, copy.size)
            {
                st.local_uses.reject_copy_endpoints(copy);
            }
        }
        st.local_uses.reject_sources_of_rejected_copies();

        // Drop accesses of rejected allocas and sort the rest by block
        let local_uses = &mut st.local_uses;
        let mut accesses = std::mem::take(&mut local_uses.accesses);
        accesses.retain_mut(|access| {
            if let LocalAccessKind::OverwriteFromMemory { src_local, .. } = &mut access.kind
                && src_local.is_some_and(|(src_alloca, _)| local_uses.is_rejected(src_alloca))
            {
                *src_local = None;
            }
            !local_uses.is_rejected(access.alloca)
        });
        if !accesses.is_sorted_by_key(|a| a.block.as_u32()) {
            accesses.sort_by_key(|a| a.block.as_u32());
        }
        local_uses.accesses = accesses;
        if !local_uses.has_promotable_alloca() {
            break 'promote 0;
        }

        // Reset the promotion buffers
        st.placeholders.clear();
        st.pending_load_index.clear();
        st.pending_loads.clear();
        let need = st.local_uses.accesses.len();
        if st.block_exit_values.capacity() > 16 * need.max(64) {
            st.block_exit_values = FxHashMap::with_capacity_and_hasher(need, Default::default());
        } else {
            if !st.block_exit_values.is_empty() {
                st.block_exit_values.clear();
            }
            st.block_exit_values.reserve(need);
        }
        st.current_values.clear();
        st.current_values.resize(st.slots.len(), (0, Value::Empty));
        st.current_block_stamp = 0;
        st.phis.clear();
        st.phi_cases.clear();
        st.phi_index_of_inst.clear();
        st.pending_phi_cases.clear();

        // Seed each block's exit value per slot; an overwrite gets one placeholder per slot
        for i in 0..st.local_uses.accesses.len() {
            let access = st.local_uses.accesses[i];
            let block = access.block;
            match access.kind {
                LocalAccessKind::Load(_) => {}
                LocalAccessKind::Store(t, value) => {
                    let slot = st.get_slot_index(access.alloca, access.offset, t);
                    st.block_exit_values.insert(make_block_exit_key(slot, block), value);
                }
                LocalAccessKind::Zero { size } => {
                    let (first, last) =
                        st.get_slot_range_within(access.alloca, access.offset, size);
                    for slot in first..last {
                        let zero = Value::zero(st.slots[slot as usize].t);
                        st.block_exit_values.insert(make_block_exit_key(slot, block), zero);
                    }
                }
                LocalAccessKind::OverwriteFromMemory {
                    src_addr,
                    size,
                    align,
                    unaligned,
                    src_local,
                } => {
                    st.local_uses.accesses[i].first_placeholder = st.placeholders.len() as u32;
                    let (first, last) =
                        st.get_slot_range_within(access.alloca, access.offset, size);
                    for slot in first..last {
                        let scalar_slot = st.slots[slot as usize];
                        let placeholder =
                            u.new_inst(Inst::Unreachable, Span::NONE, IrComment::None);
                        if src_local.is_none() {
                            let offset = scalar_slot.offset - access.offset;
                            let t_align = scalar_slot.t.get_layout().align;
                            st.pending_load_index
                                .insert(placeholder, st.pending_loads.len() as u32);
                            st.pending_loads.push(PendingLoad {
                                placeholder,
                                insert_after: access.inst,
                                base_addr: src_addr,
                                offset,
                                t: scalar_slot.t,
                                unaligned: unaligned || align < t_align || offset % t_align != 0,
                                is_used: false,
                            });
                        }
                        st.placeholders.push(placeholder);
                        st.block_exit_values
                            .insert(make_block_exit_key(slot, block), Value::Inst(placeholder));
                    }
                }
            }
        }

        // Rewrite accesses in block order: a load becomes its slot's current value
        let mut current_block = None;
        for i in 0..st.local_uses.accesses.len() {
            let access = st.local_uses.accesses[i];
            if current_block != Some(access.block) {
                st.current_block_stamp += 1;
                current_block = Some(access.block);
            }
            match access.kind {
                LocalAccessKind::Load(t) => {
                    let slot = st.get_slot_index(access.alloca, access.offset, t);
                    let value = st.resolve_current_value(u, slot, access.block);
                    u.subst.insert(access.inst, value);
                }
                LocalAccessKind::Store(t, value) => {
                    let slot = st.get_slot_index(access.alloca, access.offset, t);
                    st.set_current_value(slot, value);
                }
                LocalAccessKind::Zero { size } => {
                    let (first, last) =
                        st.get_slot_range_within(access.alloca, access.offset, size);
                    for slot in first..last {
                        st.set_current_value(slot, Value::zero(st.slots[slot as usize].t));
                    }
                }
                LocalAccessKind::OverwriteFromMemory { size, src_local, .. } => {
                    let (first, last) =
                        st.get_slot_range_within(access.alloca, access.offset, size);
                    let first_placeholder = access.first_placeholder as usize;
                    if let Some((src_alloca, src_offset)) = src_local {
                        for (i, slot) in (first..last).enumerate() {
                            let scalar_slot = st.slots[slot as usize];
                            let src_slot = st.get_slot_index(
                                src_alloca,
                                src_offset + scalar_slot.offset - access.offset,
                                scalar_slot.t,
                            );
                            let value = st.resolve_current_value(u, src_slot, access.block);
                            u.subst.insert(st.placeholders[first_placeholder + i], value);
                        }
                    }
                    for (i, slot) in (first..last).enumerate() {
                        let placeholder = st.placeholders[first_placeholder + i];
                        st.set_current_value(slot, Value::Inst(placeholder));
                    }
                }
            }
        }

        // Fill phi cases from each predecessor's exit value
        while let Some(PendingPhiCase { case_index, pred, slot }) = st.pending_phi_cases.pop() {
            let value = match st.block_exit_values.get(&make_block_exit_key(slot, pred)) {
                Some(v) => *v,
                None => st.resolve_entry_value(u, slot, pred),
            };
            st.phi_cases[case_index as usize].value = value;
        }

        // Substitute trivial phis, whose cases are all one value or the phi itself
        let phi_count = st.phis.len();
        if st.phi_users.len() < phi_count {
            st.phi_users.resize_with(phi_count, Vec::new);
        }
        for users in &mut st.phi_users[..phi_count] {
            users.clear();
        }
        for (p, phi) in st.phis.iter().enumerate() {
            for case in &st.phi_cases[phi.cases_start as usize..phi.cases_end as usize] {
                if let Value::Inst(id) = u.subst.resolve(case.value)
                    && let Some(q) = st.phi_index_of_inst.get(id)
                    && q as usize != p
                {
                    st.phi_users[q as usize].push(p as u32);
                }
            }
        }
        st.phi_worklist.clear();
        for p in (0..phi_count as u32).rev() {
            st.phi_worklist.push(p);
        }
        while let Some(p) = st.phi_worklist.pop() {
            let p = p as usize;
            let phi = st.phis[p];
            if phi.is_trivial {
                continue;
            }
            let mut same: Option<Value> = None;
            let mut trivial = true;
            for case in &st.phi_cases[phi.cases_start as usize..phi.cases_end as usize] {
                let v = u.subst.resolve(case.value);
                if v == Value::Inst(phi.inst) || same == Some(v) {
                    continue;
                }
                if same.is_some() {
                    trivial = false;
                    break;
                }
                same = Some(v);
            }
            if !trivial {
                continue;
            }
            let value = same.unwrap_or(Value::zero(phi.t));
            u.subst.insert(phi.inst, value);
            st.phis[p].is_trivial = true;
            let users = std::mem::take(&mut st.phi_users[p]);
            st.phi_worklist.extend_from_slice(&users);
            if let Value::Inst(id) = value
                && let Some(q) = st.phi_index_of_inst.get(id)
            {
                st.phi_users[q as usize].extend_from_slice(&users);
            }
            st.phi_users[p] = users;
        }

        // Mark the phis and pending loads that a rewritten load reaches
        if !st.phis.is_empty() || !st.pending_loads.is_empty() {
            st.live_worklist.clear();
            for access in &st.local_uses.accesses {
                if let LocalAccessKind::Load(_) = access.kind {
                    st.live_worklist.push(Value::Inst(access.inst));
                }
            }
            while let Some(v) = st.live_worklist.pop() {
                let Value::Inst(id) = u.subst.resolve(v) else { continue };
                if let Some(p) = st.phi_index_of_inst.get(id) {
                    let phi = &mut st.phis[p as usize];
                    if phi.is_trivial || phi.is_live {
                        continue;
                    }
                    phi.is_live = true;
                    for i in phi.cases_start..phi.cases_end {
                        st.live_worklist.push(st.phi_cases[i as usize].value);
                    }
                } else if let Some(load) = st.pending_load_index.get(id) {
                    st.pending_loads[load as usize].is_used = true;
                }
            }
        }

        // Emit live phis and remove the rest
        for phi in &st.phis {
            if phi.is_live {
                let cases = &st.phi_cases[phi.cases_start as usize..phi.cases_end as usize];
                let incomings = u.push_phi_cases(cases);
                *u.inst_mut(phi.inst) = Inst::Phi { t: PhysicalType::scalar(phi.t), incomings };
            } else {
                u.remove_inst(phi.inst);
            }
        }
        // Insert the memory loads that used placeholders stand for
        for load in &st.pending_loads {
            if !load.is_used {
                continue;
            }
            let span = u.view().span(load.insert_after);
            let mut after = load.insert_after;
            let addr = if load.offset == 0 {
                load.base_addr
            } else {
                let offset_inst = Inst::ArrayOffset {
                    element_t: PhysicalType::scalar(ScalarType::I8),
                    base: load.base_addr,
                    element_index: Value::Data32 { t: ScalarType::I64, data: load.offset },
                };
                let id = u.new_inst(offset_inst, span, IrComment::PromotedCopy);
                u.insert_inst_after(after, id);
                after = id;
                Value::Inst(id)
            };
            let load_inst = Inst::Load {
                t: PhysicalType::scalar(load.t),
                src: addr,
                volatile: false,
                unaligned: load.unaligned,
            };
            let id = u.new_inst(load_inst, span, IrComment::PromotedCopy);
            u.insert_inst_after(after, id);
            u.subst.insert(load.placeholder, Value::Inst(id));
        }

        // Remove the promoted accesses, offset insts and allocas
        for access in &st.local_uses.accesses {
            if !matches!(u.inst(access.inst), Inst::Call { .. }) {
                u.remove_inst(access.inst);
            }
        }
        for (offset_inst, alloca) in &st.local_uses.offset_insts {
            if st.local_uses.alloca_promotions.get(*alloca) == Some(AllocaPromotion::RemoveAlloca) {
                u.remove_inst(*offset_inst);
            }
        }
        let mut promoted = 0;
        for alloca in st.local_uses.alloca_promotions.keys() {
            match st.local_uses.alloca_promotions.get(*alloca) {
                Some(AllocaPromotion::RemoveAlloca) => u.remove_inst(*alloca),
                Some(AllocaPromotion::KeepAlloca) => {}
                _ => continue,
            }
            promoted += 1;
        }
        promoted
    };
    k1.ir.opt_buf_promote = Some(state);
    k1.trace.set_top_count(promoted);
    k1.trace_pop(frame);
}
