use super::*;
use std::marker::PhantomData;
use std::num::NonZeroU32;

nz_u32_id!(BlockId);

#[inline]
fn ix(id: impl Into<NonZeroU32>) -> usize {
    id.into().get() as usize - 1
}

#[inline]
fn id_at<I: From<NonZeroU32>>(index: usize) -> I {
    I::from(NonZeroU32::new(index as u32 + 1).unwrap())
}

pub struct IrRange<T> {
    start: u32,
    len: u32,
    _t: PhantomData<T>,
}

impl<T> Clone for IrRange<T> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<T> Copy for IrRange<T> {}

impl<T> IrRange<T> {
    pub const EMPTY: IrRange<T> = IrRange { start: 0, len: 0, _t: PhantomData };

    fn make(start: usize, len: usize) -> Self {
        IrRange { start: start as u32, len: len as u32, _t: PhantomData }
    }

    pub fn len(&self) -> u32 {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    fn of<'a>(&self, items: &'a [T]) -> &'a [T] {
        &items[self.start as usize..(self.start + self.len) as usize]
    }

    fn of_mut<'a>(&self, items: &'a mut [T]) -> &'a mut [T] {
        &mut items[self.start as usize..(self.start + self.len) as usize]
    }
}

#[derive(Clone, Copy)]
pub struct InstLayout {
    pub block: Option<BlockId>,
    pub prev: Option<InstId>,
    pub next: Option<InstId>,
}

#[derive(Clone, Copy)]
pub struct BlockData {
    pub kind: BlockSourceKind,
    pub first: Option<InstId>,
    pub last: Option<InstId>,
    pub prev: Option<BlockId>,
    pub next: Option<BlockId>,
}

static_assert_size!(InstLayout, 12);

#[derive(Clone, Copy, Default)]
pub struct UnitBody {
    pub first_block: Option<BlockId>,
    pub last_block: Option<BlockId>,
    pub last_alloca: Option<InstId>,
}

#[derive(Clone, Copy)]
pub struct IrUnit {
    pub result_type_id: TypeId,
    pub unit_id: IrUnitId,
    pub fn_type: PhysicalFunctionType,
    pub function_builtin_kind: Option<BackendBuiltin>,
    pub is_debug: bool,
    pub is_optimized: bool,
    pub body: UnitBody,
    pub insts: MSlice<Inst, ProgramIr>,
    pub inst_src: MSlice<SpanId, ProgramIr>,
    pub inst_comment: MSlice<IrComment, ProgramIr>,
    pub blocks: MSlice<BlockData, ProgramIr>,
    pub calls: MSlice<IrCall, ProgramIr>,
    pub call_args: MSlice<Value, ProgramIr>,
    pub phi_cases: MSlice<PhiCase, ProgramIr>,
    pub switch_cases: MSlice<SwitchCase, ProgramIr>,
    pub cmpxchgs: MSlice<AtomicCmpxchgData, ProgramIr>,
    pub vec_ops: MSlice<VecOpData, ProgramIr>,
}

impl IrUnit {
    pub fn new(
        result_type_id: TypeId,
        unit_id: IrUnitId,
        fn_type: PhysicalFunctionType,
        function_builtin_kind: Option<BackendBuiltin>,
        is_debug: bool,
    ) -> IrUnit {
        IrUnit {
            result_type_id,
            unit_id,
            fn_type,
            function_builtin_kind,
            is_debug,
            is_optimized: false,
            body: UnitBody::default(),
            insts: MSlice::empty(),
            inst_src: MSlice::empty(),
            inst_comment: MSlice::empty(),
            blocks: MSlice::empty(),
            calls: MSlice::empty(),
            call_args: MSlice::empty(),
            phi_cases: MSlice::empty(),
            switch_cases: MSlice::empty(),
            cmpxchgs: MSlice::empty(),
            vec_ops: MSlice::empty(),
        }
    }

    pub fn is_cold(&self, k1: &TypedProgram) -> bool {
        self.fn_type.diverges
            || matches!(self.unit_id, IrUnitId::Function(id) if k1.get_function(id).is_cold())
    }

    pub fn view(&self, mem: &kmem::Mem<ProgramIr>) -> UnitView<'static> {
        UnitView {
            body: self.body,
            insts: mem.getn(self.insts),
            inst_src: mem.getn(self.inst_src),
            inst_comment: mem.getn(self.inst_comment),
            inst_layout: &[],
            blocks: mem.getn(self.blocks),
            calls: mem.getn(self.calls),
            call_args: mem.getn(self.call_args),
            phi_cases: mem.getn(self.phi_cases),
            switch_cases: mem.getn(self.switch_cases),
            cmpxchgs: mem.getn(self.cmpxchgs),
            vec_ops: mem.getn(self.vec_ops),
        }
    }

    pub fn inst_count(&self) -> u32 {
        self.insts.len()
    }
}

#[derive(Clone, Copy)]
pub struct UnitView<'a> {
    body: UnitBody,
    insts: &'a [Inst],
    inst_src: &'a [SpanId],
    inst_comment: &'a [IrComment],
    inst_layout: &'a [InstLayout],
    blocks: &'a [BlockData],
    calls: &'a [IrCall],
    call_args: &'a [Value],
    phi_cases: &'a [PhiCase],
    switch_cases: &'a [SwitchCase],
    cmpxchgs: &'a [AtomicCmpxchgData],
    vec_ops: &'a [VecOpData],
}

impl<'a> UnitView<'a> {
    pub const EMPTY: UnitView<'static> = UnitView {
        body: UnitBody { first_block: None, last_block: None, last_alloca: None },
        insts: &[],
        inst_src: &[],
        inst_comment: &[],
        inst_layout: &[],
        blocks: &[],
        calls: &[],
        call_args: &[],
        phi_cases: &[],
        switch_cases: &[],
        cmpxchgs: &[],
        vec_ops: &[],
    };

    #[inline]
    pub fn inst(&self, id: InstId) -> &'a Inst {
        &self.insts[ix(id)]
    }

    #[inline]
    pub fn span(&self, id: InstId) -> SpanId {
        self.inst_src[ix(id)]
    }

    #[inline]
    pub fn comment(&self, id: InstId) -> IrComment {
        self.inst_comment[ix(id)]
    }

    #[inline]
    pub fn block(&self, b: BlockId) -> &'a BlockData {
        &self.blocks[ix(b)]
    }

    #[inline]
    pub fn call(&self, id: IrCallId) -> &'a IrCall {
        &self.calls[ix(id)]
    }

    #[inline]
    pub fn args(&self, args: IrRange<Value>) -> &'a [Value] {
        args.of(self.call_args)
    }

    #[inline]
    pub fn phi_cases(&self, cases: IrRange<PhiCase>) -> &'a [PhiCase] {
        cases.of(self.phi_cases)
    }

    #[inline]
    pub fn switch_cases(&self, cases: IrRange<SwitchCase>) -> &'a [SwitchCase] {
        cases.of(self.switch_cases)
    }

    #[inline]
    pub fn cmpxchg(&self, id: AtomicCmpxchgId) -> &'a AtomicCmpxchgData {
        &self.cmpxchgs[ix(id)]
    }

    #[inline]
    pub fn vec_op(&self, id: VecOpId) -> &'a VecOpData {
        &self.vec_ops[ix(id)]
    }

    pub fn first_block(&self) -> Option<BlockId> {
        self.body.first_block
    }

    pub fn inst_count(&self) -> usize {
        self.insts.len()
    }

    pub fn block_count(&self) -> usize {
        self.blocks.len()
    }

    pub fn block_insts(&self, b: BlockId) -> InstIter<'a> {
        let block = self.block(b);
        InstIter { layout: self.inst_layout, next: block.first, last: block.last }
    }

    pub fn block_ids(&self) -> BlockIter<'a> {
        BlockIter { blocks: self.blocks, next: self.body.first_block }
    }
}

pub struct InstIter<'a> {
    layout: &'a [InstLayout],
    next: Option<InstId>,
    last: Option<InstId>,
}

impl Iterator for InstIter<'_> {
    type Item = InstId;
    #[inline]
    fn next(&mut self) -> Option<InstId> {
        let id = self.next?;
        self.next = match self.layout {
            [] if Some(id) == self.last => None,
            [] => Some(id + 1),
            layout => layout[ix(id)].next,
        };
        Some(id)
    }
}

pub struct BlockIter<'a> {
    blocks: &'a [BlockData],
    next: Option<BlockId>,
}

impl Iterator for BlockIter<'_> {
    type Item = BlockId;
    #[inline]
    fn next(&mut self) -> Option<BlockId> {
        let id = self.next?;
        self.next = self.blocks[ix(id)].next;
        Some(id)
    }
}

#[derive(Default)]
pub struct UnitBuf {
    pub body: UnitBody,
    insts: Vec<Inst>,
    inst_src: Vec<SpanId>,
    inst_comment: Vec<IrComment>,
    inst_layout: Vec<InstLayout>,
    blocks: Vec<BlockData>,
    calls: Vec<IrCall>,
    call_args: Vec<Value>,
    phi_cases: Vec<PhiCase>,
    switch_cases: Vec<SwitchCase>,
    cmpxchgs: Vec<AtomicCmpxchgData>,
    vec_ops: Vec<VecOpData>,

    pub cfg_valid: bool,
    preds: Vec<BlockId>,
    pred_ranges: Vec<(u32, u32)>,
    cfg_edges: Vec<(BlockId, BlockId)>,
    cfg_seen: Vec<bool>,
    cfg_stack: Vec<BlockId>,
}

impl UnitBuf {
    pub fn view(&self) -> UnitView<'_> {
        UnitView {
            body: self.body,
            insts: &self.insts,
            inst_src: &self.inst_src,
            inst_comment: &self.inst_comment,
            inst_layout: &self.inst_layout,
            blocks: &self.blocks,
            calls: &self.calls,
            call_args: &self.call_args,
            phi_cases: &self.phi_cases,
            switch_cases: &self.switch_cases,
            cmpxchgs: &self.cmpxchgs,
            vec_ops: &self.vec_ops,
        }
    }

    fn clear(&mut self) {
        self.body = UnitBody::default();
        self.insts.clear();
        self.inst_src.clear();
        self.inst_comment.clear();
        self.inst_layout.clear();
        self.blocks.clear();
        self.calls.clear();
        self.call_args.clear();
        self.phi_cases.clear();
        self.switch_cases.clear();
        self.cmpxchgs.clear();
        self.vec_ops.clear();
        self.cfg_valid = false;
        self.preds.clear();
        self.pred_ranges.clear();
    }

    pub fn load(&mut self, mem: &kmem::Mem<ProgramIr>, unit: &IrUnit) {
        self.clear();
        let v = unit.view(mem);
        self.body = v.body;
        self.insts.extend_from_slice(v.insts);
        self.inst_src.extend_from_slice(v.inst_src);
        self.inst_comment.extend_from_slice(v.inst_comment);
        self.blocks.extend_from_slice(v.blocks);
        self.calls.extend_from_slice(v.calls);
        self.call_args.extend_from_slice(v.call_args);
        self.phi_cases.extend_from_slice(v.phi_cases);
        self.switch_cases.extend_from_slice(v.switch_cases);
        self.cmpxchgs.extend_from_slice(v.cmpxchgs);
        self.vec_ops.extend_from_slice(v.vec_ops);
        self.inst_layout
            .resize(self.insts.len(), InstLayout { block: None, prev: None, next: None });
        for b in v.block_ids() {
            let BlockData { first, last, .. } = *v.block(b);
            let (Some(first), Some(last)) = (first, last) else { continue };
            for i in ix(first)..=ix(last) {
                self.inst_layout[i] = InstLayout {
                    block: Some(b),
                    prev: if i > ix(first) { Some(id_at(i - 1)) } else { None },
                    next: if i < ix(last) { Some(id_at(i + 1)) } else { None },
                };
            }
        }
    }

    #[inline]
    pub fn inst(&self, id: InstId) -> &Inst {
        &self.insts[ix(id)]
    }

    #[inline]
    pub fn inst_mut(&mut self, id: InstId) -> &mut Inst {
        &mut self.insts[ix(id)]
    }

    #[inline]
    pub fn block(&self, b: BlockId) -> &BlockData {
        &self.blocks[ix(b)]
    }

    #[inline]
    pub fn next_inst(&self, id: InstId) -> Option<InstId> {
        self.inst_layout[ix(id)].next
    }

    pub fn inst_count(&self) -> usize {
        self.insts.len()
    }

    pub fn new_inst(&mut self, inst: Inst, span: SpanId, comment: IrComment) -> InstId {
        let id = id_at(self.insts.len());
        self.insts.push(inst);
        self.inst_src.push(span);
        self.inst_comment.push(comment);
        self.inst_layout.push(InstLayout { block: None, prev: None, next: None });
        id
    }

    pub fn add_call(&mut self, call: IrCall) -> IrCallId {
        self.calls.push(call);
        id_at(self.calls.len() - 1)
    }

    pub fn call_mut(&mut self, id: IrCallId) -> &mut IrCall {
        &mut self.calls[ix(id)]
    }

    pub fn push_args(&mut self, args: &[Value]) -> IrRange<Value> {
        let start = self.call_args.len();
        self.call_args.extend_from_slice(args);
        IrRange::make(start, args.len())
    }

    pub fn push_phi_cases(&mut self, cases: &[PhiCase]) -> IrRange<PhiCase> {
        let start = self.phi_cases.len();
        self.phi_cases.extend_from_slice(cases);
        IrRange::make(start, cases.len())
    }

    pub fn phi_cases_mut(&mut self, cases: IrRange<PhiCase>) -> &mut [PhiCase] {
        cases.of_mut(&mut self.phi_cases)
    }

    pub fn push_switch_cases(&mut self, cases: &[SwitchCase]) -> IrRange<SwitchCase> {
        let start = self.switch_cases.len();
        self.switch_cases.extend_from_slice(cases);
        IrRange::make(start, cases.len())
    }

    pub fn switch_cases_mut(&mut self, cases: IrRange<SwitchCase>) -> &mut [SwitchCase] {
        cases.of_mut(&mut self.switch_cases)
    }

    pub fn add_cmpxchg(&mut self, cas: AtomicCmpxchgData) -> AtomicCmpxchgId {
        self.cmpxchgs.push(cas);
        id_at(self.cmpxchgs.len() - 1)
    }

    pub fn add_vec_op(&mut self, vop: VecOpData) -> VecOpId {
        self.vec_ops.push(vop);
        id_at(self.vec_ops.len() - 1)
    }

    pub fn retain_phi_cases(&mut self, phi: InstId, mut keep: impl FnMut(&PhiCase) -> bool) {
        let Inst::Phi { incomings, .. } = &mut self.insts[ix(phi)] else { panic!("not a phi") };
        let cases = incomings.of_mut(&mut self.phi_cases);
        let mut kept = 0;
        for i in 0..cases.len() {
            if keep(&cases[i]) {
                cases[kept] = cases[i];
                kept += 1;
            }
        }
        incomings.len = kept as u32;
    }

    fn link_block(&mut self, b: BlockId, prev: Option<BlockId>, next: Option<BlockId>) {
        let data = &mut self.blocks[ix(b)];
        data.prev = prev;
        data.next = next;
        match prev {
            None => self.body.first_block = Some(b),
            Some(p) => self.blocks[ix(p)].next = Some(b),
        }
        match next {
            None => self.body.last_block = Some(b),
            Some(n) => self.blocks[ix(n)].prev = Some(b),
        }
    }

    fn new_block(&mut self, kind: BlockSourceKind) -> BlockId {
        let b = id_at(self.blocks.len());
        self.blocks.push(BlockData { kind, first: None, last: None, prev: None, next: None });
        b
    }

    pub fn add_block(&mut self, kind: BlockSourceKind) -> BlockId {
        let b = self.new_block(kind);
        self.link_block(b, self.body.last_block, None);
        b
    }

    pub fn insert_block_after(&mut self, after: BlockId, kind: BlockSourceKind) -> BlockId {
        let b = self.new_block(kind);
        let next = self.blocks[ix(after)].next;
        self.link_block(b, Some(after), next);
        b
    }

    pub fn remove_block(&mut self, b: BlockId) {
        let BlockData { prev, next, .. } = self.blocks[ix(b)];
        match prev {
            None => self.body.first_block = next,
            Some(p) => self.blocks[ix(p)].next = next,
        }
        match next {
            None => self.body.last_block = prev,
            Some(n) => self.blocks[ix(n)].prev = prev,
        }
        let data = &mut self.blocks[ix(b)];
        data.prev = None;
        data.next = None;
    }

    fn link_inst(&mut self, b: BlockId, id: InstId, prev: Option<InstId>, next: Option<InstId>) {
        self.inst_layout[ix(id)] = InstLayout { block: Some(b), prev, next };
        match prev {
            None => self.blocks[ix(b)].first = Some(id),
            Some(p) => self.inst_layout[ix(p)].next = Some(id),
        }
        match next {
            None => self.blocks[ix(b)].last = Some(id),
            Some(n) => self.inst_layout[ix(n)].prev = Some(id),
        }
    }

    pub fn push_inst(&mut self, b: BlockId, id: InstId) {
        let last = self.blocks[ix(b)].last;
        self.link_inst(b, id, last, None);
    }

    pub fn push_inst_front(&mut self, b: BlockId, id: InstId) {
        let first = self.blocks[ix(b)].first;
        self.link_inst(b, id, None, first);
    }

    pub fn insert_inst_after(&mut self, after: InstId, id: InstId) {
        let InstLayout { block, next, .. } = self.inst_layout[ix(after)];
        self.link_inst(block.unwrap(), id, Some(after), next);
    }

    pub fn remove_inst(&mut self, id: InstId) {
        let InstLayout { block, prev, next } = self.inst_layout[ix(id)];
        let b = block.unwrap();
        match prev {
            None => self.blocks[ix(b)].first = next,
            Some(p) => self.inst_layout[ix(p)].next = next,
        }
        match next {
            None => self.blocks[ix(b)].last = prev,
            Some(n) => self.inst_layout[ix(n)].prev = prev,
        }
        self.inst_layout[ix(id)] = InstLayout { block: None, prev: None, next: None };
    }

    pub fn split_block_at(&mut self, b: BlockId, at: InstId) -> BlockId {
        let kind = self.blocks[ix(b)].kind;
        let after = self.insert_block_after(b, kind);
        let before_last = self.inst_layout[ix(at)].prev;
        let old_last = self.blocks[ix(b)].last;
        self.blocks[ix(b)].last = before_last;
        match before_last {
            None => self.blocks[ix(b)].first = None,
            Some(p) => self.inst_layout[ix(p)].next = None,
        }
        self.inst_layout[ix(at)].prev = None;
        let after_data = &mut self.blocks[ix(after)];
        after_data.first = Some(at);
        after_data.last = old_last;
        let mut cur = Some(at);
        while let Some(id) = cur {
            let layout = &mut self.inst_layout[ix(id)];
            layout.block = Some(after);
            cur = layout.next;
        }
        after
    }

    pub fn cfg_compute(&mut self) {
        let block_count = self.blocks.len();
        self.cfg_edges.clear();
        self.cfg_seen.clear();
        self.cfg_seen.resize(block_count, false);
        self.pred_ranges.clear();
        self.pred_ranges.resize(block_count, (0, 0));
        self.preds.clear();
        self.cfg_stack.clear();
        if let Some(entry) = self.body.first_block {
            self.cfg_stack.push(entry);
        }
        while let Some(b) = self.cfg_stack.pop() {
            if self.cfg_seen[ix(b)] {
                continue;
            }
            self.cfg_seen[ix(b)] = true;
            let Some(terminator) = self.blocks[ix(b)].last else { continue };
            match self.insts[ix(terminator)] {
                Inst::Jump(target) => {
                    self.cfg_edges.push((b, target));
                    self.cfg_stack.push(target);
                }
                Inst::JumpIf { cons, alt, .. } => {
                    self.cfg_edges.push((b, cons));
                    self.cfg_edges.push((b, alt));
                    self.cfg_stack.push(cons);
                    self.cfg_stack.push(alt);
                }
                Inst::Switch { cases, default, .. } => {
                    for case in cases.of(&self.switch_cases) {
                        self.cfg_edges.push((b, case.target));
                        self.cfg_stack.push(case.target);
                    }
                    self.cfg_edges.push((b, default));
                    self.cfg_stack.push(default);
                }
                _ => {}
            }
        }
        for (_, target) in &self.cfg_edges {
            self.pred_ranges[ix(*target)].1 += 1;
        }
        let mut start = 0;
        for range in &mut self.pred_ranges {
            range.0 = start;
            start += range.1;
            range.1 = 0;
        }
        self.preds.resize(start as usize, BlockId::PENDING);
        for (pred, target) in &self.cfg_edges {
            let range = &mut self.pred_ranges[ix(*target)];
            self.preds[(range.0 + range.1) as usize] = *pred;
            range.1 += 1;
        }
        self.cfg_valid = true;
    }

    pub fn preds(&self, b: BlockId) -> &[BlockId] {
        match self.pred_ranges.get(ix(b)) {
            None => &[],
            Some((start, len)) => &self.preds[*start as usize..(*start + *len) as usize],
        }
    }

    pub fn preds_mut(&mut self, b: BlockId) -> &mut [BlockId] {
        match self.pred_ranges.get(ix(b)) {
            None => &mut [],
            Some((start, len)) => &mut self.preds[*start as usize..(*start + *len) as usize],
        }
    }

    pub fn clone_payload(&mut self, from: &UnitView, inst: &mut Inst) {
        match inst {
            Inst::Call { call_id } => {
                let mut call = *from.call(*call_id);
                call.args = self.push_args(from.args(call.args));
                *call_id = self.add_call(call);
            }
            Inst::AtomicCmpxchg { id } => *id = self.add_cmpxchg(*from.cmpxchg(*id)),
            Inst::VecOp { id } => *id = self.add_vec_op(*from.vec_op(*id)),
            Inst::Phi { incomings, .. } => {
                *incomings = self.push_phi_cases(from.phi_cases(*incomings))
            }
            Inst::Switch { cases, .. } => {
                *cases = self.push_switch_cases(from.switch_cases(*cases))
            }
            _ => {}
        }
    }

    pub fn map_inst_refs(
        &mut self,
        id: InstId,
        value: &impl Fn(&mut Value),
        block: &impl Fn(&mut BlockId),
    ) {
        let mut p = PayloadsMut {
            calls: &mut self.calls,
            call_args: &mut self.call_args,
            phi_cases: &mut self.phi_cases,
            switch_cases: &mut self.switch_cases,
            cmpxchgs: &mut self.cmpxchgs,
            vec_ops: &mut self.vec_ops,
        };
        map_refs(&mut self.insts[ix(id)], &mut p, value, block);
    }
}

struct PayloadsMut<'a> {
    calls: &'a mut [IrCall],
    call_args: &'a mut [Value],
    phi_cases: &'a mut [PhiCase],
    switch_cases: &'a mut [SwitchCase],
    cmpxchgs: &'a mut [AtomicCmpxchgData],
    vec_ops: &'a mut [VecOpData],
}

fn map_refs(
    inst: &mut Inst,
    p: &mut PayloadsMut,
    value: &impl Fn(&mut Value),
    block: &impl Fn(&mut BlockId),
) {
    match inst {
        Inst::Data(_)
        | Inst::ReloadGlobalAddr { .. }
        | Inst::Alloca { .. }
        | Inst::Fence { .. }
        | Inst::Unreachable => {}
        Inst::Store { dst, value: v, .. } | Inst::AtomicStore { dst, value: v, .. } => {
            value(dst);
            value(v);
        }
        Inst::Load { src, dst, .. } => {
            value(src);
            if *dst != Value::Empty {
                value(dst);
            }
        }
        Inst::AtomicLoad { src, .. } => value(src),
        Inst::AtomicRmw { dst, operand, .. } => {
            value(dst);
            value(operand);
        }
        Inst::AtomicCmpxchg { id } => {
            let cas = &mut p.cmpxchgs[ix(*id)];
            for v in [&mut cas.dst, &mut cas.expected, &mut cas.desired, &mut cas.result] {
                value(v);
            }
        }
        Inst::VecOp { id } => {
            let vop = &mut p.vec_ops[ix(*id)];
            for v in [&mut vop.dst, &mut vop.lhs, &mut vop.rhs] {
                value(v);
            }
        }
        Inst::Copy { dst, src, .. } => {
            value(dst);
            value(src);
        }
        Inst::StructOffset { base, .. } => value(base),
        Inst::ArrayOffset { base, element_index, .. } => {
            value(base);
            value(element_index);
        }
        Inst::Call { call_id } => {
            let call = &mut p.calls[ix(*call_id)];
            for arg in call.args.of_mut(p.call_args) {
                value(arg);
            }
            if let IrCallee::Indirect(_, v) = &mut call.callee {
                value(v);
            }
            if let Some(v) = &mut call.dst {
                value(v);
            }
        }
        Inst::Jump(target) => block(target),
        Inst::JumpIf { cond, cons, alt } => {
            value(cond);
            block(cons);
            block(alt);
        }
        Inst::Switch { value: v, cases, default, .. } => {
            value(v);
            for case in cases.of_mut(p.switch_cases) {
                block(&mut case.target);
            }
            block(default);
        }
        Inst::Phi { incomings, .. } => {
            for case in incomings.of_mut(p.phi_cases) {
                value(&mut case.value);
                block(&mut case.from);
            }
        }
        Inst::Ret { v, .. }
        | Inst::BoolNegate { v }
        | Inst::BitNot { v }
        | Inst::FloatNeg { v, .. }
        | Inst::BitCast { v, .. }
        | Inst::IntTrunc { v, .. }
        | Inst::IntExtU { v, .. }
        | Inst::IntExtS { v, .. }
        | Inst::FloatTrunc { v, .. }
        | Inst::FloatExt { v, .. }
        | Inst::Float32ToIntUnsigned { v, .. }
        | Inst::Float64ToIntUnsigned { v, .. }
        | Inst::Float32ToIntSigned { v, .. }
        | Inst::Float64ToIntSigned { v, .. }
        | Inst::IntToFloatUnsigned { v, .. }
        | Inst::IntToFloatSigned { v, .. }
        | Inst::PtrToWord { v }
        | Inst::WordToPtr { v }
        | Inst::BakeStaticValue { value: v, .. } => value(v),
        Inst::IntAdd { lhs, rhs, .. }
        | Inst::IntSub { lhs, rhs, .. }
        | Inst::IntMul { lhs, rhs, .. }
        | Inst::IntDivUnsigned { lhs, rhs, .. }
        | Inst::IntDivSigned { lhs, rhs, .. }
        | Inst::IntRemUnsigned { lhs, rhs, .. }
        | Inst::IntRemSigned { lhs, rhs, .. }
        | Inst::IntCmp { lhs, rhs, .. }
        | Inst::FloatAdd { lhs, rhs, .. }
        | Inst::FloatSub { lhs, rhs, .. }
        | Inst::FloatMul { lhs, rhs, .. }
        | Inst::FloatDiv { lhs, rhs, .. }
        | Inst::FloatRem { lhs, rhs, .. }
        | Inst::FloatCmp { lhs, rhs, .. }
        | Inst::BitAnd { lhs, rhs, .. }
        | Inst::BitOr { lhs, rhs, .. }
        | Inst::BitXor { lhs, rhs, .. }
        | Inst::BitShiftLeft { lhs, rhs, .. }
        | Inst::BitUnsignedShiftRight { lhs, rhs, .. }
        | Inst::BitSignedShiftRight { lhs, rhs, .. } => {
            value(lhs);
            value(rhs);
        }
    }
}

impl ProgramIr {
    pub fn take_unit_buf(&mut self) -> UnitBuf {
        self.unit_bufs.pop().unwrap_or_default()
    }

    pub fn release_unit_buf(&mut self, mut buf: UnitBuf) {
        buf.clear();
        self.unit_bufs.push(buf);
    }

    pub fn live_inst_count(&self) -> usize {
        let mut count = 0;
        for unit in self.functions.values() {
            count += unit.insts.len() as usize;
        }
        for unit in self.exprs.values() {
            count += unit.insts.len() as usize;
        }
        count
    }
}

fn push_slice<T: Copy>(mem: &mut kmem::Mem<ProgramIr>, values: &[T]) -> MSlice<T, ProgramIr> {
    if values.is_empty() { MSlice::empty() } else { mem.pushn(values) }
}

pub fn commit_unit(ir: &mut ProgramIr, u: &UnitBuf, unit: &mut IrUnit) {
    let mut inst_map = std::mem::take(&mut ir.compact_inst_map);
    let mut block_map = std::mem::take(&mut ir.compact_block_map);
    inst_map.clear();
    inst_map.resize(u.insts.len(), None);
    block_map.clear();
    block_map.resize(u.blocks.len(), None);

    let mut identity = true;
    let mut inst_count = 0;
    let mut block_count = 0;
    let mut cur_block = u.body.first_block;
    while let Some(b) = cur_block {
        identity &= ix(b) == block_count;
        block_map[ix(b)] = Some(id_at(block_count));
        block_count += 1;
        let mut cur = u.blocks[ix(b)].first;
        while let Some(id) = cur {
            identity &= ix(id) == inst_count;
            inst_map[ix(id)] = Some(id_at(inst_count));
            inst_count += 1;
            cur = u.inst_layout[ix(id)].next;
        }
        cur_block = u.blocks[ix(b)].next;
    }
    identity &= inst_count == u.insts.len() && block_count == u.blocks.len();

    let mem = &mut ir.mem;
    unit.calls = push_slice(mem, &u.calls);
    unit.call_args = push_slice(mem, &u.call_args);
    unit.phi_cases = push_slice(mem, &u.phi_cases);
    unit.switch_cases = push_slice(mem, &u.switch_cases);
    unit.cmpxchgs = push_slice(mem, &u.cmpxchgs);
    unit.vec_ops = push_slice(mem, &u.vec_ops);

    if identity {
        unit.body = u.body;
        unit.insts = push_slice(mem, &u.insts);
        unit.inst_src = push_slice(mem, &u.inst_src);
        unit.inst_comment = push_slice(mem, &u.inst_comment);
        unit.blocks = push_slice(mem, &u.blocks);
    } else {
        let mut p = PayloadsMut {
            calls: mem.getn_mut(unit.calls),
            call_args: mem.getn_mut(unit.call_args),
            phi_cases: mem.getn_mut(unit.phi_cases),
            switch_cases: mem.getn_mut(unit.switch_cases),
            cmpxchgs: mem.getn_mut(unit.cmpxchgs),
            vec_ops: mem.getn_mut(unit.vec_ops),
        };
        let map_value = |v: &mut Value| {
            if let Value::Inst(id) = v {
                *id = inst_map[ix(*id)].expect("live inst refers to a removed inst");
            }
        };
        let map_block = |b: &mut BlockId| {
            *b = block_map[ix(*b)].expect("live inst refers to a removed block");
        };
        let mut insts = mem.new_list(inst_count as u32);
        let mut inst_src = mem.new_list(inst_count as u32);
        let mut inst_comment = mem.new_list(inst_count as u32);
        let mut blocks = mem.new_list(block_count as u32);
        let mut cur_block = u.body.first_block;
        while let Some(b) = cur_block {
            let data = u.blocks[ix(b)];
            let first_inst = insts.len();
            let mut cur = data.first;
            while let Some(id) = cur {
                let i = ix(id);
                let mut inst = u.insts[i];
                map_refs(&mut inst, &mut p, &map_value, &map_block);
                insts.push(inst);
                inst_src.push(u.inst_src[i]);
                inst_comment.push(u.inst_comment[i]);
                cur = u.inst_layout[i].next;
            }
            let last_inst = insts.len();
            let index = blocks.len();
            blocks.push(BlockData {
                kind: data.kind,
                first: if last_inst > first_inst { Some(id_at(first_inst)) } else { None },
                last: if last_inst > first_inst { Some(id_at(last_inst - 1)) } else { None },
                prev: if index > 0 { Some(id_at(index - 1)) } else { None },
                next: if index + 1 < block_count { Some(id_at(index + 1)) } else { None },
            });
            cur_block = data.next;
        }
        unit.body = UnitBody {
            first_block: if block_count > 0 { Some(id_at(0)) } else { None },
            last_block: if block_count > 0 { Some(id_at(block_count - 1)) } else { None },
            last_alloca: u.body.last_alloca.map(|id| inst_map[ix(id)].unwrap()),
        };
        unit.insts = insts.to_slice();
        unit.inst_src = inst_src.to_slice();
        unit.inst_comment = inst_comment.to_slice();
        unit.blocks = blocks.to_slice();
    }

    ir.compact_inst_map = inst_map;
    ir.compact_block_map = block_map;
}

pub struct IdMap<K, V> {
    slots: Vec<Option<V>>,
    keys: Vec<K>,
}

impl<K, V> Default for IdMap<K, V> {
    fn default() -> Self {
        IdMap { slots: Vec::new(), keys: Vec::new() }
    }
}

impl<K: crate::vpool::PoolIndex, V: Copy> IdMap<K, V> {
    #[inline]
    pub fn get(&self, k: K) -> Option<V> {
        self.slots.get(ix(k)).copied().flatten()
    }

    #[inline]
    pub fn contains(&self, k: K) -> bool {
        self.get(k).is_some()
    }

    pub fn insert(&mut self, k: K, v: V) {
        let i = ix(k);
        if i >= self.slots.len() {
            self.slots.resize(i + 1, None);
        }
        if self.slots[i].is_none() {
            self.keys.push(k);
        }
        self.slots[i] = Some(v);
    }

    pub fn is_empty(&self) -> bool {
        self.keys.is_empty()
    }

    pub fn clear(&mut self) {
        for k in &self.keys {
            self.slots[ix(*k)] = None;
        }
        self.keys.clear();
    }

    pub fn update_values(&mut self, mut f: impl FnMut(&mut V)) {
        for k in &self.keys {
            if let Some(v) = &mut self.slots[ix(*k)] {
                f(v);
            }
        }
    }

    pub fn retain(&mut self, mut keep: impl FnMut(K, V) -> bool) {
        let mut kept = 0;
        for i in 0..self.keys.len() {
            let k = self.keys[i];
            let slot = &mut self.slots[ix(k)];
            if keep(k, slot.unwrap()) {
                self.keys[kept] = k;
                kept += 1;
            } else {
                *slot = None;
            }
        }
        self.keys.truncate(kept);
    }
}
