use super::*;

#[derive(Clone, Copy)]
enum MatchCell {
    Any,
    Pattern(TypedPatternId),
}

#[derive(Clone, Copy)]
struct MatchMatrix {
    cells: TmpSlice<MatchCell>,
    rows: u32,
    columns: u32,
}

#[derive(Clone, Copy)]
enum MatchCtor {
    Bool(bool),
    Enum(u32),
    Sum(u32),
    Struct,
    Reference,
    Test(TypedPatternId),
}

#[derive(Clone, Copy)]
enum PatternHead {
    Any,
    Ctor(MatchCtor),
}

type MatchWitnessId = Handle<MatchWitness, MemTmp>;

#[derive(Clone, Copy)]
struct MatchWitness {
    type_id: TypeId,
    kind: MatchWitnessKind,
    children: TmpSlice<MatchWitnessId>,
}

#[derive(Clone, Copy)]
enum MatchWitnessKind {
    Any,
    Open,
    Ctor(MatchCtor),
}

struct SpecializedMatch {
    matrix: MatchMatrix,
    query: TmpSlice<MatchCell>,
    columns: TmpSlice<TypeId>,
    arity: usize,
}

impl TypedProgram {
    pub(super) fn check_pattern_exhaustiveness(
        &mut self,
        subject_type: TypeId,
        all_unguarded_patterns: &[TypedPatternId],
        subject_span: SpanId,
        skip_build_message: bool,
    ) -> K1Result<()> {
        let mark = self.tmp.mark();
        let mut root_cells = self.tmp.new_list(all_unguarded_patterns.len() as u32);
        root_cells.extend_iter(all_unguarded_patterns.iter().copied().map(MatchCell::Pattern));
        let root_cells = root_cells.to_slice();
        let columns = self.tmp.pushn(&[subject_type]);
        let mut first_useless = None;

        for (index, pattern) in all_unguarded_patterns.iter().copied().enumerate() {
            let matrix = MatchMatrix { cells: root_cells, rows: index as u32, columns: 1 };
            let query = self.tmp.pushn(&[MatchCell::Pattern(pattern)]);
            if self.match_useful(matrix, query, columns).is_none()
                && first_useless.is_none()
                && !self.patterns.pattern_never_useless(pattern)
                && !self.pattern_matches_uninhabited(pattern)
            {
                first_useless = Some(pattern);
            }
        }

        let matrix = MatchMatrix {
            cells: root_cells,
            rows: all_unguarded_patterns.len() as u32,
            columns: 1,
        };
        let wildcard = self.tmp.pushn(&[MatchCell::Any]);
        let missing = self.match_useful(matrix, wildcard, columns);
        let missing_message = missing.map(|witnesses| {
            if skip_build_message {
                "Unhandled patterns".to_string()
            } else {
                let witness = self.tmp.getn(witnesses)[0];
                format!(
                    "Non-exhaustive match; for example, this pattern is not covered:\n- {}",
                    self.match_witness_to_string(witness)
                )
            }
        });
        let useless_message = first_useless.map(|pattern| {
            let span = self.patterns.get(pattern).span_id();
            let message =
                format!("This pattern handled no cases: {}", self.pattern_to_string(pattern));
            (span, message)
        });
        self.tmp.reset_to(mark);

        if let Some(message) = missing_message {
            return self.make_fail(message, subject_span);
        }
        if let Some((span, message)) = useless_message {
            return self.make_fail(message, span);
        }
        Ok(())
    }

    fn match_useful(
        &mut self,
        matrix: MatchMatrix,
        query: TmpSlice<MatchCell>,
        columns: TmpSlice<TypeId>,
    ) -> Option<TmpSlice<MatchWitnessId>> {
        debug_assert_eq!(matrix.columns, query.len());
        debug_assert_eq!(matrix.columns, columns.len());

        if matrix.columns == 0 {
            return (matrix.rows == 0).then_some(MSlice::empty());
        }
        if self.match_matrix_has_any_row(matrix) {
            return None;
        }

        let query_cells = self.tmp.getn(query);
        if matrix.rows == 0 && query_cells.iter().all(|cell| matches!(cell, MatchCell::Any)) {
            let column_types = self.tmp.getn(columns);
            if column_types.iter().all(|type_id| self.match_type_is_inhabited(*type_id)) {
                let mut witnesses = self.tmp.new_list(column_types.len() as u32);
                for type_id in column_types {
                    witnesses.push(self.add_match_witness(
                        *type_id,
                        MatchWitnessKind::Any,
                        MSlice::empty(),
                    ));
                }
                return Some(witnesses.to_slice());
            }
        }

        let first_type = self.tmp.getn(columns)[0];
        match self.match_cell_head(query_cells[0]) {
            PatternHead::Ctor(ctor) => self.match_useful_ctor(matrix, query, columns, ctor),
            PatternHead::Any => self.match_useful_any(matrix, query, columns, first_type),
        }
    }

    fn match_useful_any(
        &mut self,
        matrix: MatchMatrix,
        query: TmpSlice<MatchCell>,
        columns: TmpSlice<TypeId>,
        type_id: TypeId,
    ) -> Option<TmpSlice<MatchWitnessId>> {
        if type_id == self.builtin_types.string() {
            return self.match_useful_open(matrix, query, columns, type_id);
        }

        match *self.types.get(type_id) {
            Type::Bool => self
                .match_useful_ctor(matrix, query, columns, MatchCtor::Bool(false))
                .or_else(|| self.match_useful_ctor(matrix, query, columns, MatchCtor::Bool(true))),
            Type::Enum(enum_type) => {
                for index in 0..enum_type.member_values.len() {
                    if let Some(witness) =
                        self.match_useful_ctor(matrix, query, columns, MatchCtor::Enum(index))
                    {
                        return Some(witness);
                    }
                }
                None
            }
            Type::Sum(sum_type) => {
                for index in 0..sum_type.variants.len() {
                    if let Some(witness) =
                        self.match_useful_ctor(matrix, query, columns, MatchCtor::Sum(index))
                    {
                        return Some(witness);
                    }
                }
                None
            }
            Type::Struct(_) => match self.get_as_container_instance(type_id) {
                Some((_, ContainerKind::Buffer | ContainerKind::Span)) => {
                    self.match_useful_open(matrix, query, columns, type_id)
                }
                _ => self.match_useful_ctor(matrix, query, columns, MatchCtor::Struct),
            },
            Type::Reference(_) => {
                self.match_useful_ctor(matrix, query, columns, MatchCtor::Reference)
            }
            Type::Never | Type::Function(_) => None,
            _ => self.match_useful_open(matrix, query, columns, type_id),
        }
    }

    fn match_useful_ctor(
        &mut self,
        matrix: MatchMatrix,
        query: TmpSlice<MatchCell>,
        columns: TmpSlice<TypeId>,
        ctor: MatchCtor,
    ) -> Option<TmpSlice<MatchWitnessId>> {
        let type_id = self.tmp.getn(columns)[0];
        let specialized = self.specialize_match(matrix, query, columns, type_id, ctor);
        let children =
            self.match_useful(specialized.matrix, specialized.query, specialized.columns)?;
        Some(self.wrap_match_witnesses(type_id, ctor, specialized.arity, children))
    }

    fn match_useful_open(
        &mut self,
        matrix: MatchMatrix,
        query: TmpSlice<MatchCell>,
        columns: TmpSlice<TypeId>,
        type_id: TypeId,
    ) -> Option<TmpSlice<MatchWitnessId>> {
        let specialized = self.default_match(matrix, query, columns);
        let tail = self.match_useful(specialized.matrix, specialized.query, specialized.columns)?;
        let tail_witnesses = self.tmp.getn(tail);
        let mut result = self.tmp.new_list((tail_witnesses.len() + 1) as u32);
        result.push(self.add_match_witness(type_id, MatchWitnessKind::Open, MSlice::empty()));
        result.extend(tail_witnesses);
        Some(result.to_slice())
    }

    fn specialize_match(
        &mut self,
        matrix: MatchMatrix,
        query: TmpSlice<MatchCell>,
        columns: TmpSlice<TypeId>,
        type_id: TypeId,
        ctor: MatchCtor,
    ) -> SpecializedMatch {
        let child_types = self.match_ctor_child_types(type_id, ctor);
        let arity = child_types.len() as usize;
        let new_columns_count = matrix
            .columns
            .checked_sub(1)
            .and_then(|count| count.checked_add(child_types.len()))
            .expect("pattern matrix column count overflow");
        let old_cells = self.tmp.getn(matrix.cells);
        let mut selected_rows = 0u32;
        for row in 0..matrix.rows as usize {
            let first = old_cells[row * matrix.columns as usize];
            if self.match_cell_accepts_ctor(first, ctor) {
                selected_rows += 1;
            }
        }

        let new_cell_count = selected_rows
            .checked_mul(new_columns_count)
            .expect("pattern matrix cell count overflow");
        let mut new_cells = self.tmp.new_list(new_cell_count);
        for row in 0..matrix.rows as usize {
            let row_start = row * matrix.columns as usize;
            let first = old_cells[row_start];
            if !self.match_cell_accepts_ctor(first, ctor) {
                continue;
            }
            for child_index in 0..arity {
                new_cells.push(self.match_cell_child(first, ctor, child_index));
            }
            new_cells.extend(&old_cells[row_start + 1..row_start + matrix.columns as usize]);
        }

        let old_query = self.tmp.getn(query);
        let mut new_query = self.tmp.new_list(new_columns_count);
        for child_index in 0..arity {
            new_query.push(self.match_cell_child(old_query[0], ctor, child_index));
        }
        new_query.extend(&old_query[1..]);

        let old_columns = self.tmp.getn(columns);
        let mut new_columns = self.tmp.new_list(new_columns_count);
        new_columns.extend(self.tmp.getn(child_types));
        new_columns.extend(&old_columns[1..]);

        SpecializedMatch {
            matrix: MatchMatrix {
                cells: new_cells.to_slice(),
                rows: selected_rows,
                columns: new_columns_count,
            },
            query: new_query.to_slice(),
            columns: new_columns.to_slice(),
            arity,
        }
    }

    fn default_match(
        &mut self,
        matrix: MatchMatrix,
        query: TmpSlice<MatchCell>,
        columns: TmpSlice<TypeId>,
    ) -> SpecializedMatch {
        let new_columns_count = matrix.columns - 1;
        let old_cells = self.tmp.getn(matrix.cells);
        let mut selected_rows = 0u32;
        for row in 0..matrix.rows as usize {
            if matches!(
                self.match_cell_head(old_cells[row * matrix.columns as usize]),
                PatternHead::Any
            ) {
                selected_rows += 1;
            }
        }

        let new_cell_count = selected_rows
            .checked_mul(new_columns_count)
            .expect("pattern matrix cell count overflow");
        let mut new_cells = self.tmp.new_list(new_cell_count);
        for row in 0..matrix.rows as usize {
            let row_start = row * matrix.columns as usize;
            if matches!(self.match_cell_head(old_cells[row_start]), PatternHead::Any) {
                new_cells.extend(&old_cells[row_start + 1..row_start + matrix.columns as usize]);
            }
        }

        let old_query = self.tmp.getn(query);
        let old_columns = self.tmp.getn(columns);
        SpecializedMatch {
            matrix: MatchMatrix {
                cells: new_cells.to_slice(),
                rows: selected_rows,
                columns: new_columns_count,
            },
            query: self.tmp.pushn(&old_query[1..]),
            columns: self.tmp.pushn(&old_columns[1..]),
            arity: 0,
        }
    }

    fn match_matrix_has_any_row(&self, matrix: MatchMatrix) -> bool {
        let cells = self.tmp.getn(matrix.cells);
        (0..matrix.rows as usize).any(|row| {
            let start = row * matrix.columns as usize;
            cells[start..start + matrix.columns as usize]
                .iter()
                .all(|cell| matches!(self.match_cell_head(*cell), PatternHead::Any))
        })
    }

    fn match_cell_head(&self, cell: MatchCell) -> PatternHead {
        let MatchCell::Pattern(pattern) = cell else { return PatternHead::Any };
        match self.patterns.get(pattern) {
            TypedPattern::Wildcard(_) | TypedPattern::Variable(_) => PatternHead::Any,
            TypedPattern::LiteralBool(value, _) => PatternHead::Ctor(MatchCtor::Bool(*value)),
            TypedPattern::Enum(pattern) => PatternHead::Ctor(MatchCtor::Enum(pattern.index)),
            TypedPattern::Sum(pattern) => PatternHead::Ctor(MatchCtor::Sum(pattern.variant_index)),
            TypedPattern::Struct(_) => PatternHead::Ctor(MatchCtor::Struct),
            TypedPattern::Reference(_) => PatternHead::Ctor(MatchCtor::Reference),
            _ => PatternHead::Ctor(MatchCtor::Test(pattern)),
        }
    }

    fn match_cell_accepts_ctor(&self, cell: MatchCell, ctor: MatchCtor) -> bool {
        match self.match_cell_head(cell) {
            PatternHead::Any => true,
            PatternHead::Ctor(head) => self.match_ctors_equal(head, ctor),
        }
    }

    fn match_ctors_equal(&self, left: MatchCtor, right: MatchCtor) -> bool {
        match (left, right) {
            (MatchCtor::Bool(a), MatchCtor::Bool(b)) => a == b,
            (MatchCtor::Enum(a), MatchCtor::Enum(b)) => a == b,
            (MatchCtor::Sum(a), MatchCtor::Sum(b)) => a == b,
            (MatchCtor::Struct, MatchCtor::Struct) => true,
            (MatchCtor::Reference, MatchCtor::Reference) => true,
            (MatchCtor::Test(a), MatchCtor::Test(b)) => a == b,
            _ => false,
        }
    }

    fn match_cell_child(&self, cell: MatchCell, ctor: MatchCtor, child_index: usize) -> MatchCell {
        let MatchCell::Pattern(pattern) = cell else { return MatchCell::Any };
        match (self.patterns.get(pattern), ctor) {
            (TypedPattern::Sum(sum), MatchCtor::Sum(_)) => {
                sum.payload.map(MatchCell::Pattern).unwrap_or(MatchCell::Any)
            }
            (TypedPattern::Struct(struc), MatchCtor::Struct) => self
                .patterns
                .get_slice(struc.fields)
                .iter()
                .find(|field| field.field_index as usize == child_index)
                .map(|field| MatchCell::Pattern(field.pattern))
                .unwrap_or(MatchCell::Any),
            (TypedPattern::Reference(reference), MatchCtor::Reference) => {
                MatchCell::Pattern(reference.inner_pattern)
            }
            (TypedPattern::Wildcard(_) | TypedPattern::Variable(_), _) => MatchCell::Any,
            _ => MatchCell::Any,
        }
    }

    fn match_ctor_child_types(&mut self, type_id: TypeId, ctor: MatchCtor) -> TmpSlice<TypeId> {
        match ctor {
            MatchCtor::Sum(index) => {
                let sum = *self.types.get(type_id).expect_sum();
                self.mem
                    .get_nth(sum.variants, index as usize)
                    .payload
                    .map(|payload| self.tmp.pushn(&[payload]))
                    .unwrap_or(MSlice::empty())
            }
            MatchCtor::Struct => {
                let struc = *self.types.get(type_id).expect_struct();
                let mut fields = self.tmp.new_list(struc.fields.len());
                fields.extend_iter(self.mem.getn(struc.fields).iter().map(|field| field.type_id));
                fields.to_slice()
            }
            MatchCtor::Reference => {
                let reference = self.types.get(type_id).expect_reference();
                self.tmp.pushn(&[reference.inner_type])
            }
            _ => MSlice::empty(),
        }
    }

    fn wrap_match_witnesses(
        &mut self,
        type_id: TypeId,
        ctor: MatchCtor,
        arity: usize,
        witnesses: TmpSlice<MatchWitnessId>,
    ) -> TmpSlice<MatchWitnessId> {
        let witnesses = self.tmp.getn(witnesses);
        let children = self.tmp.pushn(&witnesses[..arity]);
        let wrapped = self.add_match_witness(type_id, MatchWitnessKind::Ctor(ctor), children);
        let mut result = self.tmp.new_list((witnesses.len() - arity + 1) as u32);
        result.push(wrapped);
        result.extend(&witnesses[arity..]);
        result.to_slice()
    }

    fn add_match_witness(
        &mut self,
        type_id: TypeId,
        kind: MatchWitnessKind,
        children: TmpSlice<MatchWitnessId>,
    ) -> MatchWitnessId {
        self.tmp.push_h(MatchWitness { type_id, kind, children })
    }

    fn match_type_is_inhabited(&mut self, type_id: TypeId) -> bool {
        let mut ancestors = self.tmp.new_list(16);
        self.match_type_is_inhabited_rec(type_id, &mut ancestors)
    }

    fn match_type_is_inhabited_rec(
        &mut self,
        type_id: TypeId,
        ancestors: &mut List<TypeId, MemTmp>,
    ) -> bool {
        if type_id == self.builtin_types.string() || ancestors.contains(&type_id) {
            return true;
        }
        ancestors.push_grow(&mut self.tmp, type_id);
        let inhabited = match *self.types.get(type_id) {
            Type::Never | Type::Function(_) => false,
            Type::Struct(struc) => self
                .mem
                .getn(struc.fields)
                .iter()
                .all(|field| self.match_type_is_inhabited_rec(field.type_id, ancestors)),
            Type::Sum(sum) => self.mem.getn(sum.variants).iter().any(|variant| {
                variant
                    .payload
                    .is_none_or(|payload| self.match_type_is_inhabited_rec(payload, ancestors))
            }),
            Type::Reference(reference) => {
                self.match_type_is_inhabited_rec(reference.inner_type, ancestors)
            }
            Type::Generic(generic) => self.match_type_is_inhabited_rec(generic.inner, ancestors),
            Type::Array(array) => {
                self.get_concrete_count_of_array(array.size_type) == Some(0)
                    || self.match_type_is_inhabited_rec(array.element_type, ancestors)
            }
            _ => true,
        };
        ancestors.pop();
        inhabited
    }

    fn match_witness_to_string(&self, witness: MatchWitnessId) -> String {
        let mut result = String::new();
        self.display_match_witness(witness, &mut result).unwrap();
        result
    }

    fn display_match_witness(
        &self,
        witness_id: MatchWitnessId,
        out: &mut impl std::fmt::Write,
    ) -> std::fmt::Result {
        let witness = *self.tmp.get(witness_id);
        match witness.kind {
            MatchWitnessKind::Any => out.write_str("_"),
            MatchWitnessKind::Open => {
                write!(out, "<{}>", self.type_id_to_string(witness.type_id))
            }
            MatchWitnessKind::Ctor(MatchCtor::Bool(value)) => write!(out, "{value}"),
            MatchWitnessKind::Ctor(MatchCtor::Enum(index)) => {
                let enum_type = *self.types.get(witness.type_id).expect_enum();
                let member = self.mem.get_nth(enum_type.member_values, index as usize);
                write!(out, ":{}", self.ident_str(member.name))
            }
            MatchWitnessKind::Ctor(MatchCtor::Sum(index)) => {
                let sum = *self.types.get(witness.type_id).expect_sum();
                let variant = self.mem.get_nth(sum.variants, index as usize);
                write!(out, ":{}", self.ident_str(variant.name))?;
                if let Some(child) = self.tmp.getn(witness.children).first() {
                    out.write_str("(")?;
                    self.display_match_witness(*child, out)?;
                    out.write_str(")")?;
                }
                Ok(())
            }
            MatchWitnessKind::Ctor(MatchCtor::Struct) => {
                let struc = *self.types.get(witness.type_id).expect_struct();
                let fields = self.mem.getn(struc.fields);
                let children = self.tmp.getn(witness.children);
                let meaningful = children
                    .iter()
                    .enumerate()
                    .filter(|(_, child)| {
                        !matches!(self.tmp.get(**child).kind, MatchWitnessKind::Any)
                    })
                    .collect::<SmallVec<[_; 8]>>();
                if meaningful.is_empty() {
                    return out.write_str("_");
                }
                out.write_str(".{ ")?;
                for (display_index, (field_index, child)) in meaningful.iter().enumerate() {
                    if display_index != 0 {
                        out.write_str(", ")?;
                    }
                    write!(out, "{} = ", self.ident_str(fields[*field_index].name))?;
                    self.display_match_witness(**child, out)?;
                }
                out.write_str(" }")
            }
            MatchWitnessKind::Ctor(MatchCtor::Reference) => {
                let child = self.tmp.getn(witness.children)[0];
                self.display_match_witness(child, out)?;
                out.write_str("*")
            }
            MatchWitnessKind::Ctor(MatchCtor::Test(pattern)) => self.display_pattern(pattern, out),
        }
    }
}
