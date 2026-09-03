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

    pub(super) fn compile_pattern_to_type(
        &mut self,
        pat_expr: ParsedPatternId,
        target_type_id: TypeId,
        scope_id: ScopeId,
        allow_bindings: bool,
    ) -> K1Result<TypedPatternId> {
        let parsed_pattern_expr = self.ast.patterns.get(pat_expr);
        match parsed_pattern_expr {
            ParsedPattern::Wildcard(span) => Ok(self.patterns.add(TypedPattern::Wildcard(*span))),
            ParsedPattern::Literal(literal_expr_id) => {
                match self.ast.exprs.get(*literal_expr_id).expect_literal() {
                    ParsedLiteral::Char(c, span) => match self.types.get(target_type_id) {
                        Type::Char => Ok(self.patterns.add(TypedPattern::LiteralChar(*c, *span))),
                        _ => Err(kerr!(
                            self,
                            self.ast.get_pattern_span(pat_expr),
                            "unrelated pattern type char will never match {}",
                            target_type_id
                        )),
                    },
                    ParsedLiteral::Numeric(num_lit) => {
                        let num_lit = *num_lit;
                        let num_value_id = self.eval_numeric_value(
                            num_lit.text_span,
                            EvalExprContext::make(scope_id)
                                .with_expected_type(Some(target_type_id)),
                        )?;
                        match self.static_values.get(num_value_id) {
                            StaticValue::Int(_) => match self.types.get(target_type_id) {
                                Type::Integer(_) => Ok(self
                                    .patterns
                                    .add(TypedPattern::LiteralInteger(num_value_id, num_lit.span))),
                                _ => Err(kerr!(
                                    self,
                                    self.ast.get_pattern_span(pat_expr),
                                    "integer literal pattern will never match {}",
                                    target_type_id
                                )),
                            },
                            StaticValue::Float(_) => match self.types.get(target_type_id) {
                                Type::Float(_) => Ok(self
                                    .patterns
                                    .add(TypedPattern::LiteralFloat(num_value_id, num_lit.span))),
                                _ => Err(kerr!(
                                    self,
                                    self.ast.get_pattern_span(pat_expr),
                                    "float literal pattern will never match {}",
                                    target_type_id
                                )),
                            },
                            _ => {
                                unreachable!(
                                    "eval_numeric_value should produce only Integer and Float exprs"
                                )
                            }
                        }
                    }
                    ParsedLiteral::Bool(b, span) => match self.types.get(target_type_id) {
                        Type::Bool => Ok(self.patterns.add(TypedPattern::LiteralBool(*b, *span))),
                        _ => Err(kerr!(
                            self,
                            self.ast.get_pattern_span(pat_expr),
                            "bool literal pattern will never match {}",
                            target_type_id
                        )),
                    },
                    ParsedLiteral::String(string_id, span) => {
                        match self.types.get(target_type_id) {
                            Type::StaticValue(svt)
                                if svt.family_type_id == self.builtin_types.string() =>
                            {
                                Ok(())
                            }
                            _ if target_type_id == self.builtin_types.string() => Ok(()),
                            _ => Err(kerr!(
                                self,
                                self.ast.get_pattern_span(pat_expr),
                                "string literal pattern will never match {}",
                                target_type_id
                            )),
                        }?;
                        Ok(self.patterns.add(TypedPattern::LiteralString(*string_id, *span)))
                    }
                }
            }
            ParsedPattern::Variable(ident_id, span) => {
                if *ident_id == self.ast.idents.b.null {
                    match self.types.get(target_type_id) {
                        Type::Reference(reference_type) => Ok(self
                            .patterns
                            .add(TypedPattern::RefNull(reference_type.inner_type, *span))),
                        Type::Pointer => Ok(self.patterns.add(TypedPattern::PointerNull(*span))),
                        _ => Err(kerr!(
                            self,
                            self.ast.get_pattern_span(pat_expr),
                            "'null' is a pattern that applies to reference (*t) types and ptr"
                        )),
                    }
                } else {
                    if !allow_bindings {
                        kbail!(self, *span, "Bindings are not allowed here");
                    }
                    Ok(self.patterns.add(TypedPattern::Variable(VariablePattern {
                        name: *ident_id,
                        type_id: target_type_id,
                        span: *span,
                    })))
                }
            }
            ParsedPattern::Sum(sum_pattern) => {
                let sum_pattern = *sum_pattern;
                let sum_pattern_span = sum_pattern.span;

                if let Some(cs) = &mut self.completion
                    && cs.site.is_none()
                    && sum_pattern.variant_name == cs.marker
                {
                    cs.site = Some(CompletionSite::Variant { type_id: target_type_id });
                }

                match self.types.get(target_type_id) {
                    Type::Sum(sum_type) => {
                        if let Some(name) = sum_pattern.sum_name {
                            match self.scopes.find_type(scope_id, name) {
                                None => {
                                    kbail!(
                                        self,
                                        sum_pattern.span,
                                        "No type named '{}'",
                                        self.ident_str(name).blue()
                                    );
                                }
                                Some((named_type, _)) => {
                                    // Consider generics: 'Opt.Some' applies to all Opt[T]s, so we consider
                                    // the 'base' type
                                    let base_type = match self.get_instance_info(target_type_id) {
                                        Some(info) => info.generic_parent,
                                        None => target_type_id,
                                    };
                                    if base_type != named_type {
                                        kbail!(
                                            self,
                                            sum_pattern.span,
                                            "Impossible pattern: sum pattern refers to type '{}' which is not the same as match target '{}'",
                                            self.type_id_to_string_ext(
                                                named_type,
                                                dump::TypeDisplayMode::Expand
                                            )
                                            .blue(),
                                            self.type_id_to_string_ext(
                                                base_type,
                                                dump::TypeDisplayMode::Expand
                                            )
                                            .blue()
                                        );
                                    }
                                }
                            }
                        }
                        let Some(matching_variant) =
                            self.sum_variant_by_name(sum_type.variants, sum_pattern.variant_name)
                        else {
                            kbail!(
                                self,
                                sum_pattern.span,
                                "Impossible pattern: No variant named '{}' in {}",
                                sum_pattern.variant_name,
                                target_type_id,
                            );
                        };

                        let matching_variant_index = matching_variant.index;
                        let matching_variant_name = matching_variant.name;
                        self.emit_ls_entity(
                            sum_pattern.span,
                            LsEntityKind::Variant {
                                type_id: target_type_id,
                                variant_index: matching_variant_index,
                            },
                        );

                        let payload_pattern = match &sum_pattern.payload_pattern {
                            None if matching_variant.payload == Some(EMPTY_TYPE_ID) => {
                                Some(self.patterns.add(TypedPattern::Wildcard(sum_pattern_span)))
                            }
                            None => None,
                            Some(payload_expr) => {
                                let payload_type_id =
                                    matching_variant.payload.ok_or_else(|| {
                                        kerr!(
                                            self,
                                            sum_pattern.span,
                                            "Impossible pattern: Variant '{}' has no payload",
                                            matching_variant.name
                                        )
                                    })?;
                                let payload_pattern = self.compile_pattern_to_type(
                                    *payload_expr,
                                    payload_type_id,
                                    scope_id,
                                    allow_bindings,
                                )?;
                                Some(payload_pattern)
                            }
                        };

                        let sum_pattern = TypedSumPattern {
                            sum_type_id: target_type_id,
                            variant_index: matching_variant_index,
                            variant_name: matching_variant_name,
                            payload: payload_pattern,
                            span: sum_pattern_span,
                        };
                        Ok(self.patterns.add(TypedPattern::Sum(sum_pattern)))
                    }
                    Type::Enum(e) => {
                        if let Some(name) = sum_pattern.sum_name {
                            match self.scopes.find_type(scope_id, name) {
                                None => {
                                    kbail!(
                                        self,
                                        sum_pattern.span,
                                        "No type named '{}'",
                                        self.ident_str(name).blue()
                                    );
                                }
                                Some((named_type, _)) => {
                                    // No need to consider generics
                                    if target_type_id != named_type {
                                        kbail!(
                                            self,
                                            sum_pattern.span,
                                            "Impossible pattern: sum pattern refers to type '{}' which is not the same as match target '{}'",
                                            named_type,
                                            target_type_id,
                                        );
                                    }
                                }
                            }
                        }
                        let Some((matching_value_index, matching_value)) =
                            self.enum_value_by_name(e.member_values, sum_pattern.variant_name)
                        else {
                            kbail!(
                                self,
                                sum_pattern.span,
                                "Impossible pattern: No value named '{}' in {}",
                                sum_pattern.variant_name,
                                target_type_id
                            );
                        };
                        let matching_value_name = matching_value.name;
                        self.emit_ls_entity(
                            sum_pattern.span,
                            LsEntityKind::Variant {
                                type_id: target_type_id,
                                variant_index: matching_value_index as u32,
                            },
                        );

                        let enum_pattern = TypedEnumPattern {
                            enum_type_id: target_type_id,
                            member_name: matching_value_name,
                            index: matching_value_index as u32,
                            int_value: matching_value.int_value,
                            span: sum_pattern_span,
                        };
                        Ok(self.patterns.add(TypedPattern::Enum(enum_pattern)))
                    }
                    _ => Err(kerr!(
                        self,
                        sum_pattern.span,
                        "this pattern will never match {}",
                        target_type_id
                    )),
                }
            }
            ParsedPattern::Struct(struct_pattern) => {
                let target_type = self.types.get(target_type_id);
                let struct_pattern = *struct_pattern;
                let expected_struct = *target_type.as_struct().ok_or_else(|| {
                    kerr!(
                        self,
                        struct_pattern.span,
                        "Impossible pattern: Match target '{}' is not a struct",
                        target_type_id
                    )
                })?;
                let mut fields = self.patterns.mem.new_list(struct_pattern.fields.len());
                for (field_name, field_parsed_pattern_id) in
                    self.ast.mem.getn(struct_pattern.fields)
                {
                    let (expected_field_index, expected_field) =
                        expected_struct.find_field(&self.mem, *field_name).ok_or_else(|| {
                            kerr!(
                                self,
                                self.ast.get_pattern_span(*field_parsed_pattern_id),
                                "Impossible pattern: Struct has no field named '{}'",
                                self.ident_str(*field_name).blue()
                            )
                        })?;
                    let field_type_id = expected_field.type_id;
                    let field_pattern = self.compile_pattern_to_type(
                        *field_parsed_pattern_id,
                        field_type_id,
                        scope_id,
                        allow_bindings,
                    )?;
                    fields.push(TypedStructPatternField {
                        name: *field_name,
                        pattern: field_pattern,
                        field_index: expected_field_index as u32,
                        field_type_id: expected_field.type_id,
                    });
                }
                let struct_pattern = TypedStructPattern {
                    struct_type_id: target_type_id,
                    fields: fields.to_slice(),
                    span: struct_pattern.span,
                };
                Ok(self.patterns.add(TypedPattern::Struct(struct_pattern)))
            }
            ParsedPattern::Reference(reference_pattern) => {
                let Type::Reference(r) = self.types.get(target_type_id) else {
                    kbail!(
                        self,
                        reference_pattern.span,
                        "Reference pattern will never match non-reference {}",
                        target_type_id
                    );
                };
                let reference_pattern_span = reference_pattern.span;
                let inner_pattern = self.compile_pattern_to_type(
                    reference_pattern.inner,
                    r.inner_type,
                    scope_id,
                    allow_bindings,
                )?;
                Ok(self.patterns.add(TypedPattern::Reference(TypedReferencePattern {
                    inner_pattern,
                    span: reference_pattern_span,
                })))
            }
            ParsedPattern::Type(parsed_type_pattern) => {
                let parsed_type_pattern = *parsed_type_pattern;
                let type_id = self.eval_type_expr(parsed_type_pattern.type_expr, scope_id)?;
                let inner_pattern = self.compile_pattern_to_type(
                    parsed_type_pattern.inner,
                    type_id,
                    scope_id,
                    allow_bindings,
                )?;
                let typed_pattern_id = self.patterns.add(TypedPattern::Type(TypePattern {
                    inner_pattern,
                    type_id,
                    span: parsed_type_pattern.span,
                }));
                Ok(typed_pattern_id)
            }
        }
    }

    pub(super) fn eval_match_expr(
        &mut self,
        match_expr_id: ParsedExprId,
        ctx: EvalExprContext,
        check_exhaustive: bool,
        allow_bindings: bool,
        fallback_expr: Option<TypedExprId>,
    ) -> K1Result<TypedExprId> {
        pub(super) fn synth_match_scrutinee(
            k1: &mut TypedProgram,
            subject: TypedExprId,
            span: SpanId,
        ) -> TypedExprId {
            match k1.types.get(k1.exprs.get_type(subject)) {
                Type::Reference(_) => {
                    let deref = k1.synth_dereference(subject);
                    synth_match_scrutinee(k1, deref, span)
                }
                Type::Sum(_) => k1.synth_sum_get_tag(subject, span),
                Type::Enum(_) => k1.synth_enum_get_value(subject, span),
                _ => subject,
            }
        }

        let parsed_match = *self.ast.exprs.get(match_expr_id).as_match().unwrap();
        if parsed_match.is_static {
            return self.eval_static_match_expr(match_expr_id, ctx);
        };
        if parsed_match.cases.is_empty() {
            return self.make_fail(
                "match with no arms; note `x is {}` is an empty match, `x is .{}` matches the empty struct",
                parsed_match.span,
            );
        }
        let subject_expr =
            self.eval_expr(parsed_match.match_subject, ctx.with_no_expected_type())?;

        let match_subject_variable =
            self.synth_variable_defn_simple(self.ast.idents.b.subject, subject_expr, ctx.scope_id);

        let match_expr_span = parsed_match.span;

        let parsed_cases = parsed_match.cases;
        let parsed_pattern_count: u32 = self
            .ast
            .mem
            .getn(parsed_cases)
            .iter()
            .map(|parsed_case| parsed_case.patterns.len())
            .sum();

        let mut typed_arms: List<TypedMatchArm, _> = self.mem.new_list(parsed_pattern_count + 1); // Add one for fallback arm

        let mut expected_arm_type_id = ctx.expected_type_id;

        let mut all_unguarded_patterns: List<TypedPatternId, MemTmp> =
            self.tmp.new_list(parsed_pattern_count);
        let subject_type = self.exprs.get_type(match_subject_variable.variable_expr);
        let subject_expr_span = self.exprs.get_span(match_subject_variable.variable_expr);

        // Core loop to build up the typed, compiled match arms
        let mut first_error = None;
        for parsed_case in self.ast.mem.getn(parsed_cases) {
            let multi_pattern = parsed_case.patterns.len() > 1;
            let mut expected_bindings: Option<SmallVec<[VariablePattern; 8]>> = None;
            for parsed_pattern_id in parsed_case.patterns.as_slice(&self.ast.mem).iter() {
                let pattern = self.compile_pattern_to_type(
                    *parsed_pattern_id,
                    subject_type,
                    ctx.scope_id,
                    allow_bindings,
                )?;
                let pattern_bindings = self.patterns.get_pattern_bindings(pattern);

                // If a match arm has multiple patterns, they must produce the exact same
                // set of variable bindings: matching name and type
                if multi_pattern {
                    match &expected_bindings {
                        None => {
                            expected_bindings = Some(pattern_bindings.clone());
                        }
                        Some(expected_bindings) => {
                            let this_pattern_bindings = &pattern_bindings;
                            if this_pattern_bindings.is_empty() && !expected_bindings.is_empty() {
                                kbail!(
                                    self,
                                    self.patterns.get(pattern).span_id(),
                                    "Patterns in a multiple pattern arm must have the exact same bindings; but this one has none"
                                );
                            }
                            for (exp_binding, this_binding) in
                                expected_bindings.iter().zip(this_pattern_bindings.iter())
                            {
                                if exp_binding.name != this_binding.name {
                                    kbail!(
                                        self,
                                        this_binding.span,
                                        "Patterns in a multiple pattern arm must have the exact same bindings"
                                    );
                                }
                                if exp_binding.type_id != this_binding.type_id {
                                    kbail!(
                                        self,
                                        this_binding.span,
                                        "Patterns in a multiple pattern arm must have the exact same bindings; but the type differs for {}: {} vs {}",
                                        exp_binding.name,
                                        exp_binding.type_id,
                                        this_binding.type_id
                                    );
                                }
                            }
                        }
                    }
                }

                if parsed_case.guard_condition_expr.is_none() {
                    all_unguarded_patterns.push(pattern);
                }

                // Note: We compile the arm's consequent expression and the guard condition as many times as there are patterns, since each
                // one has its own scope. To get around this we'd have to create only one compiled arm even for
                // multi-pattern binding arms, and have the condition be a boolean OR of the various
                // arms, and somehow compile in the right variables defns based on which one passed.
                // Which isn't possible to know at compile time. So I think this is just where we are.
                // It'd be nice to re-use the typed expr across different scopes, but we can't do that
                //
                // The solution once again is to compile things multiple times if needed, and just make
                // compilation fast
                {
                    let pattern_eval_ctx = if pattern_bindings.is_empty() {
                        ctx.with_no_expected_type()
                    } else {
                        let arm_scope_id = self.scopes.add_child_scope(
                            ctx.scope_id,
                            ScopeType::MatchArm,
                            ScopeOwnerId::None,
                        );
                        ctx.with_scope(arm_scope_id).with_no_expected_type()
                    };
                    let mut instrs = self.mem.new_list(4);
                    let case = self.compile_pattern_into_values(
                        pattern,
                        match_subject_variable.variable_expr,
                        &mut instrs,
                        false,
                        true,
                        pattern_eval_ctx,
                    )?;

                    if let Some(guard_condition_expr_id) = parsed_case.guard_condition_expr {
                        let guard_condition_expr = self.eval_expr(
                            guard_condition_expr_id,
                            pattern_eval_ctx.with_expected_type(Some(BOOL_TYPE_ID)),
                        )?;
                        let guard_condition_type = self.exprs.get_type(guard_condition_expr);
                        if let Err(msg) = self.check_types(
                            BOOL_TYPE_ID,
                            guard_condition_type,
                            pattern_eval_ctx.scope_id,
                        ) {
                            kbail!(
                                self,
                                self.ast.get_expr_span(guard_condition_expr_id),
                                "Expected boolean condition: {msg}"
                            );
                        };
                        instrs.push_grow(
                            &mut self.mem,
                            MatchingConditionInstr::cond(guard_condition_expr),
                        );
                    };

                    // Once we've evaluated the conditions, we can eval the consequent expression inside of it,
                    // since the bindings are now available
                    let consequent_result = self.eval_expr_with_coercion(
                        parsed_case.expression,
                        pattern_eval_ctx.with_expected_type(expected_arm_type_id),
                        true,
                    );
                    let consequent_expr = match consequent_result {
                        Err(err) => {
                            self.report(err);
                            first_error = Some(err);
                            continue;
                        }
                        Ok(expr) => expr,
                    };
                    let consequent_expr_type = self.exprs.get_type(consequent_expr);

                    if expected_arm_type_id.is_none() && consequent_expr_type != NEVER_TYPE_ID {
                        // We chase down the type because, if its a static, it doesn't really make
                        // sense to expect every arm to evaluate to the same static, but rather to
                        // the static's inner type
                        let chased_consequent_id =
                            self.get_static_family_id_if_static(consequent_expr_type);
                        expected_arm_type_id = Some(chased_consequent_id);
                    }

                    let match_arm = TypedMatchArm {
                        case,
                        condition: MatchingCondition {
                            instrs: instrs.to_slice_trim(&mut self.mem),
                        },
                        consequent_expr,
                    };
                    // An arm over an uninhabited variant can never match: it is
                    // typechecked but not lowered, so codegen never sees its
                    // impossible bindings
                    if !self.pattern_matches_uninhabited(pattern) {
                        typed_arms.push(match_arm);
                    }
                }
            }
        }

        if let Some(err) = first_error {
            return Err(err);
        }

        // Exhaustiveness Checking
        if check_exhaustive {
            self.check_pattern_exhaustiveness(
                subject_type,
                all_unguarded_patterns.as_slice_mut(),
                subject_expr_span,
                false,
            )?
        }
        let fallback_value = match fallback_expr {
            Some(e) => e,
            None => self.synth_crash_call(
                if check_exhaustive {
                    self.ast.idents.b.crash_msg_no_cases_exhaustive
                } else {
                    self.ast.idents.b.crash_msg_no_cases
                },
                match_expr_span,
                ctx.with_no_expected_type(),
            )?,
        };
        let fallback_arm = TypedMatchArm {
            case: None,
            condition: MatchingCondition { instrs: MSlice::empty() },
            consequent_expr: fallback_value,
        };
        typed_arms.push(fallback_arm);
        let scrutinee = if typed_arms.iter().any(|arm| arm.case.is_some()) {
            Some(synth_match_scrutinee(
                self,
                match_subject_variable.variable_expr,
                subject_expr_span,
            ))
        } else {
            None
        };

        // The result type of the match is the type of the first non-never arm, or never
        // They've already been typechecked against each other.
        let match_result_type = typed_arms
            .iter()
            .find_map(|arm| {
                let conseqent_type = self.exprs.get_type(arm.consequent_expr);
                if conseqent_type != NEVER_TYPE_ID { Some(conseqent_type) } else { None }
            })
            .unwrap_or(NEVER_TYPE_ID);
        Ok(self.exprs.add(
            TypedExpr::Match(TypedMatchExpr {
                subject_defn: Some(match_subject_variable.defn_stmt),
                scrutinee,
                arms: typed_arms.to_slice(),
            }),
            match_result_type,
            match_expr_span,
        ))
    }

    pub(super) fn eval_static_match_expr(
        &mut self,
        match_expr_id: ParsedExprId,
        ctx: EvalExprContext,
    ) -> K1Result<TypedExprId> {
        // Our job is to evaluate the conditions statically. That means either compiling the condition
        // chains into static exprs and running them, or just allowing only trivial patterns

        let ParsedExpr::Match(parsed_match) = self.ast.exprs.get(match_expr_id) else { panic!() };
        let parsed_match = *parsed_match;
        let match_target =
            self.execute_static_expr(parsed_match.match_subject, ctx.with_no_expected_type(), &[])?;
        let subject_span = self.ast.exprs.get_span(parsed_match.match_subject);
        let StaticValue::Enum(target_type_id, enum_value) = *self.static_values.get(match_target)
        else {
            kbail!(self, subject_span, "Only enums are supported in static match for now");
        };
        let enum_members = self.types.get(target_type_id).expect_enum().member_values;
        let Some(target_member) =
            self.mem.getn(enum_members).iter().find(|m| m.int_value == enum_value)
        else {
            self.ice_span(subject_span, "Tag didn't match any variants")
        };
        let target_member_name = target_member.name;

        let mut given_cases = self.tmp.new_list(enum_members.len());
        let mut uncovered_members = self.tmp.new_list(enum_members.len());
        uncovered_members.extend_iter(self.mem.getn(enum_members).iter().map(|m| m.name));
        for case in self.ast.mem.getn(parsed_match.cases) {
            if let Some(guard_expr) = case.guard_condition_expr {
                kbail!(
                    self,
                    self.ast.exprs.get_span(guard_expr),
                    "Guard conditions are not supported in static match for now"
                );
            }
            for pattern_id in case.patterns.as_slice(&self.ast.mem) {
                let compiled_pattern_id =
                    self.compile_pattern_to_type(*pattern_id, target_type_id, ctx.scope_id, false)?;
                let pattern = self.patterns.get(compiled_pattern_id);
                let TypedPattern::Enum(enum_pattern) = pattern else {
                    kbail!(
                        self,
                        self.ast.get_pattern_span(*pattern_id),
                        "Only enum patterns are supported in static match for now"
                    );
                };
                uncovered_members.swap_remove_elem(&enum_pattern.member_name);
                given_cases.push_grow(&mut self.tmp, (*enum_pattern, case.expression));
            }
        }

        if !uncovered_members.is_empty() {
            let mut uncovered_member_names = String::new();
            for (idx, name) in uncovered_members.iter().enumerate() {
                if idx > 0 {
                    uncovered_member_names.push_str(", ");
                }
                uncovered_member_names.push_str(self.ident_str(*name));
            }
            kbail!(
                self,
                parsed_match.span,
                "Non-exhaustive static match: the following variants were not covered: {}",
                uncovered_member_names
            );
        }

        let mut matched = None;
        for (pattern, expr) in given_cases.iter() {
            if pattern.member_name == target_member_name {
                matched = Some(*expr);
            }
        }

        match matched {
            None => Err(kerr!(self, parsed_match.span, "No cases matched")),
            Some(expr) => self.eval_expr(expr, ctx),
        }
    }

    /// A pattern over an uninhabited variant (e.g. `:err e` on result[t, never])
    /// kills no ctors, but is not useless: generic code must write the arm
    pub(super) fn pattern_matches_uninhabited(&self, pattern_id: TypedPatternId) -> bool {
        match self.patterns.get(pattern_id) {
            TypedPattern::Sum(sp) => {
                let payload_is_never = match self.types.get(sp.sum_type_id) {
                    Type::Sum(sum_type) => self
                        .mem
                        .getn(sum_type.variants)
                        .get(sp.variant_index as usize)
                        .and_then(|v| v.payload)
                        .is_some_and(|p| p == NEVER_TYPE_ID),
                    _ => false,
                };
                payload_is_never || sp.payload.is_some_and(|p| self.pattern_matches_uninhabited(p))
            }
            TypedPattern::Struct(stp) => self
                .patterns
                .mem
                .getn(stp.fields)
                .iter()
                .any(|f| self.pattern_matches_uninhabited(f.pattern)),
            TypedPattern::Reference(refer) => self.pattern_matches_uninhabited(refer.inner_pattern),
            _ => false,
        }
    }

    /// Accumulates a list of 'MatchingConditionInstr' while 'compiling' a pattern match.
    /// Basically, every part of a pattern match boils down to either
    /// - A boolean condition to be evaluated
    /// - A new variable binding
    pub(super) fn compile_pattern_into_values(
        &mut self,
        pattern_id: TypedPatternId,
        target_expr: TypedExprId,
        instrs: &mut List<MatchingConditionInstr, TypedProgram>,
        is_immediately_inside_reference_pattern: bool,
        hoist_case: bool,
        ctx: EvalExprContext,
    ) -> K1Result<Option<StaticValueId>> {
        let target_expr_type = self.exprs.get_type(target_expr);
        let pat = self.patterns.get(pattern_id);
        match pat {
            TypedPattern::Struct(struct_pattern) => {
                let pattern_fields = struct_pattern.fields;
                let is_referencing = is_immediately_inside_reference_pattern;
                let struct_base = self.synth_dereference_when(target_expr, is_referencing);
                for pattern_field in self.patterns.get_slice(pattern_fields).iter() {
                    let get_struct_field = self.synth_field_access(
                        struct_base,
                        pattern_field.field_index as usize,
                        SpanId::NONE,
                    );
                    let final_field = if is_referencing {
                        // Infallible: when referencing, struct_base is a Deref
                        self.synth_address_of(get_struct_field, SpanId::NONE, false).unwrap()
                    } else {
                        get_struct_field
                    };
                    let var_name = self.build_ident_with(|k1, s| {
                        write!(s, "field_{}", k1.ident_str(pattern_field.name)).unwrap();
                    });
                    let struct_field_variable =
                        self.synth_variable_defn_simple(var_name, final_field, ctx.scope_id);
                    instrs.push_grow(
                        &mut self.mem,
                        MatchingConditionInstr::Binding {
                            let_stmt: struct_field_variable.defn_stmt,
                        },
                    );
                    self.compile_pattern_into_values(
                        pattern_field.pattern,
                        struct_field_variable.variable_expr,
                        instrs,
                        is_referencing,
                        false,
                        ctx,
                    )?;
                }
                Ok(None)
            }
            TypedPattern::Sum(sum_pattern) => {
                let sum_pattern = *sum_pattern;
                let is_referencing = is_immediately_inside_reference_pattern;
                let sum_base = self.synth_dereference_when(target_expr, is_referencing);
                let sum_type = self.types.get(sum_pattern.sum_type_id).expect_sum();
                let variant =
                    self.sum_variant_by_index(sum_type.variants, sum_pattern.variant_index);
                let variant_name = variant.name;
                let variant_index = variant.index;
                let variant_payload = variant.payload;
                let case = self.static_values.add_int(variant.tag_value);
                if !hoist_case {
                    let subject = self.synth_sum_get_tag(sum_base, sum_pattern.span);
                    instrs.push_grow(
                        &mut self.mem,
                        MatchingConditionInstr::IntEquals { subject, value: case },
                    );
                }

                if let Some(payload_pattern) = sum_pattern.payload {
                    let Some(payload_type_id) = variant_payload else {
                        kbail!(
                            self,
                            sum_pattern.span,
                            "Impossible pattern: Variant '{}' does not have data",
                            variant_name
                        );
                    };
                    let get_payload_expr = self.exprs.add(
                        TypedExpr::SumGetPayload(GetSumPayload {
                            sum_expr: sum_base,
                            variant_index,
                            packed: self.is_place_in_packed(sum_base),
                        }),
                        payload_type_id,
                        sum_pattern.span,
                    );
                    let final_payload_expr = if is_referencing {
                        // Infallible: when referencing, sum_base is a Deref
                        self.synth_address_of(get_payload_expr, SpanId::NONE, false).unwrap()
                    } else {
                        get_payload_expr
                    };
                    let payload_variable = self.synth_variable_defn_simple(
                        variant_name,
                        final_payload_expr,
                        ctx.scope_id,
                    );
                    instrs.push_grow(
                        &mut self.mem,
                        MatchingConditionInstr::Binding { let_stmt: payload_variable.defn_stmt },
                    );
                    self.compile_pattern_into_values(
                        payload_pattern,
                        payload_variable.variable_expr,
                        instrs,
                        is_referencing,
                        false,
                        ctx,
                    )?;
                };
                Ok(if hoist_case { Some(case) } else { None })
            }
            TypedPattern::Variable(variable_pattern) => {
                let variable_ident = variable_pattern.name;
                let binding_variable = self.synth_variable_defn_visible(
                    variable_ident,
                    target_expr,
                    ctx.scope_id,
                    variable_pattern.span,
                );
                instrs.push_grow(
                    &mut self.mem,
                    MatchingConditionInstr::Binding { let_stmt: binding_variable.defn_stmt },
                );
                Ok(None)
            }
            TypedPattern::Wildcard(_span) => Ok(None),
            TypedPattern::Reference(reference_pattern) => {
                let inner_pattern = reference_pattern.inner_pattern;
                let target_expr = if is_immediately_inside_reference_pattern {
                    self.synth_dereference(target_expr)
                } else {
                    target_expr
                };
                self.compile_pattern_into_values(
                    inner_pattern,
                    target_expr,
                    instrs,
                    true,
                    hoist_case,
                    ctx,
                )
            }
            TypedPattern::RefNull(_inner_type, span) => {
                let span = *span;
                let target_expr_as_ptr = self.synth_cast(
                    target_expr,
                    POINTER_TYPE_ID,
                    CastType::ReferenceToPointer,
                    Some(span),
                );
                let ptr_null_expr =
                    self.add_static_constant_expr(self.static_values.nullptr_id(), span);
                let is_null_expr =
                    self.synth_equals_call_simple(target_expr_as_ptr, ptr_null_expr, span);
                instrs.push_grow(&mut self.mem, MatchingConditionInstr::cond(is_null_expr));
                Ok(None)
            }
            TypedPattern::PointerNull(span) => {
                let span = *span;
                let ptr_null_expr =
                    self.add_static_constant_expr(self.static_values.nullptr_id(), span);
                let is_null_expr = self.synth_equals_call_simple(target_expr, ptr_null_expr, span);
                instrs.push_grow(&mut self.mem, MatchingConditionInstr::cond(is_null_expr));
                Ok(None)
            }
            TypedPattern::Type(pattern) => {
                // We want to push a cond to instrs representing whether the type matches
                let pattern = *pattern;
                let pattern_did_match = target_expr_type == pattern.type_id;
                debug!(
                    "type {} == {}? {pattern_did_match}",
                    self.type_id_to_string(target_expr_type),
                    self.type_id_to_string(pattern.type_id)
                );
                let cond = self.synth_bool(pattern_did_match, pattern.span);
                instrs.push_grow(&mut self.mem, MatchingConditionInstr::cond(cond));
                let inner_target_expr = if pattern_did_match {
                    // The variable is already of the correct type, so don't do anything at all
                    target_expr
                } else {
                    // The type pattern failed, and the consequent code will never run, but we need
                    // it to typecheck
                    self.synth_phony(pattern.type_id, pattern.span)
                };
                self.compile_pattern_into_values(
                    pattern.inner_pattern,
                    inner_target_expr,
                    instrs,
                    false,
                    false,
                    ctx,
                )?;
                Ok(None)
            }
            literal_pat => {
                match literal_pat {
                    TypedPattern::LiteralChar(_, _) => {}
                    TypedPattern::LiteralInteger(_, _) => {}
                    TypedPattern::LiteralFloat(_, _) => {}
                    TypedPattern::LiteralBool(_, _) => {}
                    TypedPattern::LiteralString(_, _) => {}
                    TypedPattern::Enum(_) => {}
                    _ => unreachable!("all non-literals should be handled by now"),
                };
                let target_expr = if is_immediately_inside_reference_pattern {
                    // Literal patterns don't do anything special for references; they just need to
                    // function on the de-rereferenced target. Whereas structs, sums, even
                    // reference patterns do different and unique things when matching on
                    // references
                    self.synth_dereference(target_expr)
                } else {
                    target_expr
                };
                let literal_pat = *self.patterns.get(pattern_id);
                let case = match literal_pat {
                    TypedPattern::Enum(e) => self.static_values.add_int(e.int_value),
                    TypedPattern::LiteralChar(byte, _) => {
                        self.static_values.add(StaticValue::Char(byte))
                    }
                    TypedPattern::LiteralInteger(int_value, _) => int_value,
                    TypedPattern::LiteralBool(bool_value, _) => {
                        self.static_values.add(StaticValue::Bool(bool_value))
                    }
                    TypedPattern::LiteralFloat(float_value, span) => {
                        let pattern_float_literal =
                            self.add_static_constant_expr(float_value, span);
                        let equals_pattern_float =
                            self.synth_equals_call_simple(target_expr, pattern_float_literal, span);
                        instrs.push_grow(
                            &mut self.mem,
                            MatchingConditionInstr::cond(equals_pattern_float),
                        );
                        return Ok(None);
                    }
                    TypedPattern::LiteralString(string_id, span) => {
                        let string_expr = self.synth_string_literal(string_id, span);
                        let condition =
                            self.synth_equals_call_simple(target_expr, string_expr, span);
                        instrs.push_grow(&mut self.mem, MatchingConditionInstr::cond(condition));
                        return Ok(None);
                    }
                    _ => {
                        unreachable!(
                            "should only be literal patterns from here: {}",
                            self.pattern_to_string(pattern_id)
                        )
                    }
                };
                if hoist_case {
                    return Ok(Some(case));
                }
                let subject = match literal_pat {
                    TypedPattern::Enum(e) => self.synth_enum_get_value(target_expr, e.span),
                    _ => target_expr,
                };
                instrs.push_grow(
                    &mut self.mem,
                    MatchingConditionInstr::IntEquals { subject, value: case },
                );
                Ok(None)
            }
        }
    }
}
