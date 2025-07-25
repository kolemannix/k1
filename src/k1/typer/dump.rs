use std::fmt::{Display, Formatter, Write};

use super::*;

impl Display for TypedProgram {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let skip_variables = true;
        f.write_str("Module ")?;
        f.write_str(&self.ast.name)?;
        f.write_str("\n")?;
        self.dump_types(f)?;
        f.write_str("--- Namespaces ---\n")?;
        for (id, namespace) in self.namespaces.iter().enumerate() {
            write!(f, "ns {:02} ", id)?;
            f.write_str(self.ident_str(namespace.name))?;
            f.write_str("\n")?;
        }
        if !skip_variables {
            f.write_str("--- Variables ---\n")?;
            for variable_id in self.variables.iter_ids() {
                write!(f, "var {:02} ", variable_id)?;
                let variable = self.variables.get(variable_id);
                self.display_variable(variable, f)?;
                f.write_str("\n")?;
            }
        }
        f.write_str("--- Functions ---\n")?;
        for (id, func) in self.function_iter() {
            write!(f, "fn {:02} ", id.0)?;
            self.display_function(func, f, false)?;
            f.write_str("\n")?;
        }
        f.write_str("--- Ability Impls ---\n")?;
        self.dump_ability_impls(f)?;
        f.write_str("--- Scopes ---\n")?;
        for (id, scope) in self.scopes.iter() {
            write!(f, "scope {:02} ", id)?;
            self.display_scope(scope, f)?;
            f.write_str("\n")?;
        }
        Ok(())
    }
}

/// Dumping impl
impl TypedProgram {
    pub fn scope_id_to_string(&self, scope_id: ScopeId) -> String {
        let mut s = String::new();
        self.display_scope(self.scopes.get_scope(scope_id), &mut s).unwrap();
        s
    }

    pub fn display_scope(&self, scope: &Scope, writ: &mut impl Write) -> std::fmt::Result {
        let scope_name = self.scopes.make_scope_name(scope, &self.ast.idents);
        let parent = scope
            .parent
            .map(|p| self.scopes.make_scope_name(self.scopes.get_scope(p), &self.ast.idents));
        writeln!(
            writ,
            "{} {} (parent: {})",
            scope_name,
            scope.scope_type.short_name(),
            parent.unwrap_or("_ROOT_".to_string())
        )?;

        if !scope.variables.is_empty() {
            writ.write_str("\tVARS\n")?;
        }
        for (id, variable_in_scope) in scope.variables.iter() {
            write!(writ, "\t{} -> ", self.ident_str(*id))?;
            match variable_in_scope {
                VariableInScope::Masked => writ.write_str("masked")?,
                VariableInScope::Defined(variable_id) => {
                    let variable = self.variables.get(*variable_id);
                    self.display_variable(variable, writ)?;
                }
            }
            writ.write_str("\n")?;
        }
        if !scope.functions.is_empty() {
            writ.write_str("\tFUNCTIONS\n")?;
        }
        for (name, function_id) in scope.functions.iter() {
            let function = self.get_function(*function_id);
            writ.write_str("\t")?;
            self.write_ident(writ, *name)?;
            writ.write_str(" -> ")?;
            self.display_function(function, writ, false)?;
            writ.write_str("\n")?;
        }
        if !scope.types.is_empty() {
            writ.write_str("\tTYPES\n")?;
        }
        for (ident, type_id) in scope.types.iter() {
            writ.write_str("\t")?;
            self.write_ident(writ, *ident)?;
            writ.write_str(" -> ")?;
            self.display_type_id(*type_id, true, writ)?;
            writ.write_str("\n")?;
        }
        if !scope.namespaces.is_empty() {
            writ.write_str("\tNAMESPACES\n")?;
        }
        for (_name, namespace_id) in scope.namespaces.iter() {
            write!(writ, "{} -> ", namespace_id)?;
            let namespace = self.namespaces.get(*namespace_id);
            writ.write_str(self.ident_str(namespace.name))?;
            writ.write_str("\n")?;
        }
        Ok(())
    }

    fn display_variable(&self, var: &Variable, writ: &mut impl Write) -> std::fmt::Result {
        if var.is_mutable {
            writ.write_str("mut ")?;
        }
        writ.write_str(self.ident_str(var.name))?;
        writ.write_str(": ")?;
        self.display_type_id(var.type_id, false, writ)
    }

    pub fn display_type_id(
        &self,
        ty: TypeId,
        expand: bool,
        writ: &mut impl Write,
    ) -> std::fmt::Result {
        self.display_type_ext(ty, expand, writ)
    }

    // Silly function but so commonly needed its worth the call-site ergonomics
    pub fn type_id_option_to_string(&self, type_id: Option<TypeId>) -> String {
        type_id.map(|t| self.type_id_to_string(t)).unwrap_or("<no type>".to_string())
    }

    pub fn type_id_to_string(&self, type_id: TypeId) -> String {
        self.type_id_to_string_ext(type_id, false)
    }

    pub fn dump_type_id_to_string(&self, type_id: TypeId) -> String {
        let mut s = String::new();
        self.dump_type(&mut s, type_id).unwrap();
        s
    }

    pub fn type_kind_to_string(&self, type_id: TypeId) -> &'static str {
        let ty = self.types.get_no_follow(type_id);
        ty.kind_name()
    }

    pub fn type_id_to_string_ext(&self, type_id: TypeId, expand: bool) -> String {
        let mut s = String::with_capacity(1028);
        self.display_type_ext(type_id, expand, &mut s).unwrap();
        s
    }

    fn display_instance_info(
        &self,
        writ: &mut impl Write,
        spec_info: &GenericInstanceInfo,
        expand: bool,
    ) -> std::fmt::Result {
        writ.write_str("[")?;
        for (index, t) in spec_info.type_args.iter().enumerate() {
            self.display_type_id(*t, expand, writ)?;
            let last = index == spec_info.type_args.len() - 1;
            if !last {
                writ.write_str(", ")?;
            }
        }
        writ.write_str("]")?;
        Ok(())
    }

    fn display_type_ext(&self, ty: TypeId, expand: bool, w: &mut impl Write) -> std::fmt::Result {
        let defn_info = self.types.get_defn_info(ty);
        match self.types.get_no_follow(ty) {
            Type::Unit => w.write_str("unit"),
            Type::Char => w.write_str("char"),
            Type::Integer(int_type) => {
                write!(w, "{}", int_type)?;
                Ok(())
            }
            Type::Float(float_type) => match float_type {
                FloatType::F32 => write!(w, "f32"),
                FloatType::F64 => write!(w, "f64"),
            },
            Type::Bool => w.write_str("bool"),
            Type::Pointer => w.write_str("Pointer"),
            Type::Struct(struc) => {
                if let Some(defn_info) = defn_info {
                    w.write_str(self.ident_str(defn_info.name))?;
                    if let Some(spec_info) = struc.generic_instance_info.as_ref() {
                        self.display_instance_info(w, spec_info, expand)?;
                    }
                    if expand {
                        w.write_str("(")?;
                        self.display_struct_fields(w, struc, expand)?;
                        w.write_str(")")?;
                    }
                } else {
                    self.display_struct_fields(w, struc, expand)?;
                }
                Ok(())
            }
            Type::TypeParameter(tv) => {
                if expand {
                    let scope_name = self
                        .scopes
                        .make_scope_name(self.scopes.get_scope(tv.scope_id), &self.ast.idents);
                    w.write_str(&scope_name)?;
                    w.write_str(".")?;
                    w.write_str("'")?;
                    w.write_str(self.ident_str(tv.name))?;
                } else {
                    w.write_str(self.ident_str(tv.name))?;
                }
                Ok(())
            }
            Type::FunctionTypeParameter(ftp) => {
                w.write_str("some ")?;
                self.display_type_id(ftp.function_type, expand, w)?;
                Ok(())
            }
            Type::InferenceHole(hole) => {
                w.write_str("'")?;
                write!(w, "{}", hole.index)?;
                Ok(())
            }
            Type::Reference(r) => {
                self.display_type_id(r.inner_type, expand, w)?;
                w.write_char('*')
            }
            Type::Enum(e) => {
                if let Some(defn_info) = defn_info {
                    w.write_str(self.ident_str(defn_info.name))?;
                    if let Some(spec_info) = e.generic_instance_info.as_ref() {
                        self.display_instance_info(w, spec_info, expand)?;
                    }
                    if expand {
                        w.write_str("(")?;
                    }
                }
                let is_named = defn_info.is_some();
                if !is_named || expand {
                    w.write_str("enum ")?;
                    for (idx, v) in e.variants.iter().enumerate() {
                        w.write_str(self.ast.idents.get_name(v.name))?;
                        if let Some(payload) = &v.payload {
                            w.write_str("(")?;
                            self.display_type_id(*payload, expand, w)?;
                            w.write_str(")")?;
                        }
                        let last = idx == e.variants.len() - 1;
                        if !last {
                            w.write_str(" | ")?;
                        }
                    }
                    if is_named {
                        w.write_str(")")?;
                    }
                }
                Ok(())
            }
            Type::EnumVariant(ev) => {
                let e = self.types.get(ev.enum_type_id).expect_enum();
                if let Some(defn_info) = defn_info {
                    self.write_ident(w, defn_info.name)?;
                    if let Some(spec_info) = e.generic_instance_info.as_ref() {
                        self.display_instance_info(w, spec_info, expand)?;
                    }
                }
                w.write_str(".")?;
                self.write_ident(w, ev.name)?;
                if let Some(payload) = &ev.payload {
                    w.write_str("(")?;
                    self.display_type_id(*payload, expand, w)?;
                    w.write_str(")")?;
                }
                Ok(())
            }
            Type::Never => w.write_str("never"),
            Type::Generic(generic) => {
                let defn_info = defn_info.unwrap();
                w.write_str(self.ident_str(defn_info.name))?;
                w.write_str("[")?;
                for (idx, param) in self.named_types.get_slice(generic.params).iter().enumerate() {
                    w.write_str(self.ident_str(param.name))?;
                    let last = idx == generic.params.len() - 1;
                    if !last {
                        w.write_str(", ")?;
                    }
                }
                w.write_str("]")?;
                if expand {
                    w.write_str("(")?;
                    self.display_type_id(generic.inner, expand, w)?;
                    w.write_str(")")?;
                }
                Ok(())
            }
            Type::Function(fun) => {
                w.write_str("\\")?;
                w.write_str("(")?;
                for (idx, param) in fun.physical_params.iter().enumerate() {
                    if param.is_lambda_env {
                        w.write_str("(env)")?;
                    }
                    self.display_type_id(param.type_id, expand, w)?;
                    let last = idx == fun.physical_params.len() - 1;
                    if !last {
                        w.write_str(", ")?;
                    }
                }
                w.write_str(") -> ")?;
                self.display_type_id(fun.return_type, expand, w)
            }
            Type::Lambda(lam) => {
                write!(w, "lambda#{}(", lam.parsed_id)?;
                self.display_type_id(lam.function_type, expand, w)?;
                w.write_str(")")?;
                Ok(())
            }
            Type::LambdaObject(lambda_object) => {
                w.write_str("lambda_object(")?;
                self.display_type_id(lambda_object.struct_representation, expand, w)?;
                w.write_str(")")?;
                Ok(())
            }
            Type::Static(stat) => {
                w.write_str("static[")?;
                self.display_type_id(stat.inner_type_id, expand, w)?;
                w.write_str(", ")?;
                if let Some(value_id) = stat.value_id {
                    self.display_static_value(w, value_id)?;
                } else {
                    w.write_str("<no value>")?;
                }
                w.write_str("]")?;
                Ok(())
            }
            Type::Unresolved(_u) => w.write_str("<unresolved>"),
            Type::RecursiveReference(rr) => {
                w.write_str("recurse~>(")?;
                let info = self.types.get_defn_info(rr.root_type_id).unwrap();
                w.write_str(self.ident_str(info.name))?;
                if let Type::Generic(generic) = self.types.get(rr.root_type_id) {
                    w.write_str("[")?;
                    for (idx, param) in
                        self.named_types.get_slice(generic.params).iter().enumerate()
                    {
                        w.write_str(self.ident_str(param.name))?;
                        let last = idx == generic.params.len() - 1;
                        if !last {
                            w.write_str(", ")?;
                        }
                    }
                    w.write_str("]")?;
                }
                w.write_str(")")?;
                Ok(())
            }
            Type::Array(array_type) => {
                w.write_str("Array[")?;
                write!(w, "{}", array_type.size)?;
                w.write_str(" x ")?;
                self.display_type_ext(array_type.element_type, expand, w)?;
                w.write_str("]")
            }
        }
    }

    fn display_struct_fields(
        &self,
        writ: &mut impl Write,
        struc: &StructType,
        expand: bool,
    ) -> std::fmt::Result {
        writ.write_str("{ ")?;
        for (index, field) in struc.fields.iter().enumerate() {
            if index > 0 {
                writ.write_str(", ")?;
            }
            writ.write_str(self.ast.idents.get_name(field.name))?;
            writ.write_str(": ")?;
            self.display_type_id(field.type_id, expand, writ)?;
        }
        writ.write_str(" }")
    }

    pub fn function_to_string(&self, function: &TypedFunction, display_block: bool) -> String {
        let mut string = String::new();
        self.display_function(function, &mut string, display_block).unwrap();
        string
    }

    pub fn display_function(
        &self,
        function: &TypedFunction,
        w: &mut impl Write,
        display_block: bool,
    ) -> std::fmt::Result {
        if matches!(function.linkage, Linkage::External(_)) {
            w.write_str("extern ")?;
        }
        if function.linkage == Linkage::Intrinsic {
            w.write_str("intern ")?;
        }

        w.write_str("fn ")?;
        self.display_function_signature(w, function.signature())?;
        if display_block {
            w.write_str(" ")?;
            if let Some(block) = &function.body_block {
                self.display_expr_id(*block, w, 0)?;
            } else {
                w.write_str("{no_block}")?;
            }
        }
        Ok(())
    }

    pub fn block_to_string(&self, block: &TypedBlock) -> String {
        let mut s = String::new();
        self.display_block(block, &mut s, 0).unwrap();
        s
    }

    fn display_block(
        &self,
        block: &TypedBlock,
        writ: &mut impl Write,
        indentation: usize,
    ) -> std::fmt::Result {
        if block.statements.len() == 1 {
            if let TypedStmt::Expr(expr, _) = self.stmts.get(block.statements[0]) {
                return self.display_expr_id(*expr, writ, indentation);
            }
        }
        writ.write_str("{\n")?;
        for (idx, stmt) in block.statements.iter().enumerate() {
            self.display_stmt(*stmt, writ, indentation + 1)?;
            if idx < block.statements.len() - 1 {
                writ.write_str(";")?;
            }
            writ.write_str("\n")?;
        }
        writ.write_str(&"  ".repeat(indentation))?;
        writ.write_str("}: ")?;
        self.display_type_id(block.expr_type, false, writ)
    }

    pub fn stmt_to_string(&self, stmt: TypedStmtId) -> String {
        let mut s = String::with_capacity(256);
        self.display_stmt(stmt, &mut s, 0).unwrap();
        s
    }

    fn display_stmt(
        &self,
        stmt: TypedStmtId,
        writ: &mut impl Write,
        indentation: usize,
    ) -> std::fmt::Result {
        writ.write_str(&"  ".repeat(indentation))?;
        match self.stmts.get(stmt) {
            TypedStmt::Expr(expr, _) => self.display_expr_id(*expr, writ, indentation),
            TypedStmt::Let(let_stmt) => {
                if let_stmt.is_referencing {
                    writ.write_str("let* ")?;
                } else {
                    writ.write_str("let ")?;
                }
                self.display_variable(self.variables.get(let_stmt.variable_id), writ)?;
                writ.write_str(" = ")?;
                self.display_expr_id(let_stmt.initializer, writ, indentation)
            }
            TypedStmt::Assignment(assignment) => {
                self.display_expr_id(assignment.destination, writ, indentation)?;
                match assignment.kind {
                    AssignmentKind::Value => writ.write_str(" = ")?,
                    AssignmentKind::Reference => writ.write_str(" <- ")?,
                }
                self.display_expr_id(assignment.value, writ, indentation)
            }
            TypedStmt::Require(require_stmt) => {
                writ.write_str("require: ")?;
                self.display_matching_condition(writ, &require_stmt.condition, indentation)?;
                writ.write_str(" else ")?;
                self.display_expr_id(require_stmt.else_body, writ, indentation)?;
                Ok(())
            }
        }
    }

    pub fn expr_to_string(&self, expr: TypedExprId) -> String {
        let mut s = String::new();
        self.display_expr_id(expr, &mut s, 0).unwrap();
        s
    }

    pub fn expr_to_string_with_type(&self, expr: TypedExprId) -> String {
        let mut s = String::new();
        self.display_expr_id(expr, &mut s, 0).unwrap();
        s.push_str(": ");
        self.display_type_id(self.exprs.get(expr).get_type(), false, &mut s).unwrap();
        s
    }

    pub fn display_expr_id(
        &self,
        expr_id: TypedExprId,
        writ: &mut impl Write,
        indentation: usize,
    ) -> std::fmt::Result {
        self.display_expr(self.exprs.get(expr_id), writ, indentation)
    }

    pub fn display_expr(
        &self,
        expr: &TypedExpr,
        w: &mut impl Write,
        indentation: usize,
    ) -> std::fmt::Result {
        match expr {
            TypedExpr::Unit(_) => w.write_str("()"),
            TypedExpr::Char(c, _) => write!(w, "'{}'", *c as char),
            TypedExpr::Integer(int) => write!(w, "{}", int.value),
            TypedExpr::Float(float) => write!(w, "{}", float.value),
            TypedExpr::Bool(b, _) => write!(w, "{}", b),
            TypedExpr::String(s, _) => {
                write!(w, "\"{}\"", self.get_string(*s))?;
                Ok(())
            }
            TypedExpr::Struct(struc) => {
                w.write_str("{\n")?;
                for (idx, field) in struc.fields.iter().enumerate() {
                    if idx > 0 {
                        w.write_str(",\n")?;
                    }
                    w.write_str(&"  ".repeat(indentation + 1))?;
                    w.write_str(self.ident_str(field.name))?;
                    w.write_str(": ")?;
                    self.display_expr_id(field.expr, w, indentation)?;
                }
                w.write_str("\n")?;
                w.write_str(&"  ".repeat(indentation))?;
                w.write_str("}")
            }
            TypedExpr::Variable(v) => {
                let variable = self.variables.get(v.variable_id);
                w.write_str(self.ident_str(variable.name))
            }
            TypedExpr::StructFieldAccess(field_access) => {
                self.display_expr_id(field_access.base, w, indentation)?;
                w.write_str(".")?;
                self.write_ident(w, field_access.target_field)?;
                Ok(())
            }
            TypedExpr::Call(fn_call) => {
                match &fn_call.callee {
                    Callee::StaticLambda { function_id, .. } => {
                        let name = self.get_function(*function_id).name;
                        self.write_ident(w, name)?;
                    }
                    Callee::StaticFunction(function_id) => {
                        let name = self.get_function(*function_id).name;
                        self.write_ident(w, name)?;
                    }
                    Callee::Abstract { .. } => {
                        w.write_str("<abstract>")?;
                    }
                    Callee::DynamicFunction { function_reference_expr } => {
                        self.display_expr_id(*function_reference_expr, w, indentation)?;
                    }
                    Callee::DynamicLambda(callee_expr) => {
                        self.display_expr_id(*callee_expr, w, indentation)?;
                    }
                    Callee::DynamicAbstract { variable_id, .. } => {
                        let variable = self.variables.get(*variable_id);
                        self.write_ident(w, variable.name)?;
                    }
                };
                w.write_str("(")?;
                for (idx, arg) in fn_call.args.iter().enumerate() {
                    if idx > 0 {
                        w.write_str(", ")?;
                    }
                    self.display_expr_id(*arg, w, indentation)?;
                }
                w.write_str(")")
            }
            TypedExpr::Block(block) => self.display_block(block, w, indentation),
            TypedExpr::Match(typed_match) => {
                for stmt in &typed_match.initial_let_statements {
                    self.display_stmt(*stmt, w, indentation)?;
                }
                w.write_str("switch {\n")?;
                for (idx, case) in typed_match.arms.iter().enumerate() {
                    w.write_str(&"  ".repeat(indentation + 1))?;
                    writeln!(w, "ARM {idx:02}").unwrap();
                    self.display_matching_condition(w, &case.condition, indentation)?;

                    w.write_str(&"  ".repeat(indentation + 2))?;
                    w.write_str("-> ")?;
                    self.display_expr_id(case.consequent_expr, w, indentation)?;
                    if idx < typed_match.arms.len() - 1 {
                        w.write_str("\n")?;
                    }
                }
                w.write_str("\n")?;
                w.write_str(&"  ".repeat(indentation))?;
                w.write_str("}")
            }
            TypedExpr::WhileLoop(while_loop) => {
                w.write_str("while ")?;
                self.display_matching_condition(w, &while_loop.condition_block, indentation)?;
                w.write_str(" ")?;
                self.display_expr_id(while_loop.body, w, indentation)
            }
            TypedExpr::LoopExpr(loop_expr) => {
                w.write_str("loop ")?;
                let TypedExpr::Block(body_block) = self.exprs.get(loop_expr.body_block) else {
                    unreachable!()
                };
                self.display_block(body_block, w, indentation)
            }
            TypedExpr::UnaryOp(unary_op) => match unary_op.kind {
                UnaryOpKind::Dereference => {
                    self.display_expr_id(unary_op.expr, w, indentation)?;
                    w.write_str(".*")
                }
            },
            TypedExpr::BinaryOp(binary_op) => {
                self.display_expr_id(binary_op.lhs, w, indentation)?;
                write!(w, " {} ", binary_op.kind)?;
                self.display_expr_id(binary_op.rhs, w, indentation)
            }
            TypedExpr::EnumConstructor(enum_constr) => {
                w.write_str(".")?;
                let variant = self.types.get(enum_constr.variant_type_id).expect_enum_variant();
                w.write_str(self.ident_str(variant.name))?;
                if let Some(payload) = &enum_constr.payload {
                    w.write_str("(")?;
                    self.display_expr_id(*payload, w, indentation)?;
                    w.write_str(")")?;
                }
                Ok(())
            }
            TypedExpr::EnumIsVariant(is_variant_expr) => {
                self.display_expr_id(is_variant_expr.enum_expr, w, indentation)?;
                w.write_str(".is[.")?;
                let variant = self
                    .get_expr_type(is_variant_expr.enum_expr)
                    .expect_enum()
                    .variant_by_index(is_variant_expr.variant_index);
                self.write_ident(w, variant.name)?;
                w.write_str("]()")
            }
            TypedExpr::Cast(cast) => {
                self.display_expr_id(cast.base_expr, w, indentation)?;
                write!(w, " as({}) ", cast.cast_type)?;
                self.display_type_id(cast.target_type_id, false, w)
            }
            TypedExpr::EnumGetTag(get_tag) => {
                self.display_expr_id(get_tag.enum_expr, w, indentation)?;
                w.write_str(".tag")?;
                Ok(())
            }
            TypedExpr::EnumGetPayload(get_payload_expr) => {
                self.display_expr_id(get_payload_expr.enum_variant_expr, w, indentation)?;
                w.write_str(".payload")?;
                if get_payload_expr.is_referencing {
                    w.write_char('*')?;
                }
                w.write_char('[')?;
                let variant = self
                    .types
                    .get_type_dereferenced(
                        self.get_expr_type_id(get_payload_expr.enum_variant_expr),
                    )
                    .expect_enum_variant();
                self.write_ident(w, variant.name)?;
                w.write_char(']')?;
                Ok(())
            }
            TypedExpr::Return(ret) => {
                w.write_str("return(")?;
                self.display_expr_id(ret.value, w, indentation)?;
                w.write_char(')')
            }
            TypedExpr::Break(brk) => {
                w.write_str("break(")?;
                self.display_expr_id(brk.value, w, indentation)?;
                w.write_char(')')
            }
            TypedExpr::Lambda(lambda_expr) => {
                w.write_char('\\')?;
                let lambda_type = self.types.get(lambda_expr.lambda_type).as_lambda().unwrap();
                let fn_type = self.types.get(lambda_type.function_type).as_function().unwrap();
                w.write_str("env=[")?;
                self.display_type_id(lambda_type.env_type, false, w).unwrap();
                w.write_str("]")?;
                for arg in fn_type.logical_params() {
                    w.write_str(self.ident_str(arg.name))?;
                    w.write_str(": ")?;
                    self.display_type_id(arg.type_id, false, w)?;
                }
                w.write_str(" -> ")?;
                let lambda_body =
                    self.get_function(lambda_type.body_function_id).body_block.as_ref().unwrap();
                self.display_expr_id(*lambda_body, w, indentation)?;
                Ok(())
            }
            TypedExpr::FunctionReference(fr) => {
                let fun = self.get_function(fr.function_id);
                w.write_str(self.ident_str(fun.name))?;
                w.write_str(".toRef()")
            }
            TypedExpr::FunctionToLambdaObject(fn2lam) => {
                let fun = self.get_function(fn2lam.function_id);
                w.write_str(self.ident_str(fun.name))?;
                w.write_str(".toDyn()")
            }
            TypedExpr::PendingCapture(pending_capture) => {
                w.write_str("capture(")?;
                let variable = self.variables.get(pending_capture.captured_variable_id);
                w.write_str(self.ident_str(variable.name))?;
                w.write_str(")")?;
                Ok(())
            }
            TypedExpr::StaticValue(id, _, _) => {
                w.write_str("#static ")?;
                self.display_static_value(w, *id)?;
                Ok(())
            }
        }
    }

    pub fn static_value_to_string(&self, id: StaticValueId) -> String {
        let mut s = String::with_capacity(256);
        self.display_static_value(&mut s, id).unwrap();
        s
    }

    pub fn display_static_value(&self, w: &mut impl Write, id: StaticValueId) -> std::fmt::Result {
        match self.static_values.get(id) {
            StaticValue::Unit => w.write_str("()"),
            StaticValue::Boolean(b) => write!(w, "{}", *b),
            StaticValue::Char(c) => write!(w, "{}", *c as char),
            StaticValue::Int(typed_integer_value) => {
                write!(w, "{}", typed_integer_value)
            }
            StaticValue::Float(typed_float_value) => {
                write!(w, "{}", typed_float_value)
            }
            StaticValue::String(s) => {
                write!(w, "\"{}\"", self.get_string(*s))
            }
            StaticValue::NullPointer => {
                write!(w, "NULL")
            }
            StaticValue::Struct(static_struct) => {
                w.write_str("{ ")?;
                let fields = self.types.get(static_struct.type_id).expect_struct().fields.clone();
                for (idx, (field_value_id, field_type)) in
                    static_struct.fields.iter().zip(fields.iter()).enumerate()
                {
                    if idx > 0 {
                        w.write_str(", ")?;
                    }
                    self.write_ident(w, field_type.name)?;
                    w.write_str(": ")?;
                    self.display_static_value(w, *field_value_id)?;
                }
                w.write_str(" }")
            }
            StaticValue::Enum(static_enum) => {
                let variant_type =
                    self.types.get(static_enum.variant_type_id).expect_enum_variant();
                let enum_defn = self.types.get_defn_info(variant_type.enum_type_id);
                match enum_defn {
                    Some(defn) => {
                        self.write_ident(w, defn.name)?;
                    }
                    None => {}
                };
                write!(w, ".")?;
                self.write_ident(w, variant_type.name)?;
                match static_enum.payload {
                    None => {}
                    Some(payload_id) => {
                        write!(w, "(")?;
                        self.display_static_value(w, payload_id)?;
                        write!(w, ")")?;
                    }
                };
                Ok(())
            }
            StaticValue::Buffer(buf) => {
                write!(w, "[")?;
                for (index, elem) in buf.elements.iter().enumerate() {
                    self.display_static_value(w, *elem)?;
                    let last = index == buf.elements.len() - 1;
                    if !last {
                        write!(w, ", ")?;
                    }
                }
                write!(w, "]")?;
                Ok(())
            }
        }
    }

    fn display_matching_condition(
        &self,
        w: &mut impl Write,
        cond: &MatchingCondition,
        indentation: usize,
    ) -> std::fmt::Result {
        for (idx, pattern) in cond.patterns.iter().enumerate() {
            self.display_pattern(pattern, w)?;
            if idx != cond.patterns.len() - 1 {
                w.write_str(" and ")?;
            }
        }

        for instr in &cond.instrs {
            match instr {
                MatchingConditionInstr::Binding { let_stmt, .. } => {
                    self.display_stmt(*let_stmt, w, indentation + 1)?;
                }
                MatchingConditionInstr::Cond { value } => {
                    w.write_str(&"  ".repeat(indentation + 1))?;
                    w.write_str("cond(")?;
                    self.display_expr_id(*value, w, indentation + 1)?;
                    w.write_str(")")?;
                }
            }
            w.write_str("\n")?;
        }
        Ok(())
    }

    pub fn function_id_to_string(&self, function_id: FunctionId, display_block: bool) -> String {
        let func = self.get_function(function_id);
        let mut s = String::new();
        self.display_function(func, &mut s, display_block).unwrap();
        s
    }

    pub fn display_pattern_ctor(
        &self,
        pattern_ctor: &PatternCtor,
        writ: &mut impl Write,
    ) -> std::fmt::Result {
        match pattern_ctor {
            PatternCtor::Unit => writ.write_str("()"),
            PatternCtor::BoolTrue => writ.write_str("true"),
            PatternCtor::BoolFalse => writ.write_str("false"),
            PatternCtor::Char => writ.write_str("'<char>'"),
            PatternCtor::String => writ.write_str("<string>"),
            PatternCtor::Int => writ.write_str("<int>"),
            PatternCtor::Float => writ.write_str("<float>"),
            PatternCtor::Pointer => writ.write_str("<ptr>"),
            PatternCtor::TypeVariable => writ.write_str("<tvar>"),
            PatternCtor::FunctionReference => writ.write_str("fn*"),
            PatternCtor::Reference(inner) => {
                writ.write_str("*")?;
                self.display_pattern_ctor(inner, writ)
            }
            PatternCtor::Struct { fields } => {
                writ.write_str("{ ")?;
                for (index, (field_name, field_pattern)) in fields.iter().enumerate() {
                    writ.write_str(self.ident_str(*field_name))?;
                    writ.write_str(": ")?;
                    self.display_pattern_ctor(field_pattern, writ)?;
                    let last = index == fields.len() - 1;
                    if !last {
                        writ.write_str(", ")?;
                    } else {
                        writ.write_str(" ")?;
                    }
                }
                writ.write_str("}")?;
                Ok(())
            }
            PatternCtor::Enum { variant_name, inner } => {
                writ.write_str(self.ident_str(*variant_name))?;
                if let Some(payload) = inner.as_ref() {
                    writ.write_str("(")?;
                    self.display_pattern_ctor(payload, writ)?;
                    writ.write_str(")")?;
                };
                Ok(())
            }
        }
    }

    pub fn pattern_ctor_to_string(&self, pattern_ctor: &PatternCtor) -> String {
        let mut s = String::new();
        self.display_pattern_ctor(pattern_ctor, &mut s).unwrap();
        s
    }

    pub fn display_pattern(
        &self,
        pattern: &TypedPattern,
        writ: &mut impl Write,
    ) -> std::fmt::Result {
        match pattern {
            TypedPattern::LiteralUnit(_) => writ.write_str("()"),
            TypedPattern::LiteralChar(value, _) => write!(writ, "{value}"),
            TypedPattern::LiteralInteger(value, _) => write!(writ, "{value}"),
            TypedPattern::LiteralFloat(value, _) => write!(writ, "{value}"),
            TypedPattern::LiteralBool(value, _) => write!(writ, "{value}"),
            TypedPattern::LiteralString(s, _) => write!(writ, "\"{s}\""),
            TypedPattern::Variable(var) => writ.write_str(self.ident_str(var.name)),
            TypedPattern::Wildcard(_) => writ.write_str("_"),
            TypedPattern::Enum(enum_pat) => {
                writ.write_str(self.ident_str(enum_pat.variant_tag_name))?;
                if let Some(payload) = enum_pat.payload.as_ref() {
                    writ.write_str("(")?;
                    self.display_pattern(payload, writ)?;
                    writ.write_str(")")?;
                };
                Ok(())
            }
            TypedPattern::Struct(struct_pat) => {
                writ.write_str("{ ")?;
                for (index, field_pat) in struct_pat.fields.iter().enumerate() {
                    writ.write_str(self.ident_str(field_pat.name))?;
                    writ.write_str(": ")?;
                    self.display_pattern(&field_pat.pattern, writ)?;
                    let last = index == struct_pat.fields.len() - 1;
                    if !last {
                        writ.write_str(", ")?;
                    } else {
                        writ.write_str(" ")?;
                    }
                }
                writ.write_str("}")?;
                Ok(())
            }
            TypedPattern::Reference(reference_pattern) => {
                self.display_pattern(&reference_pattern.inner_pattern, writ)?;
                writ.write_str("*")?;
                Ok(())
            }
        }
    }

    pub fn display_named_type(&self, w: &mut impl Write, nt: impl NamedType) -> std::fmt::Result {
        write!(w, "{} := {}", self.ident_str(nt.name()), self.type_id_to_string(nt.type_id()))
    }

    pub fn named_type_to_string(&self, nt: impl NamedType) -> String {
        let mut s: String = String::with_capacity(128);
        self.display_named_type(&mut s, nt).unwrap();
        s
    }

    pub fn display_ability_signature(
        &self,
        w: &mut impl Write,
        ability_id: AbilityId,
        impl_arguments: &[NameAndType],
    ) -> std::fmt::Result {
        let ability = self.abilities.get(ability_id);
        self.write_ident(w, ability.name)?;
        if ability.parameters.is_empty() && ability.kind.arguments().is_empty() {
            return Ok(());
        }
        write!(w, "[")?;
        for arg in self.named_types.get_slice(ability.kind.arguments()) {
            self.display_named_type(w, arg)?;
            w.write_str(", ")?;
        }
        for arg in impl_arguments {
            w.write_str("impl ")?;
            self.display_named_type(w, arg)?;
            w.write_str(", ")?;
        }
        write!(w, "]")?;
        Ok(())
    }

    pub fn ability_signature_to_string(&self, sig: TypedAbilitySignature) -> String {
        let mut s = String::new();
        self.display_ability_signature(
            &mut s,
            sig.specialized_ability_id,
            self.named_types.get_slice(sig.impl_arguments),
        )
        .unwrap();
        s
    }

    pub fn ability_impl_signature_to_string(
        &self,
        ability_id: AbilityId,
        impl_arguments: NamedTypeSlice,
    ) -> String {
        let mut s = String::new();
        self.display_ability_signature(
            &mut s,
            ability_id,
            self.named_types.get_slice(impl_arguments),
        )
        .unwrap();
        s
    }

    pub fn display_ability_impl(
        &self,
        w: &mut impl Write,
        id: AbilityImplId,
        display_functions: bool,
    ) -> std::fmt::Result {
        let i = self.ability_impls.get(id);
        let kind_str = match i.kind {
            AbilityImplKind::Concrete => "concrete",
            AbilityImplKind::Blanket { .. } => "blanket",
            AbilityImplKind::DerivedFromBlanket { .. } => "derived",
            AbilityImplKind::VariableConstraint => "constraint",
        };
        write!(w, "{kind_str:10} ")?;
        self.display_ability_signature(
            w,
            i.ability_id,
            self.named_types.get_slice(i.impl_arguments),
        )?;
        write!(w, " for ")?;
        self.display_type_id(i.self_type_id, false, w)?;
        if display_functions {
            w.write_str(" {\n")?;
            for ability_impl_fn in &i.functions {
                w.write_str("\t\t")?;
                match *ability_impl_fn {
                    AbilityImplFunction::FunctionId(ability_impl_fn) => {
                        self.display_function(self.get_function(ability_impl_fn), w, false)?
                    }
                    AbilityImplFunction::Abstract(sig) => {
                        w.write_str("abstract ")?;
                        self.display_type_id(sig.function_type, false, w)?;
                    }
                };
                writeln!(w)?;
            }
        }
        Ok(())
    }

    pub fn pattern_to_string(&self, pattern: &TypedPattern) -> String {
        let mut s = String::new();
        self.display_pattern(pattern, &mut s).unwrap();
        s
    }

    pub fn pretty_print_types(&self, types: &[TypeId], sep: &str) -> String {
        let mut s = String::new();
        let mut first = true;
        for type_id in types.iter() {
            if !first {
                s.push_str(sep)
            }
            write!(s, "{}", self.type_id_to_string_ext(*type_id, true)).unwrap();
            first = false;
        }
        s
    }

    pub fn pretty_print_type_substitutions(
        &self,
        types: &[TypeSubstitutionPair],
        sep: &str,
    ) -> String {
        let mut s = String::new();
        let mut first = true;
        for pair in types.iter() {
            if !first {
                s.push_str(sep)
            }
            write!(
                s,
                "{} -> {}",
                self.type_id_to_string_ext(pair.from, false),
                self.type_id_to_string_ext(pair.to, false)
            )
            .unwrap();
            first = false;
        }
        s
    }

    pub fn pretty_print_named_type_slice(
        &self,
        types: SliceHandle<NameAndTypeId>,
        sep: &str,
    ) -> String {
        self.pretty_print_named_types(self.named_types.get_slice(types), sep)
    }

    pub fn pretty_print_named_types(&self, types: &[impl NamedType], sep: &str) -> String {
        let mut s = String::new();
        let mut first = true;
        for nt in types {
            if !first {
                s.push_str(sep)
            }
            write!(s, "{} := {}", self.ident_str(nt.name()), self.type_id_to_string(nt.type_id()))
                .unwrap();
            first = false;
        }
        s
    }

    pub fn write_ident(&self, w: &mut impl Write, ident: Ident) -> std::fmt::Result {
        w.write_str(self.ident_str(ident))
    }

    pub fn display_namespaced_identifier(
        &self,
        w: &mut impl Write,
        ident: &NamespacedIdentifier,
    ) -> std::fmt::Result {
        for ns in &ident.namespaces {
            self.write_ident(w, *ns)?;
            write!(w, "/")?;
        }
        self.write_ident(w, ident.name)
    }

    pub fn namespaced_identifier_to_string(&self, ident: &NamespacedIdentifier) -> String {
        let mut s = String::with_capacity(32);
        self.display_namespaced_identifier(&mut s, ident).unwrap();
        s
    }

    pub fn function_signature_to_string(&self, fn_signature: FunctionSignature) -> String {
        let mut s = String::new();
        self.display_function_signature(&mut s, fn_signature).unwrap();
        s
    }

    pub fn display_function_signature(
        &self,
        w: &mut impl Write,
        signature: FunctionSignature,
    ) -> std::fmt::Result {
        if let Some(name) = signature.name {
            self.write_ident(w, name)?;
        }
        if signature.is_generic() {
            w.write_char('[')?;
            for (idx, tp) in self.named_types.get_slice(signature.type_params).iter().enumerate() {
                if idx > 0 {
                    w.write_str(", ")?;
                }
                self.write_ident(w, tp.name)?;
            }
            for ftp in self.existential_type_params.get_slice(signature.function_type_params).iter()
            {
                self.write_ident(w, ftp.name)?;
                w.write_str(": ")?;
                self.display_type_id(ftp.type_id, false, w)?;
                w.write_str(", ")?;
            }
            for stp in self.existential_type_params.get_slice(signature.static_type_params).iter() {
                self.write_ident(w, stp.name)?;
                w.write_str(": ")?;
                self.display_type_id(stp.type_id, false, w)?;
                w.write_str(", ")?;
            }
            w.write_char(']')?;
        }
        w.write_char('(')?;
        let function_type = self.types.get(signature.function_type).as_function().unwrap();
        for (idx, param) in function_type.physical_params.iter().enumerate() {
            if idx > 0 {
                w.write_str(", ")?;
            }
            w.write_str(self.ident_str(param.name))?;
            w.write_str(": ")?;
            self.display_type_id(param.type_id, false, w)?;
        }
        w.write_str(")")?;
        w.write_str(": ")?;
        self.display_type_id(function_type.return_type, false, w)?;
        Ok(())
    }

    pub fn dump_types_to_string(&self) -> String {
        let mut s = String::new();
        self.dump_types(&mut s).unwrap();
        s
    }

    pub fn dump_type(&self, w: &mut impl Write, id: TypeId) -> std::fmt::Result {
        write!(w, "type #{:02} {:10} ", id, self.types.get_no_follow(id).kind_name())?;
        let tvar_info = self.types.get_contained_type_variable_counts(id);
        let l = self.types.layouts.get(id);
        let defn_name = self.types.get_defn_info(id).map(|i| self.ident_str(i.name));
        write!(w, "defn_name={} size={} align={} ", defn_name.unwrap_or("-"), l.size, l.align)?;
        write!(
            w,
            "tparams={} holes={}",
            tvar_info.type_parameter_count, tvar_info.inference_variable_count
        )?;
        writeln!(w)?;
        self.display_type_ext(id, true, w)?;
        Ok(())
    }

    pub fn dump_types(&self, w: &mut impl Write) -> std::fmt::Result {
        w.write_str("--- TYPES ---\n")?;
        for id in self.types.iter_ids() {
            writeln!(w)?;
            self.dump_type(w, id)?;
            w.write_str("\n")?;
        }
        Ok(())
    }

    pub fn dump_ability_impls(&self, w: &mut impl Write) -> std::fmt::Result {
        for (self_type_id, impls) in self.ability_impl_table.iter() {
            writeln!(w, "impls for {}", self.type_id_to_string_ext(*self_type_id, true))?;
            for impl_handle in impls {
                w.write_str("\t")?;
                self.display_ability_impl(w, impl_handle.full_impl_id, true)?;
                w.write_str("\n")?;
            }
        }
        Ok(())
    }

    pub fn dump_blanket_impls(&self, w: &mut impl Write) -> std::fmt::Result {
        for (base_ab, impls) in self.blanket_impls.iter() {
            writeln!(
                w,
                "{:02} {}: {}",
                base_ab.0,
                self.ident_str(self.abilities.get(*base_ab).name),
                impls.len()
            )
            .unwrap();
        }
        Ok(())
    }
}
