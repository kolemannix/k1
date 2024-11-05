use std::fmt::{Display, Formatter, Write};

use super::*;

impl Display for TypedModule {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("Module ")?;
        f.write_str(&self.ast.name)?;
        f.write_str("\n")?;
        f.write_str("--- TYPES ---\n")?;
        for (id, ty) in self.types.iter() {
            write!(f, "type {:02} ", id)?;
            self.display_type(ty, true, f)?;
            f.write_str("\n")?;
        }
        f.write_str("--- Namespaces ---\n")?;
        for (id, namespace) in self.namespaces.iter() {
            write!(f, "ns {:02} ", id.0)?;
            f.write_str(self.get_ident_str(namespace.name))?;
            f.write_str("\n")?;
        }
        f.write_str("--- Variables ---\n")?;
        for (id, variable) in self.variables.iter() {
            write!(f, "var {:02} ", id.0)?;
            self.display_variable(variable, f)?;
            f.write_str("\n")?;
        }
        f.write_str("--- Functions ---\n")?;
        for (id, func) in self.function_iter() {
            write!(f, "fn {:02} ", id.0)?;
            self.display_function(func, f, false)?;
            f.write_str("\n")?;
        }
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
impl TypedModule {
    pub fn scope_to_string(&self, scope: &Scope) -> String {
        let mut s = String::new();
        self.display_scope(scope, &mut s).unwrap();
        s
    }

    pub fn display_scope(&self, scope: &Scope, writ: &mut impl Write) -> std::fmt::Result {
        let scope_name = self.scopes.make_scope_name(scope, &self.ast.identifiers);
        writeln!(writ, "{}", scope_name)?;

        for (id, variable_id) in scope.variables.iter() {
            let variable = self.variables.get_variable(*variable_id);
            write!(writ, "\t{} ", id)?;
            self.display_variable(variable, writ)?;
            writ.write_str("\n")?;
        }
        for function_id in scope.functions.values() {
            let function = self.get_function(*function_id);
            writ.write_str("\t")?;
            self.display_function(function, writ, false)?;
            writ.write_str("\n")?;
        }
        if !scope.types.is_empty() {
            writ.write_str("\tTYPES\n")?;
        }
        for (ident, type_id) in scope.types.iter() {
            writ.write_str("\t")?;
            writ.write_str(self.get_ident_str(*ident))?;
            writ.write_str(" := ")?;
            self.display_type_id(*type_id, true, writ)?;
            writ.write_str("\n")?;
        }
        for (id, namespace_id) in scope.namespaces.iter() {
            write!(writ, "{} ", id)?;
            let namespace = self.namespaces.get(*namespace_id);
            writ.write_str(self.get_ident_str(namespace.name))?;
            writ.write_str("\n")?;
        }
        Ok(())
    }

    fn display_variable(&self, var: &Variable, writ: &mut impl Write) -> std::fmt::Result {
        if var.is_mutable {
            writ.write_str("mut ")?;
        }
        writ.write_str(self.get_ident_str(var.name))?;
        writ.write_str(": ")?;
        self.display_type_id(var.type_id, false, writ)
    }

    fn display_type_id(&self, ty: TypeId, expand: bool, writ: &mut impl Write) -> std::fmt::Result {
        let ty = self.types.get_no_follow(ty);
        self.display_type(ty, expand, writ)
    }

    pub fn type_id_to_string(&self, type_id: TypeId) -> String {
        self.type_id_to_string_ext(type_id, false)
    }

    pub fn type_id_to_string_ext(&self, type_id: TypeId, expand: bool) -> String {
        let ty = self.types.get_no_follow(type_id);
        self.type_to_string(ty, expand)
    }

    pub fn type_to_string(&self, ty: &Type, expand: bool) -> String {
        let mut s = String::new();
        self.display_type(ty, expand, &mut s).unwrap();
        s
    }

    fn display_instance_info(
        &self,
        writ: &mut impl Write,
        spec_info: &GenericInstanceInfo,
        expand: bool,
    ) -> std::fmt::Result {
        writ.write_str("[")?;
        for (index, t) in spec_info.param_values.iter().enumerate() {
            self.display_type_id(*t, expand, writ)?;
            let last = index == spec_info.param_values.len() - 1;
            if !last {
                writ.write_str(", ")?;
            }
        }
        writ.write_str("]")?;
        Ok(())
    }

    fn display_type(&self, ty: &Type, expand: bool, writ: &mut impl Write) -> std::fmt::Result {
        match ty {
            Type::Unit(_) => writ.write_str("unit"),
            Type::Char(_) => writ.write_str("char"),
            Type::Integer(int_type) => {
                match int_type {
                    IntegerType::U8 => writ.write_str("u8")?,
                    IntegerType::U16 => writ.write_str("u16")?,
                    IntegerType::U32 => writ.write_str("u32")?,
                    IntegerType::U64 => writ.write_str("u64")?,
                    IntegerType::I8 => writ.write_str("i8")?,
                    IntegerType::I16 => writ.write_str("i16")?,
                    IntegerType::I32 => writ.write_str("i32")?,
                    IntegerType::I64 => writ.write_str("i64")?,
                }
                Ok(())
            }
            Type::Float(float_type) => match float_type.size {
                NumericWidth::B8 => write!(writ, "f8"),
                NumericWidth::B16 => write!(writ, "f16"),
                NumericWidth::B32 => write!(writ, "f32"),
                NumericWidth::B64 => write!(writ, "f64"),
            },
            Type::Bool(_) => writ.write_str("bool"),
            Type::Pointer(_) => writ.write_str("Pointer"),
            Type::Struct(struc) => {
                if let Some(defn_info) = struc.type_defn_info.as_ref() {
                    writ.write_str(self.get_ident_str(defn_info.name))?;
                    if let Some(spec_info) = struc.generic_instance_info.as_ref() {
                        self.display_instance_info(writ, spec_info, expand)?;
                    }
                    if expand {
                        writ.write_str("(")?;
                        self.display_struct_fields(writ, struc, expand)?;
                        writ.write_str(")")?;
                    }
                } else {
                    self.display_struct_fields(writ, struc, expand)?;
                }
                Ok(())
            }
            Type::TypeVariable(tv) => {
                let scope_name = self
                    .scopes
                    .make_scope_name(self.scopes.get_scope(tv.scope_id), &self.ast.identifiers);
                writ.write_str(&scope_name)?;
                writ.write_str(".")?;
                writ.write_str("$")?;
                writ.write_str(self.ast.identifiers.get_name(tv.name))?;
                Ok(())
            }
            Type::Reference(r) => {
                self.display_type_id(r.inner_type, expand, writ)?;
                writ.write_char('*')
            }
            Type::Enum(e) => {
                if let Some(defn_info) = e.type_defn_info.as_ref() {
                    writ.write_str(self.get_ident_str(defn_info.name))?;
                    if let Some(spec_info) = e.generic_instance_info.as_ref() {
                        self.display_instance_info(writ, spec_info, expand)?;
                    }
                    if expand {
                        writ.write_str("(")?;
                    }
                }
                if expand {
                    writ.write_str("enum ")?;
                    for (idx, v) in e.variants.iter().enumerate() {
                        writ.write_str(self.ast.identifiers.get_name(v.name))?;
                        if let Some(payload) = &v.payload {
                            writ.write_str("(")?;
                            self.display_type_id(*payload, expand, writ)?;
                            writ.write_str(")")?;
                        }
                        let last = idx == e.variants.len() - 1;
                        if !last {
                            writ.write_str(" | ")?;
                        }
                    }
                    if let Some(_defn_info) = e.type_defn_info.as_ref() {
                        writ.write_str(")")?;
                    }
                }
                Ok(())
            }
            Type::EnumVariant(ev) => {
                let e = self.types.get(ev.enum_type_id).expect_enum();
                if let Some(defn_info) = e.type_defn_info.as_ref() {
                    writ.write_str(self.get_ident_str(defn_info.name))?;
                    if let Some(spec_info) = e.generic_instance_info.as_ref() {
                        self.display_instance_info(writ, spec_info, expand)?;
                    }
                    writ.write_str(".")?;
                }
                writ.write_str(self.ast.identifiers.get_name(ev.name))?;
                if let Some(payload) = &ev.payload {
                    writ.write_str("(")?;
                    self.display_type_id(*payload, expand, writ)?;
                    writ.write_str(")")?;
                }
                Ok(())
            }
            Type::Never(_) => writ.write_str("never"),
            Type::OpaqueAlias(opaque) => {
                writ.write_str(self.get_ident_str(opaque.type_defn_info.name))?;
                writ.write_str("(")?;
                self.display_type_id(opaque.aliasee, expand, writ)?;
                writ.write_str(")")
            }
            Type::Generic(gen) => {
                writ.write_str(self.get_ident_str(gen.type_defn_info.name))?;
                writ.write_str("[")?;
                for (idx, param) in gen.params.iter().enumerate() {
                    writ.write_str(self.get_ident_str(param.name))?;
                    let last = idx == gen.params.len() - 1;
                    if !last {
                        writ.write_str(", ")?;
                    }
                }
                writ.write_str("]")?;
                if expand {
                    writ.write_str("(")?;
                    self.display_type_id(gen.inner, expand, writ)?;
                    writ.write_str(")")?;
                }
                Ok(())
            }
            Type::Function(fun) => {
                writ.write_str("fn")?;
                writ.write_str("(")?;
                for (idx, param) in fun.params.iter().enumerate() {
                    self.display_type_id(param.type_id, expand, writ)?;
                    let last = idx == fun.params.len() - 1;
                    if !last {
                        writ.write_str(", ")?;
                    }
                }
                writ.write_str(") -> ")?;
                self.display_type_id(fun.return_type, expand, writ)
            }
            Type::Closure(clos) => {
                write!(writ, "closure(")?;
                self.display_type_id(clos.function_type, expand, writ)?;
                writ.write_str(")")?;
                Ok(())
            }
            Type::ClosureObject(closure_object) => {
                writ.write_str("closure_object(")?;
                self.display_type_id(closure_object.function_type, expand, writ)?;
                writ.write_str(")")?;
                Ok(())
            }
            Type::RecursiveReference(rr) => {
                if rr.is_pending() {
                    writ.write_str("pending")
                } else {
                    let info = self.types.get_defn_info(rr.root_type_id).unwrap();
                    writ.write_str(self.get_ident_str(info.name))?;
                    Ok(())
                }
            }
        }
    }

    fn display_struct_fields(
        &self,
        writ: &mut impl Write,
        struc: &StructType,
        expand: bool,
    ) -> std::fmt::Result {
        writ.write_str("{")?;
        for (index, field) in struc.fields.iter().enumerate() {
            if index > 0 {
                writ.write_str(", ")?;
            }
            writ.write_str(self.ast.identifiers.get_name(field.name))?;
            writ.write_str(": ")?;
            self.display_type_id(field.type_id, expand, writ)?;
        }
        writ.write_str("}")
    }

    pub fn function_to_string(&self, function: &TypedFunction, display_block: bool) -> String {
        let mut string = String::new();
        self.display_function(function, &mut string, display_block).unwrap();
        string
    }

    pub fn display_function(
        &self,
        function: &TypedFunction,
        writ: &mut impl Write,
        display_block: bool,
    ) -> std::fmt::Result {
        if matches!(function.linkage, Linkage::External(_)) {
            writ.write_str("extern ")?;
        }
        if function.linkage == Linkage::Intrinsic {
            writ.write_str("intern ")?;
        }

        writ.write_str("fn ")?;
        writ.write_str(self.get_ident_str(function.name))?;
        writ.write_str("(")?;
        let function_type = self.types.get(function.type_id).as_function().unwrap();
        for (idx, param) in function_type.params.iter().enumerate() {
            if idx > 0 {
                writ.write_str(", ")?;
            }
            writ.write_str(self.get_ident_str(param.name))?;
            writ.write_str(": ")?;
            self.display_type_id(param.type_id, false, writ)?;
        }
        writ.write_str(")")?;
        writ.write_str(": ")?;
        self.display_type_id(function_type.return_type, false, writ)?;
        if display_block {
            writ.write_str(" ")?;
            if let Some(block) = &function.body_block {
                self.display_block(block, writ, 0)?;
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
            if let TypedStmt::Expr(expr) = &block.statements[0] {
                return self.display_expr(expr, writ, indentation);
            }
        }
        writ.write_str("{\n")?;
        for (idx, stmt) in block.statements.iter().enumerate() {
            self.display_stmt(stmt, writ, indentation + 1)?;
            if idx < block.statements.len() - 1 {
                writ.write_str(";")?;
            }
            writ.write_str("\n")?;
        }
        writ.write_str(&"  ".repeat(indentation))?;
        writ.write_str("}: ")?;
        self.display_type_id(block.expr_type, false, writ)
    }

    fn display_stmt(
        &self,
        stmt: &TypedStmt,
        writ: &mut impl Write,
        indentation: usize,
    ) -> std::fmt::Result {
        writ.write_str(&"  ".repeat(indentation))?;
        match stmt {
            TypedStmt::Expr(expr) => self.display_expr(expr, writ, indentation),
            TypedStmt::ValDef(val_def) => {
                writ.write_str("val ")?;
                self.display_variable(self.variables.get_variable(val_def.variable_id), writ)?;
                writ.write_str(" = ")?;
                self.display_expr(&val_def.initializer, writ, indentation)
            }
            TypedStmt::Assignment(assignment) => {
                self.display_expr(&assignment.destination, writ, 0)?;
                writ.write_str(" = ")?;
                self.display_expr(&assignment.value, writ, 0)
            }
            TypedStmt::WhileLoop(while_loop) => {
                writ.write_str("while ")?;
                self.display_expr(&while_loop.cond, writ, 0)?;
                writ.write_str(" ")?;
                self.display_block(&while_loop.block, writ, indentation + 1)
            }
        }
    }

    pub fn expr_to_string(&self, expr: &TypedExpr) -> String {
        let mut s = String::new();
        self.display_expr(expr, &mut s, 0).unwrap();
        s
    }

    pub fn display_expr(
        &self,
        expr: &TypedExpr,
        writ: &mut impl Write,
        indentation: usize,
    ) -> std::fmt::Result {
        match expr {
            TypedExpr::Unit(_) => writ.write_str("()"),
            TypedExpr::Char(c, _) => write!(writ, "'{}'", c),
            TypedExpr::Integer(int) => write!(writ, "{}", int.value),
            TypedExpr::Float(float) => write!(writ, "{}", float.value),
            TypedExpr::Bool(b, _) => write!(writ, "{}", b),
            TypedExpr::Str(s, _) => write!(writ, "\"{}\"", s),
            TypedExpr::Struct(struc) => {
                writ.write_str("{\n")?;
                for (idx, field) in struc.fields.iter().enumerate() {
                    if idx > 0 {
                        writ.write_str(",\n")?;
                    }
                    writ.write_str(&"  ".repeat(indentation + 1))?;
                    writ.write_str(self.get_ident_str(field.name))?;
                    writ.write_str(": ")?;
                    self.display_expr(&field.expr, writ, indentation)?;
                }
                writ.write_str("\n")?;
                writ.write_str(&"  ".repeat(indentation))?;
                writ.write_str("}")
            }
            TypedExpr::Variable(v) => {
                let variable = self.variables.get_variable(v.variable_id);
                writ.write_str(self.get_ident_str(variable.name))
            }
            TypedExpr::StructFieldAccess(field_access) => {
                self.display_expr(&field_access.base, writ, indentation)?;
                writ.write_str(".")?;
                writ.write_str(self.get_ident_str(field_access.target_field))
            }
            TypedExpr::Call(fn_call) => {
                match &fn_call.callee {
                    Callee::StaticClosure { function_id, .. } => {
                        let name = self.get_function(*function_id).name;
                        writ.write_str(self.get_ident_str(name))?;
                    }
                    Callee::StaticFunction(function_id) => {
                        let name = self.get_function(*function_id).name;
                        writ.write_str(self.get_ident_str(name))?;
                    }
                    Callee::DynamicFunction(callee_expr) => {
                        self.display_expr(callee_expr, writ, indentation)?;
                    }
                    Callee::DynamicClosure(callee_expr) => {
                        self.display_expr(callee_expr, writ, indentation)?;
                    }
                };
                writ.write_str("(")?;
                for (idx, arg) in fn_call.args.iter().enumerate() {
                    if idx > 0 {
                        writ.write_str(", ")?;
                    }
                    self.display_expr(arg, writ, indentation)?;
                }
                writ.write_str(")")
            }
            TypedExpr::Block(block) => self.display_block(block, writ, indentation),
            TypedExpr::If(if_expr) => {
                writ.write_str("if ")?;
                self.display_expr(&if_expr.condition, writ, indentation)?;
                writ.write_str(" ")?;
                self.display_expr(&if_expr.consequent, writ, indentation)?;
                if !if_expr.alternate.is_unit() {
                    writ.write_str(" else ")?;
                    self.display_expr(&if_expr.alternate, writ, indentation)?;
                }
                Ok(())
            }
            TypedExpr::UnaryOp(unary_op) => {
                writ.write_fmt(format_args!("{}", unary_op.kind))?;
                self.display_expr(&unary_op.expr, writ, indentation)
            }
            TypedExpr::BinaryOp(binary_op) => {
                self.display_expr(&binary_op.lhs, writ, indentation)?;
                write!(writ, " {} ", binary_op.kind)?;
                self.display_expr(&binary_op.rhs, writ, indentation)
            }
            TypedExpr::EnumConstructor(enum_constr) => {
                writ.write_str(".")?;
                writ.write_str(self.get_ident_str(enum_constr.variant_name))?;
                if let Some(payload) = &enum_constr.payload {
                    writ.write_str("(")?;
                    self.display_expr(payload, writ, indentation)?;
                    writ.write_str(")")?;
                }
                Ok(())
            }
            TypedExpr::EnumIsVariant(is_variant_expr) => {
                self.display_expr(&is_variant_expr.target_expr, writ, indentation)?;
                writ.write_str(".is[.")?;
                writ.write_str(self.ast.identifiers.get_name(is_variant_expr.variant_name))?;
                writ.write_str("]()")
            }
            TypedExpr::Cast(cast) => {
                self.display_expr(&cast.base_expr, writ, indentation)?;
                write!(writ, " as({}) ", cast.cast_type)?;
                self.display_type_id(cast.target_type_id, false, writ)
            }
            TypedExpr::EnumGetPayload(as_variant_expr) => {
                self.display_expr(&as_variant_expr.target_expr, writ, indentation)?;
                writ.write_str(".payload")
            }
            TypedExpr::Return(ret) => {
                writ.write_str("return(")?;
                self.display_expr(&ret.value, writ, indentation)?;
                writ.write_char(')')
            }
            TypedExpr::Closure(closure_expr) => {
                writ.write_char('\\')?;
                let closure_type = self.types.get(closure_expr.closure_type).as_closure().unwrap();
                let fn_type = self.types.get(closure_type.function_type).as_function().unwrap();
                for arg in fn_type.params.iter() {
                    writ.write_str(self.get_ident_str(arg.name))?;
                    writ.write_str(": ")?;
                    self.display_type_id(arg.type_id, false, writ)?;
                }
                writ.write_str(" -> ")?;
                let closure_body =
                    self.get_function(closure_type.body_function_id).body_block.as_ref().unwrap();
                self.display_block(closure_body, writ, indentation)?;
                Ok(())
            }
            TypedExpr::FunctionName(fn_name_expr) => {
                let fun = self.get_function(fn_name_expr.function_id);
                writ.write_str(self.get_ident_str(fun.name))
            }
        }
    }

    pub fn function_id_to_string(&self, function_id: FunctionId, display_block: bool) -> String {
        let func = self.get_function(function_id);
        let mut s = String::new();
        self.display_function(func, &mut s, display_block).unwrap();
        s
    }

    pub fn display_pattern_ctor(
        &self,
        pattern_ctor: &PatternConstructor,
        writ: &mut impl Write,
    ) -> std::fmt::Result {
        match pattern_ctor {
            PatternConstructor::Unit => writ.write_str("()"),
            PatternConstructor::BoolTrue => writ.write_str("true"),
            PatternConstructor::BoolFalse => writ.write_str("false"),
            PatternConstructor::Char => writ.write_str("'<char>'"),
            PatternConstructor::String => writ.write_str("\"<string>\""),
            PatternConstructor::Int => writ.write_str("<int>"),
            PatternConstructor::Float => writ.write_str("<float>"),
            PatternConstructor::TypeVariable => writ.write_str("<tvar>"),
            PatternConstructor::Reference(inner) => {
                writ.write_str("*")?;
                self.display_pattern_ctor(inner, writ)
            }
            PatternConstructor::Struct { fields } => {
                writ.write_str("{ ")?;
                for (index, (field_name, field_pattern)) in fields.iter().enumerate() {
                    writ.write_str(self.get_ident_str(*field_name))?;
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
            PatternConstructor::Enum { variant_name, inner } => {
                writ.write_str(self.get_ident_str(*variant_name))?;
                if let Some(payload) = inner.as_ref() {
                    writ.write_str("(")?;
                    self.display_pattern_ctor(payload, writ)?;
                    writ.write_str(")")?;
                };
                Ok(())
            }
        }
    }

    pub fn pattern_ctor_to_string(&self, pattern_ctor: &PatternConstructor) -> String {
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
            TypedPattern::Variable(var) => writ.write_str(self.get_ident_str(var.name)),
            TypedPattern::Wildcard(_) => writ.write_str("_"),
            TypedPattern::Enum(enum_pat) => {
                writ.write_str(self.get_ident_str(enum_pat.variant_tag_name))?;
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
                    writ.write_str(self.get_ident_str(field_pat.name))?;
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
        }
    }

    pub fn pattern_to_string(&self, pattern: &TypedPattern) -> String {
        let mut s = String::new();
        self.display_pattern(pattern, &mut s).unwrap();
        s
    }
}
