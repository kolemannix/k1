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
            self.display_type(ty, f)?;
            f.write_str("\n")?;
        }
        f.write_str("--- Namespaces ---\n")?;
        for (id, namespace) in self.namespaces.iter() {
            write!(f, "ns {:02} ", id.0)?;
            f.write_str(&self.get_ident_str(namespace.name))?;
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
    pub fn make_scope_name(&self, scope: &Scope) -> String {
        let mut name = match scope.name {
            Some(name) => (*self.get_ident_str(name)).to_string(),
            None => scope.scope_type.short_name().to_string(),
        };
        if let Some(p) = scope.parent {
            let parent_scope = self.scopes.get_scope(p);
            name = format!("{}.{}", self.make_scope_name(parent_scope), name);
        }
        name
    }

    pub fn scope_to_string(&self, scope: &Scope) -> String {
        let mut s = String::new();
        self.display_scope(scope, &mut s).unwrap();
        s
    }

    pub fn display_scope(&self, scope: &Scope, writ: &mut impl Write) -> std::fmt::Result {
        let scope_name = self.make_scope_name(scope);
        write!(writ, "{}\n", scope_name)?;

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
            writ.write_str(&self.get_ident_str(*ident))?;
            writ.write_str(" := ")?;
            self.display_type_id(*type_id, writ)?;
            writ.write_str("\n")?;
        }
        for (id, namespace_id) in scope.namespaces.iter() {
            write!(writ, "{} ", id)?;
            let namespace = self.namespaces.get(*namespace_id);
            writ.write_str(&self.get_ident_str(namespace.name))?;
            writ.write_str("\n")?;
        }
        Ok(())
    }

    fn display_variable(&self, var: &Variable, writ: &mut impl Write) -> std::fmt::Result {
        if var.is_mutable {
            writ.write_str("mut ")?;
        }
        writ.write_str(&self.get_ident_str(var.name))?;
        writ.write_str(": ")?;
        self.display_type_id(var.type_id, writ)
    }

    fn display_type_id(&self, ty: TypeId, writ: &mut impl Write) -> std::fmt::Result {
        let ty = self.types.get(ty);
        self.display_type(ty, writ)
    }

    pub fn type_id_to_string(&self, type_id: TypeId) -> String {
        let ty = self.types.get(type_id);
        self.type_to_string(ty)
    }

    pub fn type_to_string(&self, ty: &Type) -> String {
        let mut s = String::new();
        self.display_type(ty, &mut s).unwrap();
        s
    }

    fn display_type(&self, ty: &Type, writ: &mut impl Write) -> std::fmt::Result {
        if let Some(defn_info) = ty.defn_info() {
            return writ.write_str(self.get_ident_str(defn_info.name));
        }
        match ty {
            Type::Unit => writ.write_str("()"),
            Type::Char => writ.write_str("char"),
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
            Type::Bool => writ.write_str("bool"),
            Type::String => writ.write_str("string"),
            Type::Pointer => writ.write_str("Pointer"),
            Type::Struct(struc) => {
                // Leftover, redundant to print name and expand. We need 2 prints, one that expands for
                // debug and one that doesn't for codegen and other real use
                if let Some(defn_info) = struc.type_defn_info.as_ref() {
                    writ.write_str(self.get_ident_str(defn_info.name))?;
                    writ.write_str("(")?;
                }
                writ.write_str("{")?;
                for (index, field) in struc.fields.iter().enumerate() {
                    if index > 0 {
                        writ.write_str(", ")?;
                    }
                    writ.write_str(self.ast.identifiers.get_name(field.name))?;
                    writ.write_str(": ")?;
                    self.display_type_id(field.type_id, writ)?;
                }
                writ.write_str("}")?;
                if let Some(_defn_info) = struc.type_defn_info.as_ref() {
                    writ.write_str(")")?;
                };
                Ok(())
            }
            Type::Array(array) => {
                writ.write_str("Array[")?;
                self.display_type_id(array.element_type, writ)?;
                writ.write_str("]")
            }
            Type::TypeVariable(tv) => {
                let scope_name = self.make_scope_name(self.scopes.get_scope(tv.scope_id));
                writ.write_str(&scope_name)?;
                writ.write_str(".")?;
                writ.write_str("$")?;
                writ.write_str(self.ast.identifiers.get_name(tv.name))?;
                Ok(())
            }
            Type::Optional(opt) => {
                self.display_type_id(opt.inner_type, writ)?;
                writ.write_char('?')
            }
            Type::Reference(r) => {
                self.display_type_id(r.inner_type, writ)?;
                writ.write_char('*')
            }
            Type::TagInstance(tag) => {
                writ.write_str("Tag.")?;
                writ.write_str(self.ast.identifiers.get_name(tag.ident))
            }
            Type::Enum(e) => {
                if let Some(defn_info) = e.type_defn_info.as_ref() {
                    writ.write_str(self.get_ident_str(defn_info.name))?;
                    writ.write_str("(")?;
                }
                writ.write_str("enum ")?;
                for (idx, v) in e.variants.iter().enumerate() {
                    writ.write_str(self.ast.identifiers.get_name(v.tag_name))?;
                    if let Some(payload) = &v.payload {
                        writ.write_str("(")?;
                        self.display_type_id(*payload, writ)?;
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
                Ok(())
            }
            Type::EnumVariant(ev) => {
                let enum_type = self.types.get(ev.enum_type_id).expect_enum();
                if let Some(defn_info) = enum_type.type_defn_info.as_ref() {
                    writ.write_str(self.get_ident_str(defn_info.name))?;
                    writ.write_str(".")?;
                }
                writ.write_str(self.ast.identifiers.get_name(ev.tag_name))?;
                if let Some(payload) = &ev.payload {
                    writ.write_str("(")?;
                    self.display_type_id(*payload, writ)?;
                    writ.write_str(")")?;
                }
                Ok(())
            }
            Type::Never => writ.write_str("never"),
            Type::OpaqueAlias(opaque) => {
                writ.write_str(self.get_ident_str(opaque.type_defn_info.name))?;
                writ.write_str("(")?;
                self.display_type_id(opaque.aliasee, writ)?;
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
                writ.write_str("(")?;
                self.display_type_id(gen.inner, writ)?;
                writ.write_str(")")
            }
            Type::Function(fun) => {
                writ.write_str("fn(")?;
                for (idx, param) in fun.params.iter().enumerate() {
                    self.display_type_id(param.type_id, writ)?;
                    let last = idx == fun.params.len() - 1;
                    if !last {
                        writ.write_str(", ")?;
                    }
                }
                writ.write_str(") -> ")?;
                self.display_type_id(fun.return_type, writ)
            }
            Type::RecursiveReference(rr) => {
                if rr.is_pending() {
                    writ.write_str("pending")
                } else {
                    self.display_type_id(rr.root_type_id, writ)
                }
            }
        }
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
        if function.linkage == Linkage::External {
            writ.write_str("extern ")?;
        }
        if function.linkage == Linkage::Intrinsic {
            writ.write_str("intern ")?;
        }

        writ.write_str("fn ")?;
        writ.write_str(&self.get_ident_str(function.name))?;
        writ.write_str("(")?;
        for (idx, param) in function.params.iter().enumerate() {
            if idx > 0 {
                writ.write_str(", ")?;
            }
            writ.write_str(&self.get_ident_str(param.name))?;
            writ.write_str(": ")?;
            self.display_type_id(param.type_id, writ)?;
        }
        writ.write_str(")")?;
        writ.write_str(": ")?;
        self.display_type_id(function.ret_type, writ)?;
        if display_block {
            if let Some(block) = &function.block {
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
        writ.write_str(&" ".repeat(indentation))?;
        writ.write_str("}: ")?;
        self.display_type_id(block.expr_type, writ)
    }

    fn display_stmt(
        &self,
        stmt: &TypedStmt,
        writ: &mut impl Write,
        indentation: usize,
    ) -> std::fmt::Result {
        writ.write_str(&" ".repeat(indentation))?;
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
            TypedExpr::Bool(b, _) => write!(writ, "{}", b),
            TypedExpr::Str(s, _) => write!(writ, "\"{}\"", s),
            TypedExpr::OptionalNone(typ, _) => {
                writ.write_str("None[")?;
                self.display_type_id(*typ, writ)?;
                writ.write_str("]")
            }
            TypedExpr::Array(array) => {
                writ.write_str("[")?;
                for (idx, expr) in array.elements.iter().enumerate() {
                    if idx > 0 {
                        writ.write_str(", ")?;
                    }
                    self.display_expr(expr, writ, indentation)?;
                }
                writ.write_str("]")
            }
            TypedExpr::Struct(struc) => {
                writ.write_str("{")?;
                for (idx, field) in struc.fields.iter().enumerate() {
                    if idx > 0 {
                        writ.write_str(",\n")?;
                        writ.write_str(&" ".repeat(indentation + 1))?;
                    }
                    writ.write_str(&self.get_ident_str(field.name))?;
                    writ.write_str(": ")?;
                    self.display_expr(&field.expr, writ, indentation)?;
                }
                writ.write_str(&" ".repeat(indentation))?;
                writ.write_str("}")
            }
            TypedExpr::Variable(v) => {
                let variable = self.variables.get_variable(v.variable_id);
                writ.write_str(&self.get_ident_str(variable.name))
            }
            TypedExpr::StructFieldAccess(field_access) => {
                self.display_expr(&field_access.base, writ, indentation)?;
                writ.write_str(".")?;
                writ.write_str(&self.get_ident_str(field_access.target_field))
            }
            TypedExpr::FunctionCall(fn_call) => {
                let function = self.get_function(fn_call.callee_function_id);
                writ.write_str(&self.get_ident_str(function.name))?;
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
                self.display_block(&if_expr.consequent, writ, indentation)?;
                if !if_expr.alternate.is_single_unit_block() {
                    writ.write_str(" else ")?;
                    self.display_block(&if_expr.alternate, writ, indentation)?;
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
            TypedExpr::OptionalSome(opt) => {
                writ.write_str("Some(")?;
                self.display_expr(&opt.inner_expr, writ, indentation)?;
                writ.write_str(")")
            }
            TypedExpr::OptionalHasValue(opt) => {
                self.display_expr(opt, writ, indentation)?;
                writ.write_str(".hasValue()")
            }
            TypedExpr::OptionalGet(opt) => {
                self.display_expr(&opt.inner_expr, writ, indentation)?;
                writ.write_str("!")
            }
            TypedExpr::Tag(tag_expr) => {
                writ.write_str(".")?;
                writ.write_str(&self.get_ident_str(tag_expr.name))
            }
            TypedExpr::EnumConstructor(enum_constr) => {
                writ.write_str(".")?;
                writ.write_str(&self.get_ident_str(enum_constr.variant_name))?;
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
                self.display_type_id(cast.target_type_id, writ)
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
            PatternConstructor::None => writ.write_str("None"),
            PatternConstructor::Char => writ.write_str("'<char>'"),
            PatternConstructor::String => writ.write_str("\"<string>\""),
            PatternConstructor::Int => writ.write_str("<int>"),
            PatternConstructor::TypeVariable => writ.write_str("<tvar>"),
            PatternConstructor::Some(inner) => {
                writ.write_str("Some(")?;
                self.display_pattern_ctor(inner, writ)?;
                writ.write_str(")")?;
                Ok(())
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
            TypedPattern::LiteralBool(value, _) => write!(writ, "{value}"),
            TypedPattern::LiteralString(s, _) => write!(writ, "\"{s}\""),
            TypedPattern::LiteralNone(_) => writ.write_str("None"),
            TypedPattern::Variable(var) => writ.write_str(self.get_ident_str(var.name)),
            TypedPattern::Wildcard(_) => writ.write_str("_"),
            TypedPattern::Some(some) => {
                writ.write_str("Some(")?;
                self.display_pattern(&some.inner_pattern, writ)?;
                writ.write_str(")")?;
                Ok(())
            }
            TypedPattern::Enum(enum_pat) => {
                writ.write_str(self.get_ident_str(enum_pat.variant_tag_name))?;
                if let Some(payload) = enum_pat.payload.as_ref() {
                    writ.write_str("(")?;
                    self.display_pattern(&payload, writ)?;
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
