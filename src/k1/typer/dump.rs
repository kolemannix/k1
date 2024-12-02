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
            self.display_type(f, ty, true)?;
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

impl TypedModule {
    pub fn scope_to_string(&self, scope: &Scope) -> String {
        let mut s = String::new();
        self.display_scope(scope, &mut s).unwrap();
        s
    }

    pub fn display_scope(&self, scope: &Scope, w: &mut impl Write) -> std::fmt::Result {
        let scope_name = self.scopes.make_scope_name(scope, &self.ast.identifiers);
        writeln!(w, "{}", scope_name)?;

        for (id, variable_id) in scope.variables.iter() {
            let variable = self.variables.get_variable(*variable_id);
            write!(w, "\t{} ", id)?;
            self.display_variable(variable, w)?;
            w.write_str("\n")?;
        }
        for function_id in scope.functions.values() {
            let function = self.get_function(*function_id);
            w.write_str("\t")?;
            self.display_function(function, w, false)?;
            w.write_str("\n")?;
        }
        if !scope.types.is_empty() {
            w.write_str("\tTYPES\n")?;
        }
        for (ident, type_id) in scope.types.iter() {
            w.write_str("\t")?;
            w.write_str(self.get_ident_str(*ident))?;
            w.write_str(" := ")?;
            self.display_type_id(w, *type_id, true)?;
            w.write_str("\n")?;
        }
        for (id, namespace_id) in scope.namespaces.iter() {
            write!(w, "{} ", id)?;
            let namespace = self.namespaces.get(*namespace_id);
            w.write_str(self.get_ident_str(namespace.name))?;
            w.write_str("\n")?;
        }
        Ok(())
    }

    fn display_variable(&self, var: &Variable, w: &mut impl Write) -> std::fmt::Result {
        if var.is_mutable {
            w.write_str("mut ")?;
        }
        w.write_str(self.get_ident_str(var.name))?;
        w.write_str(": ")?;
        self.display_type_id(w, var.type_id, false)
    }

    fn display_type_id(&self, w: &mut impl Write, ty: TypeId, expand: bool) -> std::fmt::Result {
        let ty = self.types.get_no_follow(ty);
        self.display_type(w, ty, expand)
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
        self.display_type(&mut s, ty, expand).unwrap();
        s
    }

    fn display_instance_info(
        &self,
        w: &mut impl Write,
        spec_info: &GenericInstanceInfo,
        expand: bool,
    ) -> std::fmt::Result {
        w.write_str("[")?;
        for (index, t) in spec_info.param_values.iter().enumerate() {
            self.display_type_id(w, *t, expand)?;
            let last = index == spec_info.param_values.len() - 1;
            if !last {
                w.write_str(", ")?;
            }
        }
        w.write_str("]")?;
        Ok(())
    }

    fn display_type(&self, w: &mut impl Write, ty: &Type, expand: bool) -> std::fmt::Result {
        match ty {
            Type::Unit(_) => w.write_str("unit"),
            Type::Char(_) => w.write_str("char"),
            Type::Integer(int_type) => {
                match int_type {
                    IntegerType::U8 => w.write_str("u8")?,
                    IntegerType::U16 => w.write_str("u16")?,
                    IntegerType::U32 => w.write_str("u32")?,
                    IntegerType::U64 => w.write_str("u64")?,
                    IntegerType::I8 => w.write_str("i8")?,
                    IntegerType::I16 => w.write_str("i16")?,
                    IntegerType::I32 => w.write_str("i32")?,
                    IntegerType::I64 => w.write_str("i64")?,
                }
                Ok(())
            }
            Type::Float(float_type) => match float_type.size {
                NumericWidth::B8 => write!(w, "f8"),
                NumericWidth::B16 => write!(w, "f16"),
                NumericWidth::B32 => write!(w, "f32"),
                NumericWidth::B64 => write!(w, "f64"),
            },
            Type::Bool(_) => w.write_str("bool"),
            Type::Pointer(_) => w.write_str("Pointer"),
            Type::Struct(struc) => {
                if let Some(defn_info) = struc.type_defn_info.as_ref() {
                    w.write_str(self.get_ident_str(defn_info.name))?;
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
            Type::TypeVariable(tv) => {
                if expand {
                    let scope_name = self
                        .scopes
                        .make_scope_name(self.scopes.get_scope(tv.scope_id), &self.ast.identifiers);
                    w.write_str(&scope_name)?;
                    w.write_str(".")?;
                    w.write_str("$")?;
                    w.write_str(self.ast.identifiers.get_name(tv.name))?;
                } else {
                    w.write_str(self.ast.identifiers.get_name(tv.name))?;
                }
                Ok(())
            }
            Type::Reference(r) => {
                self.display_type_id(w, r.inner_type, expand)?;
                w.write_char('*')
            }
            Type::Enum(e) => {
                if let Some(defn_info) = e.type_defn_info.as_ref() {
                    w.write_str(self.get_ident_str(defn_info.name))?;
                    if let Some(spec_info) = e.generic_instance_info.as_ref() {
                        self.display_instance_info(w, spec_info, expand)?;
                    }
                    if expand {
                        w.write_str("(")?;
                    }
                }
                if expand {
                    w.write_str("either ")?;
                    for (idx, v) in e.variants.iter().enumerate() {
                        w.write_str(self.ast.identifiers.get_name(v.name))?;
                        if let Some(payload) = &v.payload {
                            w.write_str("(")?;
                            self.display_type_id(w, *payload, expand)?;
                            w.write_str(")")?;
                        }
                        let last = idx == e.variants.len() - 1;
                        if !last {
                            w.write_str(" | ")?;
                        }
                    }
                    if let Some(_defn_info) = e.type_defn_info.as_ref() {
                        w.write_str(")")?;
                    }
                }
                Ok(())
            }
            Type::EnumVariant(ev) => {
                let e = self.types.get(ev.enum_type_id).expect_enum();
                if let Some(defn_info) = e.type_defn_info.as_ref() {
                    w.write_str(self.get_ident_str(defn_info.name))?;
                    if let Some(spec_info) = e.generic_instance_info.as_ref() {
                        self.display_instance_info(w, spec_info, expand)?;
                    }
                    w.write_str(".")?;
                }
                w.write_str(self.ast.identifiers.get_name(ev.name))?;
                if let Some(payload) = &ev.payload {
                    w.write_str("(")?;
                    self.display_type_id(w, *payload, expand)?;
                    w.write_str(")")?;
                }
                Ok(())
            }
            Type::Never(_) => w.write_str("never"),
            Type::Generic(gen) => {
                w.write_str(self.get_ident_str(gen.type_defn_info.name))?;
                w.write_str("[")?;
                for (idx, param) in gen.params.iter().enumerate() {
                    w.write_str(self.get_ident_str(param.name))?;
                    let last = idx == gen.params.len() - 1;
                    if !last {
                        w.write_str(", ")?;
                    }
                }
                w.write_str("]")?;
                if expand {
                    w.write_str("(")?;
                    self.display_type_id(w, gen.inner, expand)?;
                    w.write_str(")")?;
                }
                Ok(())
            }
            Type::Function(fun) => {
                w.write_str("fn")?;
                w.write_str("(")?;
                for (idx, param) in fun.params.iter().enumerate() {
                    self.display_type_id(w, param.type_id, expand)?;
                    let last = idx == fun.params.len() - 1;
                    if !last {
                        w.write_str(", ")?;
                    }
                }
                w.write_str(") -> ")?;
                self.display_type_id(w, fun.return_type, expand)
            }
            Type::Closure(clos) => {
                write!(w, "closure(")?;
                self.display_type_id(w, clos.function_type, expand)?;
                w.write_str(")")?;
                Ok(())
            }
            Type::ClosureObject(closure_object) => {
                w.write_str("closure_object(")?;
                self.display_type_id(w, closure_object.function_type, expand)?;
                w.write_str(")")?;
                Ok(())
            }
            Type::RecursiveReference(rr) => {
                if rr.is_pending() {
                    w.write_str("pending")
                } else {
                    let info = self.types.get_defn_info(rr.root_type_id).unwrap();
                    w.write_str(self.get_ident_str(info.name))?;
                    Ok(())
                }
            }
            Type::Unknown(_u) => w.write_str("?"),
        }
    }

    fn display_struct_fields(
        &self,
        w: &mut impl Write,
        struc: &StructType,
        expand: bool,
    ) -> std::fmt::Result {
        w.write_str("{")?;
        for (index, field) in struc.fields.iter().enumerate() {
            if index > 0 {
                w.write_str(", ")?;
            }
            w.write_str(self.ast.identifiers.get_name(field.name))?;
            w.write_str(": ")?;
            self.display_type_id(w, field.type_id, expand)?;
        }
        w.write_str("}")
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
        w.write_str(self.get_ident_str(function.name))?;
        if !function.type_params.is_empty() {
            w.write_str("[")?;
            for type_param in &function.type_params {
                w.write_str(self.get_ident_str(type_param.type_param.name))?;
            }
            w.write_str("]")?;
        }
        w.write_str("(")?;
        let function_type = self.types.get(function.type_id).as_function().unwrap();
        for (idx, param) in function_type.params.iter().enumerate() {
            if idx > 0 {
                w.write_str(", ")?;
            }
            w.write_str(self.get_ident_str(param.name))?;
            w.write_str(": ")?;
            self.display_type_id(w, param.type_id, false)?;
        }
        w.write_str(")")?;
        w.write_str(": ")?;
        self.display_type_id(w, function_type.return_type, false)?;
        if display_block {
            w.write_str(" ")?;
            if let Some(block) = &function.body_block {
                self.display_block(block, w, 0)?;
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
        w: &mut impl Write,
        indentation: usize,
    ) -> std::fmt::Result {
        if block.statements.len() == 1 {
            if let TypedStmt::Expr(expr) = &block.statements[0] {
                return self.display_expr(expr, w, indentation);
            }
        }
        w.write_str("{\n")?;
        for (idx, stmt) in block.statements.iter().enumerate() {
            self.display_stmt(stmt, w, indentation + 1)?;
            if idx < block.statements.len() - 1 {
                w.write_str(";")?;
            }
            w.write_str("\n")?;
        }
        w.write_str(&"  ".repeat(indentation))?;
        w.write_str("}: ")?;
        self.display_type_id(w, block.expr_type, false)
    }

    fn display_stmt(
        &self,
        stmt: &TypedStmt,
        w: &mut impl Write,
        indentation: usize,
    ) -> std::fmt::Result {
        w.write_str(&"  ".repeat(indentation))?;
        match stmt {
            TypedStmt::Expr(expr) => self.display_expr(expr, w, indentation),
            TypedStmt::Let(let_stmt) => {
                w.write_str("let ")?;
                self.display_variable(self.variables.get_variable(let_stmt.variable_id), w)?;
                w.write_str(" = ")?;
                self.display_expr(&let_stmt.initializer, w, indentation)
            }
            TypedStmt::Assignment(assignment) => {
                self.display_expr(&assignment.destination, w, indentation)?;
                w.write_str(" = ")?;
                self.display_expr(&assignment.value, w, indentation)
            }
        }
    }

    pub fn expr_to_string(&self, expr: &TypedExpr) -> String {
        let mut s = String::new();
        self.display_expr(expr, &mut s, 0).unwrap();
        s
    }

    pub fn expr_to_string_with_type(&self, expr: &TypedExpr) -> String {
        let mut s = String::new();
        self.display_expr(expr, &mut s, 0).unwrap();
        s.push_str(": ");
        self.display_type_id(&mut s, expr.get_type(), false).unwrap();
        s
    }

    pub fn display_expr(
        &self,
        expr: &TypedExpr,
        w: &mut impl Write,
        indentation: usize,
    ) -> std::fmt::Result {
        match expr {
            TypedExpr::Unit(_) => w.write_str("()"),
            TypedExpr::Char(c, _) => write!(w, "'{}'", c),
            TypedExpr::Integer(int) => write!(w, "{}", int.value),
            TypedExpr::Float(float) => write!(w, "{}", float.value),
            TypedExpr::Bool(b, _) => write!(w, "{}", b),
            TypedExpr::Str(s, _) => write!(w, "\"{}\"", s),
            TypedExpr::Struct(struc) => {
                w.write_str("{\n")?;
                for (idx, field) in struc.fields.iter().enumerate() {
                    if idx > 0 {
                        w.write_str(",\n")?;
                    }
                    w.write_str(&"  ".repeat(indentation + 1))?;
                    w.write_str(self.get_ident_str(field.name))?;
                    w.write_str(": ")?;
                    self.display_expr(&field.expr, w, indentation)?;
                }
                w.write_str("\n")?;
                w.write_str(&"  ".repeat(indentation))?;
                w.write_str("}")
            }
            TypedExpr::Variable(v) => {
                let variable = self.variables.get_variable(v.variable_id);
                w.write_str(self.get_ident_str(variable.name))
            }
            TypedExpr::StructFieldAccess(field_access) => {
                self.display_expr(&field_access.base, w, indentation)?;
                w.write_str(".")?;
                w.write_str(self.get_ident_str(field_access.target_field))
            }
            TypedExpr::Call(fn_call) => {
                match &fn_call.callee {
                    Callee::StaticClosure { function_id, .. } => {
                        let name = self.get_function(*function_id).name;
                        w.write_str(self.get_ident_str(name))?;
                    }
                    Callee::StaticFunction(function_id) => {
                        let name = self.get_function(*function_id).name;
                        w.write_str(self.get_ident_str(name))?;
                    }
                    Callee::DynamicFunction(callee_expr) => {
                        self.display_expr(callee_expr, w, indentation)?;
                    }
                    Callee::DynamicClosure(callee_expr) => {
                        self.display_expr(callee_expr, w, indentation)?;
                    }
                };
                w.write_str("(")?;
                for (idx, arg) in fn_call.args.iter().enumerate() {
                    if idx > 0 {
                        w.write_str(", ")?;
                    }
                    self.display_expr(arg, w, indentation)?;
                }
                w.write_str(")")
            }
            TypedExpr::Block(block) => self.display_block(block, w, indentation),
            TypedExpr::If(if_expr) => {
                w.write_str("if ")?;
                self.display_expr(&if_expr.condition, w, indentation)?;
                w.write_str(" ")?;
                self.display_expr(&if_expr.consequent, w, indentation)?;
                if !if_expr.alternate.is_unit() {
                    w.write_str(" else ")?;
                    self.display_expr(&if_expr.alternate, w, indentation)?;
                }
                Ok(())
            }
            TypedExpr::WhileLoop(while_loop) => {
                w.write_str("while ")?;
                self.display_expr(&while_loop.cond, w, indentation)?;
                w.write_str(" ")?;
                self.display_block(&while_loop.body, w, indentation)
            }
            TypedExpr::LoopExpr(loop_expr) => {
                w.write_str("loop ")?;
                self.display_block(&loop_expr.body, w, indentation)
            }
            TypedExpr::UnaryOp(unary_op) => {
                w.write_fmt(format_args!("{}", unary_op.kind))?;
                self.display_expr(&unary_op.expr, w, indentation)
            }
            TypedExpr::BinaryOp(binary_op) => {
                self.display_expr(&binary_op.lhs, w, indentation)?;
                write!(w, " {} ", binary_op.kind)?;
                self.display_expr(&binary_op.rhs, w, indentation)
            }
            TypedExpr::EnumConstructor(enum_constr) => {
                w.write_str(".")?;
                w.write_str(self.get_ident_str(enum_constr.variant_name))?;
                if let Some(payload) = &enum_constr.payload {
                    w.write_str("(")?;
                    self.display_expr(payload, w, indentation)?;
                    w.write_str(")")?;
                }
                Ok(())
            }
            TypedExpr::EnumIsVariant(is_variant_expr) => {
                self.display_expr(&is_variant_expr.target_expr, w, indentation)?;
                w.write_str(".is[.")?;
                w.write_str(self.ast.identifiers.get_name(is_variant_expr.variant_name))?;
                w.write_str("]()")
            }
            TypedExpr::Cast(cast) => {
                self.display_expr(&cast.base_expr, w, indentation)?;
                write!(w, " as({}) ", cast.cast_type)?;
                self.display_type_id(w, cast.target_type_id, false)
            }
            TypedExpr::EnumGetPayload(as_variant_expr) => {
                self.display_expr(&as_variant_expr.target_expr, w, indentation)?;
                w.write_str(".payload")
            }
            TypedExpr::Return(ret) => {
                w.write_str("return(")?;
                self.display_expr(&ret.value, w, indentation)?;
                w.write_char(')')
            }
            TypedExpr::Break(brk) => {
                w.write_str("break(")?;
                self.display_expr(&brk.value, w, indentation)?;
                w.write_char(')')
            }
            TypedExpr::Closure(closure_expr) => {
                w.write_char('\\')?;
                let closure_type = self.types.get(closure_expr.closure_type).as_closure().unwrap();
                let fn_type = self.types.get(closure_type.function_type).as_function().unwrap();
                for arg in fn_type.params.iter() {
                    w.write_str(self.get_ident_str(arg.name))?;
                    w.write_str(": ")?;
                    self.display_type_id(w, arg.type_id, false)?;
                }
                w.write_str(" -> ")?;
                let closure_body =
                    self.get_function(closure_type.body_function_id).body_block.as_ref().unwrap();
                self.display_block(closure_body, w, indentation)?;
                Ok(())
            }
            TypedExpr::FunctionName(fn_name_expr) => {
                let fun = self.get_function(fn_name_expr.function_id);
                w.write_str(self.get_ident_str(fun.name))
            }
            TypedExpr::PendingCapture(pending_capture) => {
                w.write_str("capture(")?;
                let variable = self.variables.get_variable(pending_capture.captured_variable_id);
                w.write_str(self.get_ident_str(variable.name))?;
                w.write_str(")")?;
                Ok(())
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
        w: &mut impl Write,
    ) -> std::fmt::Result {
        match pattern_ctor {
            PatternConstructor::Unit => w.write_str("()"),
            PatternConstructor::BoolTrue => w.write_str("true"),
            PatternConstructor::BoolFalse => w.write_str("false"),
            PatternConstructor::Char => w.write_str("'<char>'"),
            PatternConstructor::String => w.write_str("\"<string>\""),
            PatternConstructor::Int => w.write_str("<int>"),
            PatternConstructor::Float => w.write_str("<float>"),
            PatternConstructor::TypeVariable => w.write_str("<tvar>"),
            PatternConstructor::Reference(inner) => {
                w.write_str("*")?;
                self.display_pattern_ctor(inner, w)
            }
            PatternConstructor::Struct { fields } => {
                w.write_str("{ ")?;
                for (index, (field_name, field_pattern)) in fields.iter().enumerate() {
                    w.write_str(self.get_ident_str(*field_name))?;
                    w.write_str(": ")?;
                    self.display_pattern_ctor(field_pattern, w)?;
                    let last = index == fields.len() - 1;
                    if !last {
                        w.write_str(", ")?;
                    } else {
                        w.write_str(" ")?;
                    }
                }
                w.write_str("}")?;
                Ok(())
            }
            PatternConstructor::Enum { variant_name, inner } => {
                w.write_str(self.get_ident_str(*variant_name))?;
                if let Some(payload) = inner.as_ref() {
                    w.write_str("(")?;
                    self.display_pattern_ctor(payload, w)?;
                    w.write_str(")")?;
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

    pub fn display_pattern(&self, pattern: &TypedPattern, w: &mut impl Write) -> std::fmt::Result {
        match pattern {
            TypedPattern::LiteralUnit(_) => w.write_str("()"),
            TypedPattern::LiteralChar(value, _) => write!(w, "{value}"),
            TypedPattern::LiteralInteger(value, _) => write!(w, "{value}"),
            TypedPattern::LiteralFloat(value, _) => write!(w, "{value}"),
            TypedPattern::LiteralBool(value, _) => write!(w, "{value}"),
            TypedPattern::LiteralString(s, _) => write!(w, "\"{s}\""),
            TypedPattern::Variable(var) => w.write_str(self.get_ident_str(var.name)),
            TypedPattern::Wildcard(_) => w.write_str("_"),
            TypedPattern::Enum(enum_pat) => {
                w.write_str(self.get_ident_str(enum_pat.variant_tag_name))?;
                if let Some(payload) = enum_pat.payload.as_ref() {
                    w.write_str("(")?;
                    self.display_pattern(payload, w)?;
                    w.write_str(")")?;
                };
                Ok(())
            }
            TypedPattern::Struct(struct_pat) => {
                w.write_str("{ ")?;
                for (index, field_pat) in struct_pat.fields.iter().enumerate() {
                    w.write_str(self.get_ident_str(field_pat.name))?;
                    w.write_str(": ")?;
                    self.display_pattern(&field_pat.pattern, w)?;
                    let last = index == struct_pat.fields.len() - 1;
                    if !last {
                        w.write_str(", ")?;
                    } else {
                        w.write_str(" ")?;
                    }
                }
                w.write_str("}")?;
                Ok(())
            }
        }
    }

    pub fn pattern_to_string(&self, pattern: &TypedPattern) -> String {
        let mut s = String::new();
        self.display_pattern(pattern, &mut s).unwrap();
        s
    }

    pub fn display_visible_types(&self, scope_id: ScopeId) -> String {
        let mut scope_id = Some(scope_id);
        let mut s = String::new();
        while let Some(current) = scope_id {
            let current_scope = self.scopes.get_scope(current);
            s.push_str(&self.scopes.make_scope_name(current_scope, &self.ast.identifiers));
            let entries: Vec<_> = current_scope
                .types
                .iter()
                .map(|(name, type_id)| NamedType { name: *name, type_id: *type_id })
                .collect();
            s.push_str(&self.pretty_print_named_types(&entries, current));
            scope_id = current_scope.parent;
        }
        s
    }
}
