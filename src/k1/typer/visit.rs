// Copyright (c) 2025 knix
// All rights reserved.

use super::*;
impl TypedProgram {
    #[allow(unused)]
    fn visit_stmt_tree<S, R>(
        &self,
        stmt_id: TypedStmtId,
        state: &mut S,
        action: &mut impl FnMut(&TypedProgram, TypedExprId, &mut S) -> Option<R>,
    ) -> Option<R> {
        let stmt = self.stmts.get(stmt_id);
        match stmt {
            TypedStmt::Expr(e, _) => self.visit_expr_tree(*e, state, action),
            TypedStmt::Let(val_def) => match val_def.initializer {
                None => None,
                Some(initializer) => self.visit_expr_tree(initializer, state, action),
            },
            TypedStmt::Assignment(assgn) => {
                if let Some(r) = self.visit_expr_tree(assgn.destination, state, action) {
                    return Some(r);
                };
                if let Some(r) = self.visit_expr_tree(assgn.value, state, action) {
                    return Some(r);
                };
                None
            }
            TypedStmt::Require(typed_require_stmt) => {
                if let Some(r) =
                    self.visit_matching_condition(&typed_require_stmt.condition, state, action)
                {
                    return Some(r);
                };
                if let Some(else_body) = typed_require_stmt.else_body {
                    if let Some(r) = self.visit_expr_tree(else_body, state, action) {
                        return Some(r);
                    }
                }
                None
            }
            TypedStmt::Defer(defer) => None,
        }
    }

    fn visit_matching_condition<S, R>(
        &self,
        cond: &MatchingCondition,
        state: &mut S,
        action: &mut impl FnMut(&TypedProgram, TypedExprId, &mut S) -> Option<R>,
    ) -> Option<R> {
        for instr in self.mem.getn(cond.instrs) {
            let result = match instr {
                MatchingConditionInstr::Binding { let_stmt, .. } => {
                    self.visit_stmt_tree(*let_stmt, state, action)
                }
                MatchingConditionInstr::Cond { value } => {
                    self.visit_expr_tree(*value, state, action)
                }
            };
            if let Some(r) = result {
                return Some(r);
            }
        }
        None
    }

    // The task of 'visiting' each expr involves 2 things
    // - Call action on it(self)
    // - Call visit on its children.
    //
    // It is not the job of action to recurse to its children
    fn visit_expr_tree<S, R>(
        &self,
        expr: TypedExprId,
        state: &mut S,
        action: &mut impl FnMut(&TypedProgram, TypedExprId, &mut S) -> Option<R>,
    ) -> Option<R> {
        eprintln!("VISITING {}", self.expr_to_string(expr));
        macro_rules! recurse {
            ($expr_id:expr) => {
                if let Some(r) = self.visit_expr_tree($expr_id, state, action) {
                    return Some(r);
                }
            };
        }
        macro_rules! recurse_stmt {
            ($stmt_id:expr) => {
                if let Some(r) = self.visit_stmt_tree($stmt_id, state, action) {
                    return Some(r);
                }
            };
        }

        if let Some(r) = action(self, expr, state) {
            return Some(r);
        }

        match self.exprs.get(expr) {
            TypedExpr::Struct(s) => {
                for f in self.mem.getn(s.fields).iter() {
                    if let Some(expr) = f.expr {
                        recurse!(expr);
                    }
                }
            }
            TypedExpr::StructFieldAccess(field_access) => {
                recurse!(field_access.base);
            }
            TypedExpr::ArrayGetElement(array_get) => {
                recurse!(array_get.base);
                recurse!(array_get.index);
            }
            TypedExpr::Variable(_) => (),
            TypedExpr::Deref(deref) => {
                recurse!(deref.target);
            }
            TypedExpr::AddressOf(_) => {}
            TypedExpr::Block(block) => {
                for stmt in self.mem.getn(block.statements).iter() {
                    recurse_stmt!(*stmt);
                }
            }
            TypedExpr::Call { call_id, .. } => {
                let call = self.calls.get(*call_id);
                match call.callee {
                    Callee::DynamicLambda(callee_expr) => recurse!(callee_expr),
                    Callee::DynamicFunction { function_pointer_expr } => {
                        recurse!(function_pointer_expr)
                    }
                    _ => {}
                };
                for arg in self.mem.getn(call.args) {
                    recurse!(*arg);
                }
            }
            TypedExpr::Match(typed_match) => {
                for stmt in self.mem.getn(typed_match.initial_let_statements) {
                    recurse_stmt!(*stmt);
                }
                for arm in self.mem.getn(typed_match.arms) {
                    if let Some(r) = self.visit_matching_condition(&arm.condition, state, action) {
                        return Some(r);
                    };
                    recurse!(arm.consequent_expr);
                }
            }
            TypedExpr::WhileLoop(while_loop) => {
                if let Some(r) = self.visit_matching_condition(&while_loop.condition, state, action)
                {
                    return Some(r);
                };
                recurse!(while_loop.body);
            }
            TypedExpr::LoopExpr(loop_expr) => {
                recurse!(loop_expr.body_block);
            }
            TypedExpr::SumConstructor(constr) => {
                if let Some(payload) = constr.payload {
                    recurse!(payload)
                }
            }
            TypedExpr::SumGetTag(get_enum_tag) => {
                recurse!(get_enum_tag.sum_expr_or_reference);
            }
            TypedExpr::SumGetPayload(enum_get_payload) => {
                recurse!(enum_get_payload.sum_expr);
            }
            TypedExpr::Enum(_) => {}
            TypedExpr::EnumGetValue(egv) => {
                recurse!(egv.enum_expr)
            }
            TypedExpr::Cast(cast) => recurse!(cast.base_expr),
            TypedExpr::Return(ret) => recurse!(ret.value),
            TypedExpr::Break(brk) => recurse!(brk.value),
            TypedExpr::Lambda(lam) => {
                let lambda_type_id = self.types.get(lam.lambda_type).as_lambda().unwrap();
                let lambda_type = self.types.lambda_types.get(lambda_type_id);
                let function = self.get_function(lambda_type.function_id);
                recurse!(function.body_block.expect("lambdas have bodies"));
            }
            TypedExpr::FunctionPointer(_) => {}
            TypedExpr::PendingCapture(_) => {}
            TypedExpr::StaticValue(_) => {}
        };
        None
    }
}
