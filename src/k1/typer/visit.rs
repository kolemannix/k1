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
                self.visit_expr_tree(typed_require_stmt.else_body, state, action)
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
        for instr in &cond.instrs {
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
            TypedExpr::Unit(_) => (),
            TypedExpr::Char(_, _) => (),
            TypedExpr::Bool(_, _) => (),
            TypedExpr::Integer(_) => (),
            TypedExpr::Float(_) => (),
            TypedExpr::String(_, _) => (),
            TypedExpr::Struct(s) => {
                for f in s.fields.clone().iter() {
                    recurse!(f.expr);
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
            TypedExpr::Block(block) => {
                for stmt in block.statements.iter() {
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
                for arg in call.args.iter() {
                    recurse!(*arg);
                }
            }
            TypedExpr::Match(typed_match) => {
                for stmt in &typed_match.initial_let_statements {
                    recurse_stmt!(*stmt);
                }
                for arm in &typed_match.arms {
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
            TypedExpr::EnumConstructor(constr) => {
                if let Some(payload) = constr.payload {
                    recurse!(payload)
                }
            }
            TypedExpr::EnumGetTag(get_enum_tag) => {
                recurse!(get_enum_tag.enum_expr_or_reference);
            }
            TypedExpr::EnumGetPayload(enum_get_payload) => {
                recurse!(enum_get_payload.enum_variant_expr);
            }
            TypedExpr::Cast(cast) => recurse!(cast.base_expr),
            TypedExpr::Return(ret) => recurse!(ret.value),
            TypedExpr::Break(brk) => recurse!(brk.value),
            TypedExpr::Lambda(lam) => {
                let lambda_type = self.types.get(lam.lambda_type).as_lambda().unwrap();
                let function = self.get_function(lambda_type.body_function_id);
                recurse!(function.body_block.expect("lambdas have bodies"));
            }
            TypedExpr::FunctionPointer(_) => {}
            TypedExpr::FunctionToLambdaObject(_) => {}
            TypedExpr::PendingCapture(_) => {}
            TypedExpr::StaticValue(_, _, _) => {}
        };
        None
    }
}
