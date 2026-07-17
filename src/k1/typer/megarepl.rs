use super::*;
use crate::failf;

#[derive(Clone)]
pub struct MegareplCell {
    pub id: u32,
    /// Run after every execution of other cells
    pub is_watcher: bool,
    /// Show this cell's output as a tile on the canvas
    pub is_pinned: bool,
    pub expr_id: Option<TypedExprId>,
    pub iteration: u32,
    pub source_id: FileId,
    /// An explanatory message for display
    pub message: String,
    pub last_result: CellResult,
    pub last_exec_time: Option<std::time::Duration>,
}

#[derive(Clone)]
pub enum CellResult {
    Expr { value: StaticValueId },
    // Defn {  },
    Error { k1_message: K1Message },
}
impl CellResult {
    pub fn error(k1_message: K1Message) -> Self {
        CellResult::Error { k1_message }
    }
}
pub struct MegareplState {
    pub vm: vm::Vm,
    pub parsed_ns: ParsedNamespaceId,
    pub ns: NamespaceId,
    pub ns_scope: ScopeId,
    pub cells: Vec<MegareplCell>,
}

impl TypedProgram {
    fn parse_repl_source(&mut self, file_id: FileId) -> K1Result<ParseReplSourceResult> {
        self.with_parser(file_id, move |p| {
            let msg_base = "Failed to parse the code you returned: ";
            let error_count_start = p.ast.errors.len();
            let p_result = p.parse_function(None);
            let new_errors = p.ast.errors.len() - error_count_start;
            match p_result {
                Err(e) => {
                    failf!(e.span(), "{msg_base}{}", e)
                }
                Ok(_) if new_errors > 0 => {
                    let e = p.ast.errors.last().unwrap();
                    failf!(e.span(), "{msg_base}{}", e)
                }
                Ok(Some(defn)) => Ok(ParseReplSourceResult::Defn(ParsedId::Function(defn))),
                Ok(None) => match p.parse_block_statements(TokenKind::Eof) {
                    Err(e) => failf!(e.span(), "{msg_base}{}", e),
                    Ok(stmts) => Ok(ParseReplSourceResult::Stmts(stmts)),
                },
            }
        })
    }

    pub fn megarepl_submit(&mut self, cell_id: Option<u32>, code: String) -> u32 /* cell id */ {
        self.ensure_megarepl_session();
        if let Some(cell_id) = cell_id {
            let existing = self.megarepl_get_cell(cell_id);
            let existing_code = &self.ast.sources.get(existing.source_id).content;
            if existing_code != &code {
                let iteration = existing.iteration + 1;
                let new_source = self.megarepl_create_source(cell_id, iteration, code);
                let cell = self.megarepl_get_cell_mut(cell_id);
                cell.source_id = new_source;
                cell.iteration = iteration;
                if let Err(e) = self.megarepl_compile_source(cell_id) {
                    self.megarepl_set_cell_compile_error(cell_id, e);
                }
            };
            self.megarepl_execute_cell(cell_id);
            self.megarepl_set_cell_message(cell_id, "Re-ran".to_string());
            cell_id
        } else {
            let cell_id = self.megarepl_new(code);
            if let Err(e) = self.megarepl_compile_source(cell_id) {
                self.megarepl_set_cell_compile_error(cell_id, e);
            }
            self.megarepl_execute_cell(cell_id);
            cell_id
        }
    }

    fn megarepl_set_cell_compile_error(&mut self, cell_id: u32, error: K1Message) {
        let mut cell = self.megarepl_get_cell_mut(cell_id);
        cell.last_result = CellResult::Error { k1_message: error };
        cell.expr_id = None;
    }

    fn megarepl_set_cell_message(&mut self, cell_id: u32, message: String) {
        self.megarepl_get_cell_mut(cell_id).message = message;
    }

    pub fn megarepl_get_cell(&self, cell_id: u32) -> &MegareplCell {
        &self.megarepl.as_ref().unwrap().cells[cell_id as usize]
    }

    fn megarepl_get_cell_mut(&mut self, cell_id: u32) -> &mut MegareplCell {
        &mut self.megarepl.as_mut().unwrap().cells[cell_id as usize]
    }

    fn megarepl_create_source(&mut self, cell_id: u32, iteration: u32, code: String) -> FileId {
        let filename = format!("{}_repl_cell_{}.{}.k1", self.program_name(), cell_id, iteration);
        let source_id = self.ast.sources.add_file(crate::parse::SourceFile::make(
            0,
            self.config.out_dir.to_str().unwrap().to_owned(),
            filename,
            code,
        ));
        source_id
    }

    fn megarepl_new(&mut self, code: String) -> u32 {
        let mr = self.megarepl.as_mut().unwrap();
        let cell_id = mr.cells.len() as u32;
        let source_id = self.megarepl_create_source(cell_id, 0, code);
        let mr = self.megarepl.as_mut().unwrap();
        mr.cells.push(MegareplCell {
            id: cell_id,
            is_watcher: false,
            is_pinned: false,
            expr_id: None,
            iteration: 0,
            source_id,
            last_result: CellResult::error(make_warning("uninit", SpanId::NONE)),
            message: String::new(),
            last_exec_time: None,
        });
        cell_id
    }

    // Have to compile, which sets expr
    // Then execute, which sets result
    fn megarepl_compile_source(&mut self, cell_id: u32) -> K1Result<()> {
        let source = self.megarepl_get_cell(cell_id).source_id;
        let repl_source_result = match self.parse_repl_source(source) {
            Err(e) => {
                return Err(e);
            }
            Ok(r) => r,
        };

        match repl_source_result {
            ParseReplSourceResult::Stmts(stmts) => match self.megarepl_compile_statements(stmts) {
                Err(k1_message) => Err(k1_message),
                Ok(expr_id) => {
                    self.megarepl_get_cell_mut(cell_id).expr_id = Some(expr_id);
                    // Solo: megarepl_submit executes again after compiling, and
                    // that run does the watcher fan-out
                    self.megarepl_execute_cell_solo(cell_id);
                    Ok(())
                }
            },
            ParseReplSourceResult::Defn(_parsed_id) => {
                eprintln!("skipping compile of definitions");
                Ok(())
            }
        }
    }

    fn megarepl_execute_cell(&mut self, cell_id: u32) {
        self.megarepl_execute_cell_solo(cell_id);

        // Watchers re-run after any other cell runs. Fanning out only from
        // here (not from megarepl_execute_cell_solo) keeps a watcher's own
        // execution from re-triggering the watchers forever.
        let cells = self.megarepl.as_ref().unwrap().cells.iter().map(|c| c.id).collect_vec();
        for watcher_id in cells {
            if watcher_id != cell_id && self.megarepl_get_cell(watcher_id).is_watcher {
                self.megarepl_execute_cell_solo(watcher_id);
            }
        }
    }

    fn megarepl_execute_cell_solo(&mut self, cell_id: u32) {
        let cell = self.megarepl_get_cell(cell_id);
        let Some(cell_expr) = cell.expr_id else {
            eprintln!("nothing to execute");
            return;
        };
        let span = self.exprs.get_span(cell_expr);
        eprintln!(
            "executing repl unit.\n{}",
            ir::unit_to_string(self, IrUnitId::Expr(cell_expr), true)
        );
        let exec_start = std::time::Instant::now();
        let exec_result = self
            .do_with_vm(span, |k1, vm| bc::exec::execute_compiled_expr(k1, vm, cell_expr, true));
        self.megarepl_get_cell_mut(cell_id).last_exec_time = Some(exec_start.elapsed());
        match exec_result {
            Err(k1_message) => {
                self.megarepl_get_cell_mut(cell_id).last_result = CellResult::Error { k1_message };
            }
            Ok(value) => {
                self.megarepl_get_cell_mut(cell_id).last_result = CellResult::Expr { value };
            }
        }
    }

    pub fn megarepl_toggle_watcher(&mut self, cell_id: u32) -> bool {
        let cell = self.megarepl_get_cell_mut(cell_id);
        cell.is_watcher = !cell.is_watcher;
        cell.is_watcher
    }

    pub fn megarepl_toggle_pin(&mut self, cell_id: u32) -> bool {
        let cell = self.megarepl_get_cell_mut(cell_id);
        cell.is_pinned = !cell.is_pinned;
        // For now pinned implies watched: a displayed tile is a live one
        cell.is_watcher = cell.is_pinned;
        cell.is_pinned
    }

    fn megarepl_compile_statements(
        &mut self,
        stmts: List<ParsedStmtId, ParsedProgram>,
    ) -> K1Result<TypedExprId> {
        let span = stmts.first().map(|s| self.ast.get_stmt_span(*s)).unwrap_or(SpanId::NONE);
        let repl_ns_scope = self.megarepl.as_ref().unwrap().ns_scope;
        // We're putting a Lexical Block in a Namespace directly.
        // This is impossible in K1, but the compiler should not care.
        let mut cell_block = self.new_block_builder(
            repl_ns_scope,
            ScopeType::LexicalBlock,
            span,
            stmts.len() as u32 + 1,
        );
        let block_scope = cell_block.scope_id;
        let ctx = EvalExprContext::make(block_scope);
        for (index, stmt) in stmts.as_slice().iter().enumerate() {
            match self.ast.stmts.get(*stmt).clone() {
                ParsedStmt::Let(parsed_let) => {
                    // Intercept and create a global for repl lets
                    let expected_type = match parsed_let.type_expr {
                        None => None,
                        Some(type_expr) => Some(self.eval_type_expr(type_expr, block_scope)?),
                    };
                    let rhs = match parsed_let.value {
                        Some(expr) => Some(self.eval_expr(expr, ctx.with_expected_type(expected_type))?),
                        None => None,
                    };
                    if rhs.is_none() && expected_type.is_none() {
                        return failf!(
                            parsed_let.span,
                            "let without type or initializer is not allowed"
                        );
                    }
                    let type_id = match rhs {
                        None => expected_type.unwrap(),
                        Some(rhs) => self.exprs.get_type(rhs),
                    };
                    let global_id = self.globals.next_id();
                    let name = parsed_let.name;
                    let flags = VariableFlags::Reassigned;
                    let variable_id = self.variables.add(Variable {
                        name,
                        type_id,
                        owner_scope: repl_ns_scope,
                        flags,
                        usage_count: 0,
                        usages: vec![],
                        kind: VariableKind::Global(global_id),
                        defn_span: parsed_let.span,
                    });
                    let _global_id = self.globals.add_expected_id(
                        TypedGlobal {
                            variable_id,
                            // We're relocating the initializer to the block; so this
                            // should appear as an uninitialized global.
                            // The whole situation feels like a hack.
                            // But if I can build this without touching any other code
                            // maybe that's worth celebrating as the opposite of a hack;
                            // the compiler is supporting higher-level systems like a platform
                            parsed_expr: None,
                            initial_value: GlobalInitialValue::Uninit,
                            type_id,
                            span: parsed_let.span,
                            is_constant: false,
                            is_tls: false,
                            is_exported: false,
                            is_external: false,
                            ast_id: ParsedGlobalId::PENDING,
                            parent_scope: repl_ns_scope,
                        },
                        global_id,
                    );
                    self.scopes.add_variable(repl_ns_scope, name, variable_id);

                    // Global is created. Now at this position in the block, we do
                    // a set
                    if let Some(initializer) = rhs {
                        let variable_expr = self.synth_variable_expr(variable_id, SpanId::NONE);
                        let assign_stmt = self.stmts.add(TypedStmt::Assignment(AssignmentStmt {
                            destination: variable_expr,
                            value: initializer,
                            span: parsed_let.span,
                            kind: AssignmentKind::Set,
                        }));
                        self.push_block_stmt_id(&mut cell_block, assign_stmt);
                    }
                }
                ParsedStmt::Use(_)
                | ParsedStmt::Require(_)
                | ParsedStmt::Assign(_)
                | ParsedStmt::Store(_)
                | ParsedStmt::Defer(_)
                | ParsedStmt::LoneExpression(_) => {
                    if let Some(stmt_id) = self.eval_stmt(*stmt, ctx, false, index)? {
                        self.push_block_stmt_id(&mut cell_block, stmt_id);
                    }
                }
            }
        }
        let type_id = match cell_block.statements.last() {
            Some(last) => {
                // TODO: Handle never
                if let TypedStmt::Expr(e, _) = self.stmts.get(*last) {
                    cell_block.statements.pop();
                    let type_id = self.exprs.get_type(*e);
                    let return_expr = self.exprs.add_return(*e, None, span);
                    self.push_block_expr_id(&mut cell_block, return_expr);
                    type_id
                } else {
                    let empty = self.synth_empty_struct(span);
                    let return_expr = self.exprs.add_return(empty, None, span);
                    self.push_block_expr_id(&mut cell_block, return_expr);
                    self.types.builtins.empty
                }
            }
            None => {
                let empty = self.synth_empty_struct(span);
                let return_expr = self.exprs.add_return(empty, None, span);
                self.push_block_expr_id(&mut cell_block, return_expr);
                self.types.builtins.empty
            }
        };
        let cell_expr = self.exprs.add_block(&mut self.mem, cell_block, type_id);
        eprintln!("compiled megarepl block: \n{}", self.expr_to_string(cell_expr));

        ir::compile_top_level_expr(self, cell_expr, &[], false)?;
        ir::validate_unit(self, IrUnitId::Expr(cell_expr))?;
        self.compile_all_pending_ir(span)?;
        ir::optimize_unit(self, IrUnitId::Expr(cell_expr));
        Ok(cell_expr)
    }

    pub fn ensure_megarepl_session(&mut self) {
        match self.megarepl {
            Some(_) => {}
            None => {
                let name = self.ast.idents.intern("megarepl");
                let parsed_namespace_id =
                    self.ast.namespaces.add(parse::ParsedNamespace::empty(name));

                let module_id = self.module_in_progress.unwrap();
                let module = self.modules.get(module_id);
                let ns_scope_id = self.scopes.add_child_scope(
                    module.namespace_scope_id,
                    ScopeType::Namespace,
                    ScopeOwnerId::None,
                );
                let ns_id = self.namespaces.add(Namespace {
                    name,
                    scope_id: ns_scope_id,
                    namespace_type: NamespaceKind::User,
                    companion_type_id: None,
                    parent_id: Some(module.namespace_id),
                    owner_module: Some(module_id),
                    parsed_id: ParsedId::Namespace(parsed_namespace_id),
                });
                self.megarepl = Some(MegareplState {
                    vm: vm::Vm::make(),
                    parsed_ns: parsed_namespace_id,
                    ns: ns_id,
                    ns_scope: ns_scope_id,
                    cells: vec![],
                });
            }
        }
    }
}
