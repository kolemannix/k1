use super::*;
use crate::failf;

pub type CellId = u32;
pub type WidgetId = u32;

#[derive(Clone)]
pub struct MegareplCell {
    pub id: CellId,
    pub is_watcher: bool,
    /// When set (and watching), the page re-runs this cell every N ms via
    /// datastar's data-on-interval
    pub watch_interval_ms: Option<u32>,
    pub expr_id: Option<TypedExprId>,
    pub iteration: u32,
    pub source_id: FileId,
    pub last_result: CellResult,
    pub last_exec_time: Option<std::time::Duration>,
    pub last_ir: String,
    /// Hidden globals capturing the values of the cell's non-unit toplevel
    /// expressions, in source order
    pub output_globals: Vec<(TypedGlobalId, TypeId)>,
}

#[derive(Clone)]
pub enum CellResult {
    Expr {
        /// Values of intermediate non-unit toplevel expressions, in order
        outputs: Vec<StaticValueId>,
        /// The cell's final value
        value: StaticValueId,
        stdout: String,
        stderr: String,
    },
    // Defn {  },
    Error {
        k1_message: K1Message,
        stdout: String,
        stderr: String,
    },
}
impl CellResult {
    pub fn error(k1_message: K1Message) -> Self {
        CellResult::Error { k1_message, stdout: String::new(), stderr: String::new() }
    }
}

impl MegareplCell {
    pub fn is_error(&self) -> bool {
        matches!(self.last_result, CellResult::Error { .. })
    }
}

pub struct MegareplGlobal {
    pub global_id: TypedGlobalId,
    /// Value after the most recent run, refreshed once per submit; None until
    /// the global is first assigned
    pub last_value: Option<StaticValueId>,
}

#[derive(Clone, Copy)]
pub enum WidgetDataSource {
    /// A let-bound name in the repl scope
    Binding(StringId),
    /// A watcher cell's latest result
    Watcher(CellId),
    Checkbox {
        name: StringId,
        get_fn: FunctionId,
        set_fn: FunctionId,
        current_value: bool,
    },
}

/// A view of some Data placed on the canvas
#[derive(Clone, Copy)]
pub struct MegareplWidget {
    pub id: WidgetId,
    pub data: WidgetDataSource,
}

pub struct MegareplState {
    pub parsed_ns: ParsedNamespaceId,
    pub ns: NamespaceId,
    pub ns_scope: ScopeId,
    pub cells: Vec<MegareplCell>,
    pub globals: Vec<MegareplGlobal>,
    pub widgets: Vec<MegareplWidget>,
    /// Monotonic so deleting a widget never re-labels the others
    pub next_widget_id: CellId,
    /// The session's dedicated execution VM: repl globals live in its memory
    /// for the whole session, so it is never reset. Option only so
    /// `megarepl_with_vm` can lend it out while `self` stays borrowable.
    pub vm: Option<vm::Vm>,
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

    pub fn megarepl_submit(&mut self, cell_id: Option<CellId>, code: String) {
        self.ensure_megarepl_session();
        let cell_id = match cell_id {
            Some(cell_id) => {
                self.megarepl_update_code(cell_id, code);
                cell_id
            }
            None => self.megarepl_new(code),
        };
        self.megarepl_run(cell_id);
    }

    /// Swaps in new source and recompiles, iff the code actually changed
    fn megarepl_update_code(&mut self, cell_id: CellId, code: String) {
        let existing = self.megarepl_get_cell(cell_id);
        if self.ast.sources.get(existing.source_id).content == code {
            return;
        }
        let iteration = existing.iteration + 1;
        let new_source = self.megarepl_create_source(cell_id, iteration, code);
        let cell = self.megarepl_get_cell_mut(cell_id);
        cell.source_id = new_source;
        cell.iteration = iteration;
        if let Err(e) = self.megarepl_compile_source(cell_id) {
            self.megarepl_set_cell_compile_error(cell_id, e);
        }
    }

    /// The one meaning of "run": execute the cell as compiled (observers fan
    /// out from there), then re-read Data
    pub fn megarepl_run(&mut self, cell_id: CellId) {
        self.megarepl_execute_cell(cell_id);
        self.megarepl_refresh_globals();
    }

    pub fn megarepl_global_is_live(&self, mr_global: &MegareplGlobal) -> bool {
        let ns_scope = self.megarepl.as_ref().unwrap().ns_scope;
        let variable_id = self.globals.get(mr_global.global_id).variable_id;
        let name = self.variables.get(variable_id).name;
        matches!(
            self.scopes.find_variable_local(ns_scope, name),
            Some(VariableInScope::Defined(found)) if found == variable_id
        )
    }

    /// Re-reads every live global's current value out of the VM for display
    fn megarepl_refresh_globals(&mut self) {
        let mr = self.megarepl.as_ref().unwrap();
        let live: Vec<(usize, TypedGlobalId, TypeId)> = mr
            .globals
            .iter()
            .enumerate()
            .filter(|(_, g)| self.megarepl_global_is_live(g))
            .map(|(index, g)| (index, g.global_id, self.globals.get(g.global_id).type_id))
            .collect();

        let values: Vec<Option<StaticValueId>> = self.megarepl_with_vm(|k1, vm| {
            live.iter()
                .map(|(_, global_id, type_id)| {
                    vm::peek_global_as_static(k1, vm, *global_id, *type_id)
                })
                .collect()
        });
        let mr = self.megarepl.as_mut().unwrap();
        for ((index, _, _), value) in live.iter().zip(values) {
            if value.is_some() {
                mr.globals[*index].last_value = value;
            }
        }
    }

    fn megarepl_set_cell_compile_error(&mut self, cell_id: CellId, error: K1Message) {
        let cell = self.megarepl_get_cell_mut(cell_id);
        cell.last_result = CellResult::error(error);
        cell.expr_id = None;
        cell.output_globals.clear();
        cell.last_ir.clear();
    }

    pub fn megarepl_get_cell(&self, cell_id: CellId) -> &MegareplCell {
        &self.megarepl.as_ref().unwrap().cells[cell_id as usize]
    }

    pub fn megarepl_get_cell_opt(&self, cell_id: CellId) -> Option<&MegareplCell> {
        self.megarepl.as_ref().unwrap().cells.get(cell_id as usize)
    }

    fn megarepl_get_cell_mut(&mut self, cell_id: CellId) -> &mut MegareplCell {
        &mut self.megarepl.as_mut().unwrap().cells[cell_id as usize]
    }

    fn megarepl_create_source(&mut self, cell_id: CellId, iteration: u32, code: String) -> FileId {
        let filename = format!("{}_repl_cell_{}.{}.k1", self.program_name(), cell_id, iteration);
        let source_id = self.ast.sources.add_file(crate::parse::SourceFile::make(
            0,
            self.config.out_dir.to_str().unwrap().to_owned(),
            filename,
            code,
        ));
        source_id
    }

    fn megarepl_new(&mut self, code: String) -> CellId {
        let mr = self.megarepl.as_mut().unwrap();
        let cell_id = mr.cells.len() as CellId;
        let source_id = self.megarepl_create_source(cell_id, 0, code);
        let mr = self.megarepl.as_mut().unwrap();
        mr.cells.push(MegareplCell {
            id: cell_id,
            is_watcher: false,
            watch_interval_ms: None,
            expr_id: None,
            iteration: 0,
            source_id,
            last_result: CellResult::error(make_warning("uninit", SpanId::NONE)),
            last_exec_time: None,
            output_globals: vec![],
            last_ir: String::new(),
        });
        if let Err(e) = self.megarepl_compile_source(cell_id) {
            self.megarepl_set_cell_compile_error(cell_id, e);
        }
        cell_id
    }

    // Have to compile, which sets expr
    // Then execute, which sets result
    fn megarepl_compile_source(&mut self, cell_id: CellId) -> K1Result<()> {
        let source = self.megarepl_get_cell(cell_id).source_id;
        let repl_source_result = match self.parse_repl_source(source) {
            Err(e) => {
                return Err(e);
            }
            Ok(r) => r,
        };

        match repl_source_result {
            ParseReplSourceResult::Stmts(stmts) => {
                let iteration = self.megarepl_get_cell(cell_id).iteration;
                match self.megarepl_compile_statements(stmts, cell_id, iteration) {
                    Err(k1_message) => Err(k1_message),
                    Ok((expr_id, output_globals)) => {
                        let cell = self.megarepl_get_cell_mut(cell_id);
                        cell.expr_id = Some(expr_id);
                        cell.output_globals = output_globals;
                        Ok(())
                    }
                }
            }
            ParseReplSourceResult::Defn(_parsed_id) => {
                eprintln!("skipping compile of definitions");
                Ok(())
            }
        }
    }

    pub fn megarepl(&self) -> &MegareplState {
        self.megarepl.as_ref().unwrap()
    }

    /// Runs `f` with the session's dedicated VM (not the typer's pooled VMs,
    /// which get reset between static evals and would lose the repl globals)
    fn megarepl_with_vm<T>(&mut self, f: impl FnOnce(&mut TypedProgram, &mut vm::Vm) -> T) -> T {
        let mut vm = self.megarepl_mut().vm.take().expect("megarepl vm already lent out");
        let result = f(self, &mut vm);
        self.megarepl_mut().vm = Some(vm);
        result
    }

    pub fn megarepl_mut(&mut self) -> &mut MegareplState {
        self.megarepl.as_mut().unwrap()
    }

    fn megarepl_execute_cell(&mut self, cell_id: CellId) {
        self.megarepl_execute_cell_solo(cell_id);

        // Take care not to re-run ourselves
        self.megarepl_run_observers(&[cell_id]);
    }

    // Watchers and widgets
    fn megarepl_run_observers(&mut self, skip_cells: &[CellId]) {
        let cells = self.megarepl.as_ref().unwrap().cells.iter().map(|c| c.id).collect_vec();
        for watcher_id in cells {
            if !skip_cells.contains(&watcher_id) && self.megarepl_get_cell(watcher_id).is_watcher {
                self.megarepl_execute_cell_solo(watcher_id);
            }
        }

        // Checkbox widgets are like watchers. Soon this will generalize... 'Control' widgets?
        // Vs Binding and Watcher which are display widgets
        let widgets = self.megarepl().widgets.clone();
        for (index, widget) in widgets.iter().enumerate() {
            match widget.data {
                WidgetDataSource::Checkbox { get_fn, .. } => {
                    let Ok(is_checked_value) = self.megarepl_with_vm(|k1, vm| {
                        bc::exec::execute_compiled_function(k1, vm, get_fn, &[], false)
                    }) else {
                        eprintln!("megarepl checkbox widget function exec failed");
                        continue;
                    };
                    let StaticValue::Bool(is_checked) = *self.static_values.get(is_checked_value)
                    else {
                        eprintln!("megarepl checkbox widget function returned non-boolean");
                        continue;
                    };
                    let WidgetDataSource::Checkbox { current_value, .. } =
                        &mut self.megarepl_mut().widgets[index].data
                    else {
                        panic!()
                    };
                    *current_value = is_checked;
                }
                _ => {}
            }
        }
    }

    fn megarepl_execute_cell_solo(&mut self, cell_id: CellId) {
        let cell = self.megarepl_get_cell(cell_id);
        let Some(cell_expr) = cell.expr_id else {
            eprintln!("nothing to execute");
            return;
        };
        let ir_string = ir::unit_to_string(self, IrUnitId::Expr(cell_expr), true);
        debug!("executing repl unit.\n{ir_string}");
        self.megarepl_get_cell_mut(cell_id).last_ir = ir_string;
        let output_globals = self.megarepl_get_cell(cell_id).output_globals.clone();
        let exec_start = std::time::Instant::now();

        let (exec_result, stdout, stderr, repl_commands) = self.megarepl_with_vm(|k1, vm| {
            let messages_start = vm.compiler_messages.len();
            vm.quiet_messages = true;
            let result = bc::exec::execute_compiled_expr(k1, vm, cell_expr, false);
            vm.quiet_messages = false;
            let (stdout, stderr) = vm::drain_captured_prints(k1, vm, messages_start);
            let repl_commands = std::mem::take(&mut vm.repl_commands);
            let result = result.and_then(|value| {
                let mut outputs = Vec::with_capacity(output_globals.len());
                for (global_id, type_id) in &output_globals {
                    outputs.push(vm::read_global_as_static(k1, vm, *global_id, *type_id)?);
                }
                Ok((outputs, value))
            });
            (result, stdout, stderr, repl_commands)
        });
        for command in repl_commands {
            match command {
                vm::ReplCommand::Checkbox { name, get, set } => {
                    // Upsert by name: cells re-run (watchers, re-submits), so
                    // registration must converge, not accumulate
                    let existing = self.megarepl_mut().widgets.iter_mut().find(|w| {
                        matches!(w.data, WidgetDataSource::Checkbox { name: n, .. } if n == name)
                    });
                    match existing {
                        Some(widget) => {
                            let WidgetDataSource::Checkbox { current_value, .. } = widget.data
                            else {
                                unreachable!("matched checkbox above");
                            };
                            widget.data = WidgetDataSource::Checkbox {
                                name,
                                get_fn: get,
                                set_fn: set,
                                current_value,
                            };
                        }
                        None => {
                            self.megarepl_add_widget(WidgetDataSource::Checkbox {
                                name,
                                get_fn: get,
                                set_fn: set,
                                current_value: false,
                            });
                        }
                    }
                }
            }
        }
        let cell = self.megarepl_get_cell_mut(cell_id);
        cell.last_exec_time = Some(exec_start.elapsed());
        cell.last_result = match exec_result {
            Err(k1_message) => CellResult::Error { k1_message, stdout, stderr },
            Ok((outputs, value)) => CellResult::Expr { outputs, value, stdout, stderr },
        };
    }

    pub fn megarepl_toggle_watcher(&mut self, cell_id: CellId) -> bool {
        let cell = self.megarepl_get_cell_mut(cell_id);
        cell.is_watcher = !cell.is_watcher;
        cell.is_watcher
    }

    pub fn megarepl_set_watch_interval(&mut self, cell_id: CellId, ms: Option<u32>) {
        self.megarepl_get_cell_mut(cell_id).watch_interval_ms = ms;
    }

    pub fn megarepl_add_widget(&mut self, data: WidgetDataSource) -> WidgetId {
        let mr = self.megarepl.as_mut().unwrap();
        let id = mr.next_widget_id;
        mr.next_widget_id += 1;
        mr.widgets.push(MegareplWidget { id, data });
        id
    }

    pub fn megarepl_remove_widget(&mut self, widget_id: WidgetId) {
        self.megarepl.as_mut().unwrap().widgets.retain(|w| w.id != widget_id);
    }

    /// Widget ids are monotonic, not indices; deletion leaves holes
    pub fn megarepl_widget_opt(&self, widget_id: WidgetId) -> Option<&MegareplWidget> {
        self.megarepl().widgets.iter().find(|w| w.id == widget_id)
    }

    // Only checkbox control for now
    pub fn megarepl_send_control(&mut self, widget_id: WidgetId, value: bool) {
        let Some(widget) = self.megarepl_widget_opt(widget_id) else {
            return;
        };
        let WidgetDataSource::Checkbox { set_fn, .. } = widget.data else {
            return;
        };
        let static_bool =
            if value { self.static_values.true_id() } else { self.static_values.false_id() };
        let result = self.megarepl_with_vm(|k1, vm| {
            bc::exec::execute_compiled_function(k1, vm, set_fn, &[static_bool], false)
        });
        if let Err(e) = result {
            eprintln!("megarepl checkbox set failed: {}", e.message);
        }

        self.megarepl_run_observers(&[]);
        self.megarepl_refresh_globals();
    }

    pub fn megarepl_resolve_binding(&self, name: StringId) -> Option<&MegareplGlobal> {
        let mr = self.megarepl.as_ref().unwrap();
        let Some(VariableInScope::Defined(variable_id)) =
            self.scopes.find_variable_local(mr.ns_scope, name)
        else {
            return None;
        };
        let VariableKind::Global(global_id) = self.variables.get(variable_id).kind else {
            return None;
        };
        mr.globals.iter().find(|g| g.global_id == global_id)
    }

    fn megarepl_make_global(
        &mut self,
        name: StringId,
        type_id: TypeId,
        span: SpanId,
    ) -> (TypedGlobalId, VariableId) {
        let repl_ns_scope = self.megarepl.as_ref().unwrap().ns_scope;
        let global_id = self.globals.next_id();
        let variable_id = self.variables.add(Variable {
            name,
            type_id,
            owner_scope: repl_ns_scope,
            flags: VariableFlags::Reassigned,
            usage_count: 0,
            usages: vec![],
            kind: VariableKind::Global(global_id),
            defn_span: span,
        });
        let global_id = self.globals.add_expected_id(
            TypedGlobal {
                variable_id,
                parsed_expr: None,
                initial_value: GlobalInitialValue::Uninit,
                type_id,
                span,
                is_constant: false,
                is_tls: false,
                is_exported: false,
                is_external: false,
                ast_id: ParsedGlobalId::PENDING,
                parent_scope: repl_ns_scope,
            },
            global_id,
        );
        self.megarepl
            .as_mut()
            .unwrap()
            .globals
            .push(MegareplGlobal { global_id, last_value: None });
        (global_id, variable_id)
    }

    fn megarepl_compile_statements(
        &mut self,
        stmts: List<ParsedStmtId, ParsedProgram>,
        cell_id: CellId,
        iteration: u32,
    ) -> K1Result<(TypedExprId, Vec<(TypedGlobalId, TypeId)>)> {
        let span = stmts.first().map(|s| self.ast.get_stmt_span(*s)).unwrap_or(SpanId::NONE);
        let repl_ns_scope = self.megarepl.as_ref().unwrap().ns_scope;
        let mut output_globals: Vec<(TypedGlobalId, TypeId)> = vec![];
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
                        Some(expr) => {
                            Some(self.eval_expr(expr, ctx.with_expected_type(expected_type))?)
                        }
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
                    let name = parsed_let.name;

                    let existing_global = match self.scopes.find_variable_local(repl_ns_scope, name)
                    {
                        // A changed type will shadow
                        Some(VariableInScope::Defined(variable_id))
                            if self.variables.get(variable_id).type_id == type_id =>
                        {
                            Some(variable_id)
                        }
                        _ => None,
                    };
                    let variable_id = match existing_global {
                        Some(variable_id) => variable_id,
                        None => {
                            let (_global_id, variable_id) =
                                self.megarepl_make_global(name, type_id, parsed_let.span);
                            self.scopes.add_variable(repl_ns_scope, name, variable_id);
                            variable_id
                        }
                    };

                    // Global exists. Now at this position in the block, we do
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
                ParsedStmt::LoneExpression(_) => {
                    let Some(stmt_id) = self.eval_stmt(*stmt, ctx, false, index)? else {
                        continue;
                    };
                    // Intermediate expressions with a runtime value get captured
                    // into hidden globals so the repl can show every output, not
                    // just the final one. Unit and never values are skipped (unit
                    // has a physical type, but an empty one). The last expression
                    // stays put; it becomes the cell's return value below.
                    let is_last = index + 1 == stmts.len();
                    let mut captured = None;
                    if !is_last {
                        if let TypedStmt::Expr(expr_id, expr_type) = self.stmts.get(stmt_id) {
                            let (expr_id, expr_type) = (*expr_id, *expr_type);
                            let has_value = match self
                                .types
                                .get_physical_type(&self.static_values, expr_type)
                            {
                                PhysicalTypeResult::Yes(pt) => !pt.is_empty(),
                                _ => false,
                            };
                            if has_value {
                                captured = Some((expr_id, expr_type));
                            }
                        }
                    }
                    match captured {
                        None => self.push_block_stmt_id(&mut cell_block, stmt_id),
                        Some((expr_id, expr_type)) => {
                            let expr_span = self.exprs.get_span(expr_id);
                            let name = self.ast.idents.intern(format!(
                                "__cell{}_it{}_out{}",
                                cell_id,
                                iteration,
                                output_globals.len()
                            ));
                            let (global_id, variable_id) =
                                self.megarepl_make_global(name, expr_type, expr_span);
                            let variable_expr = self.synth_variable_expr(variable_id, SpanId::NONE);
                            let assign_stmt =
                                self.stmts.add(TypedStmt::Assignment(AssignmentStmt {
                                    destination: variable_expr,
                                    value: expr_id,
                                    span: expr_span,
                                    kind: AssignmentKind::Set,
                                }));
                            self.push_block_stmt_id(&mut cell_block, assign_stmt);
                            output_globals.push((global_id, expr_type));
                        }
                    }
                }
                ParsedStmt::Use(_)
                | ParsedStmt::Require(_)
                | ParsedStmt::Assign(_)
                | ParsedStmt::Store(_)
                | ParsedStmt::Defer(_) => {
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
        Ok((cell_expr, output_globals))
    }

    pub fn ensure_megarepl_session(&mut self) {
        if self.megarepl.is_some() {
            return;
        }
        // Obscure name so we stay clear of the program's own namespaces
        // (`megarepl` itself is fair game for user code); if it somehow
        // exists anyway, extend it
        let name = self.ast.idents.intern("_repl_session");
        let module_id = self.module_in_progress.unwrap();
        let module = self.modules.get(module_id);
        let module_ns_scope = module.namespace_scope_id;
        let (ns_id, ns_scope_id, parsed_ns) =
            match self.scopes.find_namespace_local(module_ns_scope, name) {
                Some(ns_id) => {
                    let ns = self.namespaces.get(ns_id);
                    (ns_id, ns.scope_id, ns.parsed_id.as_namespace_id().unwrap())
                }
                None => {
                    let parsed_namespace_id =
                        self.ast.namespaces.add(parse::ParsedNamespace::empty(name));
                    let module_ns_id = module.namespace_id;
                    let ns_scope_id = self.scopes.add_child_scope(
                        module_ns_scope,
                        ScopeType::Namespace,
                        ScopeOwnerId::None,
                    );
                    let ns_id = self.namespaces.add(Namespace {
                        name,
                        scope_id: ns_scope_id,
                        namespace_type: NamespaceKind::User,
                        companion_type_id: None,
                        parent_id: Some(module_ns_id),
                        owner_module: Some(module_id),
                        parsed_id: ParsedId::Namespace(parsed_namespace_id),
                    });
                    self.scopes.set_scope_owner_id(ns_scope_id, ScopeOwnerId::Namespace(ns_id));
                    let added = self.scopes.add_namespace(module_ns_scope, name, ns_id);
                    debug_assert!(added, "megarepl ns name was free; we just checked");
                    (ns_id, ns_scope_id, parsed_namespace_id)
                }
            };
        self.megarepl = Some(MegareplState {
            parsed_ns,
            ns: ns_id,
            ns_scope: ns_scope_id,
            cells: vec![],
            globals: vec![],
            widgets: vec![],
            next_widget_id: 0,
            vm: Some(vm::Vm::make()),
        });
    }

    pub fn megarepl_static_value_to_string(&self, value_id: StaticValueId) -> String {
        self.static_value_to_string_ext(value_id, true)
    }
}
