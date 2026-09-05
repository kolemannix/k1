// Copyright (c) 2026 knix
// All rights reserved.

use super::*;
use crate::typer::trace::{
    FRAME_FLAG_EXPR_UNIT, FrameId, PASS_NAMES, RESTORE_SECTIONS, TraceFrame, TraceKind,
};
use fxhash::FxHashMap;

impl TypedProgram {
    pub fn get_span_location(&self, span: SpanId) -> (&parse::SourceFile, parse::Line) {
        let the_span = self.ast.spans.get(span);
        let source = self.ast.sources.get(the_span.file_id);
        let line = source.get_line_for_span_start(&self.ast.mem, the_span).unwrap();
        (source, line)
    }

    pub fn write_scope_path<W: std::fmt::Write + ?Sized>(
        &self,
        w: &mut W,
        scope: ScopeId,
        delimiter: &str,
        skip_root: bool,
    ) {
        let starting_namespace = self.scopes.nearest_parent_namespace(scope);
        let mut chain: SmallVec<[StringId; 8]> = smallvec![];
        let mut ns_id = starting_namespace;
        loop {
            let namespace = self.namespaces.get(ns_id);
            chain.push(namespace.name);
            match namespace.parent_id {
                Some(parent_id) => ns_id = parent_id,
                None => break,
            }
        }
        for (i, name) in chain.iter().rev().enumerate() {
            let ident_str = self.ident_str(*name);
            let is_last = i == chain.len() - 1;

            let is_root = ident_str == "_root";
            if !(is_root && skip_root) {
                write!(w, "{ident_str}").unwrap();
                if !is_last {
                    write!(w, "{delimiter}").unwrap();
                }
            }
        }
    }

    pub fn write_qualified_name(
        &self,
        w: &mut impl std::fmt::Write,
        scope: ScopeId,
        name: &str,
        suffix_id: Option<usize>,
        delimiter: &str,
        skip_root: bool,
    ) {
        self.write_scope_path(w, scope, delimiter, skip_root);
        write!(w, "{delimiter}").unwrap();
        write!(w, "{}", name).unwrap();
        if let Some(suffix_id) = suffix_id {
            write!(w, "_{suffix_id}").unwrap()
        }
    }

    pub fn make_qualified_name(
        &self,
        scope: ScopeId,
        name: StringId,
        suffix_id: Option<usize>,
        delimiter: &str,
        skip_root: bool,
    ) -> String {
        let mut buf = String::with_capacity(64);
        self.write_qualified_name(
            &mut buf,
            scope,
            self.ident_str(name),
            suffix_id,
            delimiter,
            skip_root,
        );
        buf
    }

    /// `impl_<ability>.<fn>_for_t<self>` for ability impl fns, the plain name otherwise
    pub fn write_function_name(&self, w: &mut impl std::fmt::Write, function_id: FunctionId) {
        let function = self.get_function(function_id);
        if let TypedFunctionKind::AbilityImpl(ability_id, self_type_id) = function.kind {
            write!(
                w,
                "impl_{}{}.{}_for_t{}",
                ability_id.as_u32(),
                self.ident_str(self.abilities.get(ability_id).name),
                self.ident_str(function.name),
                self_type_id.as_u32()
            )
            .unwrap();
        } else {
            write!(w, "{}", self.ident_str(function.name)).unwrap();
        }
    }

    /// Scope path, derived name, and the function id: unique per function
    pub fn function_symbol_name(&self, function_id: FunctionId) -> String {
        let mut s = String::with_capacity(64);
        self.write_scope_path(&mut s, self.get_function(function_id).scope, ".", true);
        s.push('.');
        self.write_function_name(&mut s, function_id);
        write!(s, "_{}", function_id.as_u32()).unwrap();
        s
    }

    pub fn global_link_symbol(&self, global: &TypedGlobal) -> String {
        let variable = self.variables.get(global.variable_id);
        if let Some(link_name) = global.link_name {
            return self.ident_str(link_name).to_string();
        }
        if global.is_exported || global.is_external {
            return self.ident_str(variable.name).to_string();
        }
        self.make_qualified_name(variable.owner_scope, variable.name, None, "__", false)
    }

    // Errors and logging

    pub fn message_to_anyhow(&self, e: K1Message) -> anyhow::Error {
        anyhow::anyhow!("{}: {}", e.level, self.ident_str(e.message))
    }

    pub fn make_error(&self, message: impl AsRef<str>, span: SpanId) -> K1Message {
        make_message(&self.ast.idents, message, span, MessageLevel::Error)
    }

    pub fn make_warning(&self, message: impl AsRef<str>, span: SpanId) -> K1Message {
        make_message(&self.ast.idents, message, span, MessageLevel::Warn)
    }

    pub fn make_fail<A>(&self, message: impl AsRef<str>, span: SpanId) -> K1Result<A> {
        Err(self.make_error(message, span))
    }

    pub fn report(&mut self, e: K1Message) {
        self.report_ext(e, false)
    }

    /// `emitted_sources` is sorted by `file_id` (each emission registers a fresh,
    /// strictly later source file)
    pub(super) fn emitted_source_for_file(&self, file_id: FileId) -> Option<usize> {
        self.emitted_sources.binary_search_by_key(&file_id, |e| e.file_id).ok()
    }

    /// One step back through the emitted-file table covering `span`: the same
    /// position within the chunk's source span, or the whole source span when
    /// the emitted text outran it (escapes, dedent). None when `span` is not
    /// chunk text: a real file, or glue
    fn remap_span_hop(&self, span: Span) -> Option<(Span, &EmittedSource)> {
        let table = &self.emitted_sources[self.emitted_source_for_file(span.file_id)?];
        let entries = self.mem.getn(table.entries);
        let candidate = entries.partition_point(|entry| entry.start <= span.start);
        let &CodeChunkPos { start, end, source } = &entries[candidate.checked_sub(1)?];
        if span.start >= end {
            return None;
        }
        let source_span = self.ast.spans.get(source);
        let offset = span.start - start;
        let remapped = if offset < source_span.len {
            let len = span.len.min(end - span.start).min(source_span.len - offset);
            Span { file_id: source_span.file_id, start: source_span.start + offset, len }
        } else {
            source_span
        };
        Some((remapped, table))
    }

    pub fn remap_span(&self, mut span: Span) -> Span {
        for _ in 0..16 {
            let Some((remapped, _)) = self.remap_span_hop(span) else { break };
            span = remapped;
        }
        span
    }

    pub fn module_of_span(&self, span_id: SpanId) -> ModuleId {
        let span = self.remap_span(self.ast.spans.get(span_id));
        for module in self.modules.iter() {
            for file in module.source_file_hashes.as_slice(&self.mem) {
                if file.file_id == span.file_id {
                    return module.id;
                }
            }
        }
        self.primary_module().id
    }

    pub fn remap_to_source_span(&mut self, span_id: SpanId) -> SpanId {
        let span = self.ast.spans.get(span_id);
        let remapped = self.remap_span(span);
        if remapped == span { span_id } else { self.ast.spans.add(remapped) }
    }

    /// Follows argument text back to the call it was written in; template
    /// text and glue land outside the call span and yield None
    pub fn remap_call_arg_span(&self, mut span: Span) -> Option<Span> {
        for _ in 0..16 {
            if self.emitted_source_for_file(span.file_id).is_none() {
                return Some(span);
            }
            let (remapped, table) = self.remap_span_hop(span)?;
            let call = self.ast.spans.get(table.call_span);
            if remapped.file_id != call.file_id
                || remapped.start < call.start
                || remapped.end() > call.end()
            {
                return None;
            }
            span = remapped;
        }
        None
    }

    pub fn report_ext(&mut self, e: K1Message, no_print: bool) {
        let e = K1Message { span: self.remap_to_source_span(e.span), ..e };
        // Check for duplicates (happens a lot with generic code); remapping makes
        // fresh span ids, so spans compare by contents
        //
        // This will be kind quadraticy if we ever have a lot of diagnostics, which could
        // happen in happy path if we're doing a lot of hints
        let e_span = self.ast.spans.get(e.span);
        let is_duplicate = self.messages.borrow().iter().any(|m| {
            m.message == e.message
                && m.level == e.level
                && m.error_kind == e.error_kind
                && self.ast.spans.get(m.span) == e_span
        });
        if is_duplicate {
            return;
        }

        if let Some(table_index) = self.emitted_source_for_file(e_span.file_id) {
            if e.level == MessageLevel::Error {
                self.emitted_sources[table_index].has_diagnostic = true
            }
        };

        let skip_print = no_print || {
            if e.level != MessageLevel::Error {
                // Don't print warnings when errors are present
                let has_errors = self.error_count(&[MessageLevel::Error]) > 0;
                has_errors
            } else {
                false
            }
        };

        if !skip_print {
            self.trace_live_clear();
            let use_color = std::io::stderr().is_terminal();
            self.write_error(&mut std::io::stderr(), &e, use_color).unwrap();
        }
        self.messages.borrow_mut().push(e);
    }

    pub fn report_hint(&mut self, span: SpanId, message: impl AsRef<str>) {
        self.report(K1Message {
            message: self.ast.idents.intern(message),
            span,
            level: MessageLevel::Hint,
            error_kind: ErrorKind::None,
        });
    }

    pub fn report_warn(&mut self, span: SpanId, message: impl AsRef<str>) {
        self.report(K1Message {
            message: self.ast.idents.intern(message),
            span,
            level: MessageLevel::Warn,
            error_kind: ErrorKind::None,
        });
    }

    pub fn log_hint(&self, span: SpanId, message: impl AsRef<str>) {
        let use_color = std::io::stderr().is_terminal();
        let hint = K1Message {
            message: self.ast.idents.intern(message),
            span,
            level: MessageLevel::Hint,
            error_kind: ErrorKind::None,
        };
        self.write_error(&mut std::io::stderr(), &hint, use_color).unwrap();
    }

    pub fn write_error(
        &self,
        w: &mut impl std::io::Write,
        error: &K1Message,
        use_color: bool,
    ) -> std::io::Result<()> {
        write_error(
            w,
            &self.ast,
            self.ident_str(error.message),
            error.level,
            error.span,
            use_color,
        )?;
        let file_id = self.ast.spans.get(error.span).file_id;
        if let Some(emitted_index) = self.emitted_source_for_file(file_id) {
            writeln!(w, "note: in code compiled in place of this call:")?;
            self.write_location(w, self.emitted_sources[emitted_index].call_span, use_color);
        }
        Ok(())
    }

    pub fn write_location(&self, w: &mut impl std::io::Write, span: SpanId, use_color: bool) {
        parse::write_source_location(w, &self.ast, span, MessageLevel::Info, 6, None, use_color)
            .unwrap()
    }

    pub fn write_location_error(&self, w: &mut impl std::io::Write, span: SpanId, use_color: bool) {
        parse::write_source_location(w, &self.ast, span, MessageLevel::Error, 6, None, use_color)
            .unwrap()
    }

    #[track_caller]
    pub fn ice_span(&self, span: SpanId, msg: impl AsRef<str>) -> ! {
        let use_color = std::io::stderr().is_terminal();
        self.write_location_error(&mut std::io::stderr(), span, use_color);
        panic!("Internal Compiler Error: {}", msg.as_ref())
    }

    #[track_caller]
    pub fn ice(&self, msg: impl AsRef<str>, error: Option<&K1Message>) -> ! {
        let use_color = std::io::stderr().is_terminal();
        if let Some(error) = error {
            self.write_error(&mut std::io::stderr(), error, use_color).unwrap();
        }
        panic!("Internal Compiler Error at: {}", msg.as_ref())
    }

    /// "file.k1:line" for definitions; None for synthesized spans
    pub fn span_location(&self, span_id: SpanId) -> Option<String> {
        if span_id == SpanId::NONE {
            return None;
        }
        let span = self.ast.spans.get(span_id);
        let source = self.ast.sources.get(span.file_id);
        let (line, _) = self.ast.get_lines_for_span_id(span_id)?;
        Some(format!("{}:{}", source.filename_str(&self.ast.idents), line.line_number()))
    }

    fn unit_frame_label(&self, frame: &TraceFrame) -> String {
        if frame.is_expr_unit() {
            let expr_id = TypedExprId::from_u32(frame.key).unwrap();
            match self.span_location(self.exprs.get_span(expr_id)) {
                Some(loc) => format!("expr {loc}"),
                None => format!("expr {}", frame.key),
            }
        } else {
            self.specialization_label(FunctionId::from_u32(frame.key).unwrap())
        }
    }

    pub fn frame_label(&self, frame: &TraceFrame) -> String {
        let key = frame.key;
        let span_label = || {
            self.span_location(SpanId::from_u32(key).unwrap()).unwrap_or_else(|| "?".to_string())
        };
        let mut label = match frame.kind {
            TraceKind::ModuleDiscover => {
                self.ident_str(StringId::from_u32(key).unwrap()).to_string()
            }
            TraceKind::ModuleCompile | TraceKind::ModuleRead | TraceKind::SnapStore => {
                self.ident_str(self.modules.get(ModuleId::from_u32(key).unwrap()).name).to_string()
            }
            TraceKind::Parse => {
                let filename = self.ident_str(StringId::from_u32(key).unwrap());
                filename.to_string()
            }
            TraceKind::Lex => String::new(),
            TraceKind::SetupFn
            | TraceKind::TypeInfer
            | TraceKind::StaticExec
            | TraceKind::Metaprogram
            | TraceKind::VmValueFerry => span_label(),
            TraceKind::SnapRestore => format!("{key} modules"),
            TraceKind::SnapRestoreSection => RESTORE_SECTIONS[key as usize].to_string(),
            TraceKind::TyperPass => PASS_NAMES[key as usize].to_string(),
            TraceKind::SnapRoundtrip | TraceKind::Link | TraceKind::Archive => String::new(),
            TraceKind::FunctionTypecheck => {
                let function_id = FunctionId::from_u32(key).unwrap();
                match self.span_location(self.get_function_span(function_id)) {
                    Some(loc) => format!("{} {loc}", self.specialization_label(function_id)),
                    None => self.specialization_label(function_id),
                }
            }
            TraceKind::GlobalEval => {
                let global = self.globals.get(TypedGlobalId::from_u32(key).unwrap());
                self.ident_str(self.variables.get(global.variable_id).name).to_string()
            }
            TraceKind::FunctionSpecialize | TraceKind::MacroCall => {
                self.function_label(FunctionId::from_u32(key).unwrap())
            }
            TraceKind::TypeInstantiate => self.type_id_to_string(TypeId::from_u32(key).unwrap()),
            TraceKind::IrLower
            | TraceKind::IrOptimize
            | TraceKind::IrInline
            | TraceKind::IrSimplify
            | TraceKind::IrCfgCompute
            | TraceKind::Bcgen
            | TraceKind::VmRun => self.unit_frame_label(frame),
            TraceKind::CodegenPrepare | TraceKind::ReloadDylib => {
                if key == 0 {
                    "host".to_string()
                } else {
                    let ns = self.namespaces.get(NamespaceId::from_u32(key).unwrap());
                    self.ident_str(ns.name).to_string()
                }
            }
            TraceKind::Codegen => {
                if frame.data_count == 0 {
                    format!("{key} units")
                } else {
                    format!("unit {key}")
                }
            }
            TraceKind::LlvmPasses => match frame.parent_frame {
                Some(parent) if self.trace.frames.get(parent).kind == TraceKind::Codegen => {
                    format!("unit {key}")
                }
                _ => "merged".to_string(),
            },
            TraceKind::Thinlto => format!("{key} units"),
        };
        if frame.is_speculative() {
            label.push_str(" [spec]");
        }
        label
    }

    /// Who asked for this work: the frame that queued a drained unit, else the structural parent
    fn requester_of(&self, frame: &TraceFrame) -> Option<&TraceFrame> {
        let parent = self.trace.frames.get(frame.parent_frame?);
        if parent.kind == TraceKind::IrLower
            && let Some(requester) = parent.requester_frame
        {
            return Some(self.trace.frames.get(requester));
        }
        Some(parent)
    }

    /// Kind name and label, e.g. "typecheck main main.k1:3"
    pub fn frame_title(&self, frame: &TraceFrame) -> String {
        let label = self.frame_label(frame);
        if label.is_empty() {
            frame.kind.name().to_string()
        } else {
            format!("{} {label}", frame.kind.name())
        }
    }

    pub fn print_trace_summary(&self, out: &mut impl std::io::Write) -> std::io::Result<()> {
        let trace = &self.trace;
        let ms = |ticks: u64| trace.nanos(ticks) as f64 / 1e6;
        let wall_ns = trace.nanos(trace.wall_ticks());
        let lines: usize = self.ast.sources.iter().map(|s| s.1.line_count(&self.ast.mem)).sum();
        let lines_per_s = if lines > 0 { lines as f64 * 1e9 / wall_ns as f64 } else { 0.0 };
        writeln!(
            out,
            "program {} took {}ms ({:.2} line/s, {} lines)",
            self.ast.name_str(),
            wall_ns / 1_000_000,
            lines_per_s,
            lines
        )?;
        writeln!(out, "\t{} expressions", self.exprs.len())?;
        writeln!(out, "\t{} statements", self.stmts.len())?;
        writeln!(out, "\t{} functions", self.functions.len())?;
        writeln!(out, "\t{} types", self.type_count())?;
        writeln!(out, "\t{} idents", self.ast.idents.len())?;
        writeln!(
            out,
            "\t{} instructions, {} code words",
            self.ir.instrs.len(),
            self.bc.code.len()
        )?;
        self.tmp.print_usage("\ttmp");
        self.mem.print_usage("\tperm");
        self.ir.mem.print_usage("\tmem ir");
        #[cfg(feature = "profile")]
        crate::kmem::print_stranded_counters();
        if self.restored_module_count > 0 {
            let mut names: Vec<&str> = Vec::new();
            for module in self.modules.iter().take(self.restored_module_count as usize) {
                names.push(self.ident_str(module.name));
            }
            writeln!(out, "\trestored {} modules: {}", names.len(), names.join(", "))?;
        }

        let kind_count = TraceKind::ALL.len();
        let mut counts = vec![0u64; kind_count];
        let mut exclusive = vec![0u64; kind_count];
        let mut speculative = vec![0u64; kind_count];
        let mut data = vec![0u64; kind_count];
        let mut root_ticks = 0u64;
        let mut codegen_wall = 0u64;
        let mut codegen_cpu = 0u64;
        for frame in trace.frames.iter() {
            let i = frame.kind as usize;
            counts[i] += 1;
            exclusive[i] += frame.exclusive_ticks();
            data[i] += frame.data_count;
            if frame.is_speculative() {
                speculative[i] += frame.exclusive_ticks();
            }
            if frame.parent_frame.is_none() {
                root_ticks += frame.ticks();
            }
            if frame.kind == TraceKind::Codegen {
                if frame.data_count == 0 {
                    codegen_wall += frame.ticks();
                } else {
                    codegen_cpu += frame.ticks();
                }
            }
        }
        writeln!(out, "\t{:<18} {:>8} {:>10} {:>6}", "kind", "count", "excl ms", "speculative %")?;
        for kind in TraceKind::ALL {
            let i = kind as usize;
            if counts[i] == 0 {
                continue;
            }
            let spec_pct = if exclusive[i] > 0 {
                speculative[i] as f64 * 100.0 / exclusive[i] as f64
            } else {
                0.0
            };
            let mut line = format!(
                "\t{:<18} {:>8} {:>10.2} {:>5.0}%",
                kind.name(),
                counts[i],
                ms(exclusive[i]),
                spec_pct
            );
            if let Some(label) = kind.data_label() {
                line.push_str(&format!("  {} {}", human_count(data[i] as i64), label));
                if kind == TraceKind::VmRun && data[i] > 0 {
                    line.push_str(&format!(
                        " ({:.2}us/instr)",
                        trace.nanos(exclusive[i]) as f64 / 1e3 / data[i] as f64
                    ));
                }
            }
            if kind == TraceKind::Codegen {
                line.push_str(&format!(
                    "  wall {:.2}ms, cpu {:.2}ms",
                    ms(codegen_wall),
                    ms(codegen_cpu)
                ));
            }
            writeln!(out, "{line}")?;
        }
        let untracked_ns = wall_ns.saturating_sub(trace.nanos(root_ticks));
        writeln!(out, "\t{:<18} {:>8} {:>10.2}", "untracked", "", untracked_ns as f64 / 1e6)?;

        let mut by_exclusive: Vec<(u64, FrameId)> = Vec::with_capacity(trace.frames.len());
        for (id, frame) in trace.frames.iter_with_ids() {
            by_exclusive.push((frame.exclusive_ticks(), id));
        }
        by_exclusive.sort_by_key(|(ticks, _)| std::cmp::Reverse(*ticks));
        writeln!(out, "\ttop frames:")?;
        for (ticks, id) in by_exclusive.iter().take(10) {
            let frame = trace.frames.get(*id);
            writeln!(
                out,
                "\t  {:>8.2}ms ({:>6.0}ms incl) {}",
                ms(*ticks),
                ms(frame.ticks()),
                self.frame_title(frame)
            )?;
        }

        let mut by_generic: FxHashMap<u32, (u64, u64)> = FxHashMap::default();
        let mut by_requester: FxHashMap<(TraceKind, u32, u8), (u64, u64)> = FxHashMap::default();
        for frame in trace.frames.iter() {
            match frame.kind {
                TraceKind::FunctionSpecialize => {
                    let entry = by_generic.entry(frame.key).or_insert((0, 0));
                    entry.0 += 1;
                    entry.1 += frame.ticks();
                    if let Some(requester) = self.requester_of(frame) {
                        let unit_flag = requester.flags & FRAME_FLAG_EXPR_UNIT;
                        let entry = by_requester
                            .entry((requester.kind, requester.key, unit_flag))
                            .or_insert((0, 0));
                        entry.0 += 1;
                        entry.1 += frame.ticks();
                    }
                }
                TraceKind::FunctionTypecheck => {
                    let function = self.get_function(FunctionId::from_u32(frame.key).unwrap());
                    if let Some(info) = &function.specialization_info {
                        let entry =
                            by_generic.entry(info.parent_function.as_u32()).or_insert((0, 0));
                        entry.1 += frame.ticks();
                        if let Some(requester) = self.requester_of(frame) {
                            let unit_flag = requester.flags & FRAME_FLAG_EXPR_UNIT;
                            let entry = by_requester
                                .entry((requester.kind, requester.key, unit_flag))
                                .or_insert((0, 0));
                            entry.1 += frame.ticks();
                        }
                    }
                }
                _ => {}
            }
        }
        let mut generics: Vec<(u32, (u64, u64))> = by_generic.into_iter().collect();
        generics.sort_by_key(|(_, (_, ticks))| std::cmp::Reverse(*ticks));
        if !generics.is_empty() {
            writeln!(out, "\ttop generics by specialization + body time:")?;
            for (key, (count, ticks)) in generics.iter().take(10) {
                writeln!(
                    out,
                    "\t  {:>8.2}ms {:>5} instances {}",
                    ms(*ticks),
                    count,
                    self.function_label(FunctionId::from_u32(*key).unwrap())
                )?;
            }
        }
        let mut requesters: Vec<((TraceKind, u32, u8), (u64, u64))> =
            by_requester.into_iter().collect();
        requesters.sort_by_key(|(_, (_, ticks))| std::cmp::Reverse(*ticks));
        if !requesters.is_empty() {
            writeln!(out, "\ttop requesters of specializations (declaration + body time):")?;
            for ((kind, key, unit_flag), (count, ticks)) in requesters.iter().take(10) {
                let probe = TraceFrame {
                    clock_start: 0,
                    clock_end: 0,
                    child_ticks: 0,
                    data_count: 0,
                    key: *key,
                    parent_frame: None,
                    requester_frame: None,
                    kind: *kind,
                    flags: *unit_flag,
                };
                writeln!(
                    out,
                    "\t  {:>8.2}ms {:>5} declared by {}",
                    ms(*ticks),
                    count,
                    self.frame_title(&probe)
                )?;
            }
        }

        let total_ops: i64 = trace.opcode_counts.iter().sum();
        if total_ops > 0 {
            let mut counts: Vec<(crate::bc::Opcode, i64)> =
                Vec::with_capacity(trace.opcode_counts.len());
            for (i, n) in trace.opcode_counts.iter().enumerate() {
                counts.push((crate::bc::Opcode::from_u8(i as u8), *n));
            }
            counts.sort_by_key(|(_, n)| std::cmp::Reverse(*n));
            let mut parts = Vec::with_capacity(counts.len());
            let mut other = 0i64;
            for (op, n) in counts {
                let pct = n as f64 * 100.0 / total_ops as f64;
                if pct >= 1.0 {
                    parts.push(format!("{} {:.0}% ({})", op.name(), pct, human_count(n)));
                } else {
                    other += n;
                }
            }
            if other > 0 {
                parts.push(format!(
                    "other {:.0}% ({})",
                    other as f64 * 100.0 / total_ops as f64,
                    human_count(other)
                ));
            }
            writeln!(out, "\t  ops: {}", parts.join(", "))?;
        }
        Ok(())
    }

    /// Folded stacks, one line per frame: `root;...;frame <exclusive nanos>`
    pub fn write_trace_folded(&self, out: &mut impl std::io::Write) -> std::io::Result<()> {
        let trace = &self.trace;
        let mut labels: Vec<String> = Vec::with_capacity(trace.frames.len());
        for frame in trace.frames.iter() {
            labels.push(self.frame_title(frame).replace(';', ","));
        }
        let mut path: Vec<u32> = Vec::with_capacity(32);
        for (id, frame) in trace.frames.iter_with_ids() {
            path.clear();
            path.push(id.as_u32());
            let mut cursor = frame.parent_frame;
            while let Some(parent) = cursor {
                path.push(parent.as_u32());
                cursor = trace.frames.get(parent).parent_frame;
            }
            let mut line = String::new();
            for (i, key) in path.iter().rev().enumerate() {
                if i > 0 {
                    line.push(';');
                }
                line.push_str(&labels[*key as usize - 1]);
            }
            writeln!(out, "{line} {}", trace.nanos(frame.exclusive_ticks()))?;
        }
        Ok(())
    }
}

fn human_count(n: i64) -> String {
    if n >= 1_000_000 {
        format!("{:.1}M", n as f64 / 1e6)
    } else if n >= 10_000 {
        format!("{}k", n / 1000)
    } else {
        n.to_string()
    }
}
