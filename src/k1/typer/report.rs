// Copyright (c) 2026 knix
// All rights reserved.

use super::*;

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

    // Timing
    //
    pub fn print_timing_info(
        &self,
        full_elapsed_ns: u64,
        out: &mut impl std::io::Write,
    ) -> std::io::Result<()> {
        let infer_ms = self.timing.total_infer_nanos as f64 / 1_000_000.0;
        let vm_ms = self.timing.total_vm_nanos as f64 / 1_000_000.0;
        let lines: usize = self.ast.sources.iter().map(|s| s.1.line_count(&self.ast.mem)).sum();
        // mm lines per ns, aka lines per second
        let lines_per_s = if lines > 0 { lines as f64 * 1e9 / full_elapsed_ns as f64 } else { 0.0 };
        eprintln!(
            "program {} took {}ms ({:.2} line/s, {} lines)",
            self.ast.name_str(),
            full_elapsed_ns / 1_000_000,
            lines_per_s,
            lines
        );
        eprintln!("\t{} expressions", self.exprs.len());
        eprintln!("\t{} statements", self.stmts.len());
        eprintln!("\t{} functions", self.functions.len());
        eprintln!("\t{} types", self.type_count());
        eprintln!("\t{} idents", self.ast.idents.len());
        let iropt_created = self.timing.iropt_insts_created;
        let irgen_created = self.ir.instrs.len() as i64 - iropt_created;
        eprintln!(
            "\t{} instructions ({} irgen + {} iropt, {:.2}x), {}ms ir, {}ms iropt, {:.2}ms bcgen ({} code words)",
            self.ir.instrs.len(),
            irgen_created,
            iropt_created,
            self.ir.instrs.len() as f64 / irgen_created.max(1) as f64,
            self.timing.total_ir_nanos / 1_000_000,
            self.timing.total_iropt_nanos / 1_000_000,
            self.timing.total_bcgen_nanos as f64 / 1_000_000.0,
            self.bc.code.len(),
        );
        eprintln!(
            "\t  iropt: {:.2}ms inline ({} inlines), {:.2}ms simplify ({} passes), {:.2}ms cfg ({} computes; nested in the others)",
            self.timing.iropt_inline_nanos as f64 / 1_000_000.0,
            self.timing.iropt_inline_count,
            self.timing.iropt_simplify_nanos as f64 / 1_000_000.0,
            self.timing.iropt_simplify_passes,
            self.timing.iropt_cfg_nanos as f64 / 1_000_000.0,
            self.timing.iropt_cfg_computes,
        );
        self.tmp.print_usage("\ttmp");
        self.mem.print_usage("\tperm");
        self.ir.mem.print_usage("\tmem ir");
        #[cfg(feature = "profile")]
        crate::kmem::print_stranded_counters();
        writeln!(
            out,
            "\t{} infers: {:.2}ms. avg: {:.2}ms. {} static execs during inference: {:.2}ms",
            self.timing.total_infers,
            infer_ms,
            if self.timing.total_infers > 0 {
                infer_ms / self.timing.total_infers as f64
            } else {
                0.0
            },
            self.timing.total_infer_execs,
            self.timing.total_infer_exec_nanos as f64 / 1_000_000.0,
        )?;
        let vm_us = self.timing.total_vm_nanos as f64 / 1_000.0;
        let vm_us_per_instr = vm_us / self.timing.total_vm_instrs as f64;

        writeln!(out, "\t{:.2}ms vm, avg {:.2}us/instr", vm_ms, vm_us_per_instr)?;
        let total_ops: i64 = self.timing.opcode_counts.iter().sum();
        if total_ops > 0 {
            let mut counts: Vec<(crate::bc::Opcode, i64)> =
                Vec::with_capacity(self.timing.opcode_counts.len());
            for (i, n) in self.timing.opcode_counts.iter().enumerate() {
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
        writeln!(
            out,
            "\t{:.2}ms ferry (static_value <-> vm memory)",
            self.timing.total_ferry_nanos as f64 / 1_000_000.0
        )?;
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
