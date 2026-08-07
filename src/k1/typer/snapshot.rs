// Copyright (c) 2026 knix

use crate::snap::{SnapReader, SnapWriter, restore_map_snap, write_map_snap};

use super::*;

static_assert_size!(K1Message, 12);
static_assert_size!(AbilitySpec9nInfo, 16);
static_assert_size!(SourceFileHash, 16);
static_assert_size!(NameInNamespace, 8);

impl TypedProgram {
    pub fn snap(&self) -> Vec<u8> {
        let TypedProgram {
            modules,
            module_order,
            config,
            program_settings,
            ast,
            functions,
            variables,
            types,
            type_hashes,
            type_variable_counts,
            type_instance_info,
            type_defn_info,
            type_specializations,
            phys_types,
            hole_type_cache: _,
            ast_ability_mapping,
            builtin_types,
            agg_types,
            lambda_types,
            type_idents,
            globals,
            exprs,
            calls,
            stmts,
            static_values,
            type_schemas,
            type_names,
            scopes,
            messages,
            namespaces,
            abilities,
            ability_impls,
            ability_impl_table,
            blanket_impls,
            function_name_to_ability,
            namespace_type_params,
            namespace_ast_mappings,
            function_ast_mappings,
            macro_ast_mappings,
            global_ast_mappings,
            globals_in_progress,
            ability_impl_ast_mappings,
            debug_level_stack: _,
            functions_pending_body_specialization,
            uses_pending_resolution,
            types_pending_definition,
            module_in_progress,
            ls_entities: _,
            completion: _,
            lsp,
            inference_context_stack,
            inference_context_extras: _,
            type_defn_context,
            buffers: _,
            patterns,
            pattern_ctors,
            vm: _,
            vm_alts: _,
            emitted_sources,
            vm_shared_static_stack: _,
            vm_global_constant_lookups: _,
            vm_static_value_lookups: _,
            vm_process_dlopen_handle: _,
            vm_dylib_handles: _,
            vm_ffi_functions: _,
            mem,
            tmp: _,
            ir,
            bc: _,
            timing: _,
            global_id_k1_arena,
            megarepl,
        } = self;
        assert!(megarepl.is_none(), "cannot snapshot a megarepl session");
        assert!(inference_context_stack.is_empty(), "cannot snapshot mid-inference");
        assert!(
            lsp.source_overrides.is_empty() && !lsp.completion,
            "cannot snapshot an LSP session"
        );

        let mut w = SnapWriter::new();
        w.write_section("config");
        w.write_t(config);
        ast.snap(&mut w);
        let w = &mut w;
        w.write_section("typed");
        mem.snap(w);
        modules.snap(w);
        w.write_slice(module_order);
        w.write_t(program_settings);
        functions.snap(w);
        variables.snap(w);
        types.snap(w);
        write_map_snap(w, type_hashes);
        type_variable_counts.snap(w);
        type_instance_info.snap(w);
        write_map_snap(w, type_defn_info);
        w.sorted_entries(type_specializations.iter(), |w, e| w.write_t(e));
        write_map_snap(w, phys_types);
        write_map_snap(w, ast_ability_mapping);
        w.write_t(builtin_types);
        agg_types.snap(w);
        lambda_types.snap(w);
        w.write_t(type_idents);
        globals.snap(w);
        let TypedExprPool { exprs: e_exprs, type_ids: e_type_ids, spans: e_spans } = exprs;
        e_exprs.snap(w);
        e_type_ids.snap(w);
        e_spans.snap(w);
        calls.snap(w);
        stmts.snap(w);
        static_values.snap(w);
        write_map_snap(w, type_schemas);
        write_map_snap(w, type_names);
        scopes.snap(w);
        w.write_slice(&messages.borrow());
        namespaces.namespaces.snap(w);
        abilities.snap(w);
        ability_impls.snap(w);
        write_map_snap(w, ability_impl_table);
        write_map_snap(w, blanket_impls);
        write_map_snap(w, function_name_to_ability);
        write_map_snap(w, namespace_type_params);
        write_map_snap(w, namespace_ast_mappings);
        write_map_snap(w, function_ast_mappings);
        write_map_snap(w, macro_ast_mappings);
        write_map_snap(w, global_ast_mappings);
        write_map_snap(w, ability_impl_ast_mappings);

        assert!(globals_in_progress.is_empty());
        assert!(functions_pending_body_specialization.is_empty());
        assert!(uses_pending_resolution.is_empty());
        assert!(types_pending_definition.is_empty());
        assert!(module_in_progress.is_none());
        let TypeDefnContext { stack, recursive_mentions, pending_instances, completed } =
            type_defn_context;
        assert!(stack.is_empty());
        assert!(recursive_mentions.is_empty());
        assert!(pending_instances.is_empty());
        assert!(completed.is_empty());

        patterns.mem.snap(w);
        pattern_ctors.snap(w);
        w.write_slice(emitted_sources);
        ir.snap(w);
        w.write_t(global_id_k1_arena);
        w.write_section("end");
        std::mem::take(&mut w.buf)
    }

    pub fn restore(bytes: &[u8]) -> Result<TypedProgram, String> {
        let mut reader = SnapReader::new(bytes)?;
        let r = &mut reader;
        r.section("config");
        let config: CompilerConfig = r.read_t();
        let ast = ParsedProgram::restore(r);
        let mut k1 =
            TypedProgram::new(ast, config, crate::compiler::LspCompileOptions::default());
        r.section("typed");
        k1.mem.restore(r);
        k1.modules.restore(r);
        k1.module_order = r.read_vec();
        k1.program_settings = r.read_t();
        k1.functions.restore(r);
        k1.variables.restore(r);
        k1.types.restore(r);
        k1.type_hashes = restore_map_snap(r);
        k1.type_variable_counts.restore(r);
        k1.type_instance_info.restore(r);
        k1.type_defn_info = restore_map_snap(r);
        for _ in 0..r.read_len() {
            let TypeSpecialization { base, args, specialized } = r.read_t();
            k1.insert_specialization(base, args, specialized);
        }
        k1.phys_types = restore_map_snap(r);
        k1.ast_ability_mapping = restore_map_snap(r);
        k1.builtin_types = r.read_t();
        k1.agg_types.restore(r);
        k1.lambda_types.restore(r);
        k1.type_idents = r.read_t();
        k1.globals.restore(r);
        k1.exprs.exprs.restore(r);
        k1.exprs.type_ids.restore(r);
        k1.exprs.spans.restore(r);
        k1.calls.restore(r);
        k1.stmts.restore(r);
        k1.static_values.restore(r);
        k1.type_schemas = restore_map_snap(r);
        k1.type_names = restore_map_snap(r);
        k1.scopes = Scopes::restore(r);
        k1.messages = RefCell::new(r.read_vec());
        k1.namespaces.namespaces.restore(r);
        k1.abilities.restore(r);
        k1.ability_impls.restore(r);
        k1.ability_impl_table = restore_map_snap(r);
        k1.blanket_impls = restore_map_snap(r);
        k1.function_name_to_ability = restore_map_snap(r);
        k1.namespace_type_params = restore_map_snap(r);
        k1.namespace_ast_mappings = restore_map_snap(r);
        k1.function_ast_mappings = restore_map_snap(r);
        k1.macro_ast_mappings = restore_map_snap(r);
        k1.global_ast_mappings = restore_map_snap(r);
        k1.ability_impl_ast_mappings = restore_map_snap(r);
        k1.patterns.mem.restore(r);
        k1.pattern_ctors.restore(r);
        k1.emitted_sources = r.read_vec();
        k1.ir.restore(r);
        k1.global_id_k1_arena = r.read_t();
        r.section("end");
        assert!(r.is_done(), "snapshot has {} trailing bytes", bytes.len() - r.pos());
        Ok(k1)
    }

    #[cfg(debug_assertions)]
    pub(crate) fn debug_snapshot_roundtrip(&mut self) {
        if self.megarepl.is_some()
            || self.lsp.completion
            || !self.lsp.source_overrides.is_empty()
            || !self.ast.errors.is_empty()
        {
            return;
        }
        let snap_start = std::time::Instant::now();
        let first = self.snap();
        eprintln!(
            "snapshot is {}mb ({:?})",
            first.len() as f64 / (1024.0 * 1024.0),
            snap_start.elapsed()
        );
        let mut restored = match TypedProgram::restore(&first) {
            Ok(restored) => restored,
            Err(e) => panic!("snapshot restore failed: {e}"),
        };
        let second = restored.snap();
        crate::snap::assert_identical(&first, &second, "TypedProgram snapshot roundtrip");
        std::mem::swap(self, &mut restored);
    }
}
