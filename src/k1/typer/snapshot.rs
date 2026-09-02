// Copyright (c) 2026 knix

use crate::snap::{SnapReader, SnapWriter, restore_map_snap, write_map_snap};

use super::*;

static_assert_size!(K1Message, 12);
static_assert_size!(AbilitySpec9nInfo, 12);
static_assert_size!(SourceFileHash, 16);
static_assert_size!(NameInNamespace, 8);

pub(crate) fn inputs_hash_from_settings(
    idents: &IdentPool,
    config: &crate::compiler::CompilerConfig,
) -> crate::snap::InputsHash {
    let crate::compiler::CompilerConfig {
        src_path,
        home_dir,
        k1_home,
        is_test_build,
        no_std,
        target,
        simd_bytes,
        debug,
        sanitize,
        filc,
        out_dir,
        out_dir_generated: _,
        cache_dir: _,
        optimize,
        chatty: _,
        optimize_ir,
        cache: _,
        setup_mode,
    } = config;
    let flags = [
        *is_test_build,
        *no_std,
        *debug,
        *sanitize,
        *filc,
        *optimize,
        *optimize_ir,
        cfg!(feature = "lsp"),
    ]
    .map(|b| b as u8);
    let setup_mode = match setup_mode {
        crate::compiler::SetupMode::Normal => 0u8,
        crate::compiler::SetupMode::SetupOnly { .. } => 2,
    };
    crate::snap::InputsHash(0).add(&[
        crate::BUILD_ID.as_bytes(),
        idents.get_string(*src_path).as_bytes(),
        idents.get_string(*home_dir).as_bytes(),
        idents.get_string(*k1_home).as_bytes(),
        idents.get_string(*out_dir).as_bytes(),
        target.to_str().as_bytes(),
        &simd_bytes.to_le_bytes(),
        &flags,
        &[setup_mode],
    ])
}

impl TypedProgram {
    pub fn snap(&self) -> crate::snap::SnapBytes {
        let mut w = SnapWriter::new();
        self.snap_into(&mut w);
        w.finish()
    }

    pub fn snap_into(&self, w: &mut SnapWriter) {
        let TypedProgram {
            modules,
            modules_completed,
            config: _,
            program_settings,
            emitted_parse_cache: _,
            ast,
            functions,
            function_specializations: _,
            variables,
            types,
            type_hashes,
            type_slices,
            type_slice_dedup: _,
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
            type_infos,
            scopes,
            messages,
            namespaces,
            abilities,
            ability_impls,
            ability_impl_table,
            ability_impl_table_by_ability,
            blanket_impls,
            function_name_to_ability_names,
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
            ls_entities,
            completion: _,
            lsp: _,
            inference_context_stack,
            inference_context_extras: _,
            type_defn_context,
            buffers: _,
            patterns,
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
            inputs_hash: _,
            restored_module_count: _,
        } = self;
        assert!(megarepl.is_none(), "cannot snapshot a megarepl session");
        assert!(inference_context_stack.is_empty(), "cannot snapshot mid-inference");

        ast.snap(w);
        w.write_section("typed");
        mem.snap(w);
        modules.snap(w);
        w.write_slice(modules_completed);
        w.write_t(program_settings);
        functions.snap(w);
        variables.snap(w);
        types.snap(w);
        write_map_snap(w, type_hashes);
        type_slices.snap(w);
        type_variable_counts.snap(w);
        type_instance_info.snap(w);
        write_map_snap(w, type_defn_info);
        write_map_snap(w, type_specializations);
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
        write_map_snap(w, type_infos);
        scopes.snap(w);
        w.write_slice(&messages.borrow());
        w.sorted_entries(ls_entities.borrow().iter(), |w, (file_id, entities)| {
            w.write_t(file_id);
            w.write_slice(entities);
        });
        namespaces.namespaces.snap(w);
        abilities.snap(w);
        ability_impls.snap(w);
        write_map_snap(w, ability_impl_table);
        write_map_snap(w, ability_impl_table_by_ability);
        write_map_snap(w, blanket_impls);
        write_map_snap(w, function_name_to_ability_names);
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
        let TypeDefnContext {
            stack,
            recursive_mentions,
            pending_instances,
            completed,
            expanding_aliases,
        } = type_defn_context;
        assert!(stack.is_empty());
        assert!(recursive_mentions.is_empty());
        assert!(pending_instances.is_empty());
        assert!(completed.is_empty());
        assert!(expanding_aliases.is_empty());

        patterns.mem.snap(w);
        w.write_slice(emitted_sources);
        ir.snap(w);
        w.write_t(global_id_k1_arena);
        w.write_section("end");
    }

    pub fn restore(
        bytes: &[u8],
        // `config` and `lsp` come from the restoring session
        // lets us preserve settings like chatty, cache, overrides, completion
        config: CompilerConfig,
        lsp: crate::compiler::LspCompileOptions,
    ) -> Result<TypedProgram, String> {
        let mut reader = SnapReader::new(bytes)?;
        let r = &mut reader;
        let ast = ParsedProgram::restore(r);
        let mut k1 = TypedProgram::new(ast, config, lsp);
        r.section("typed");
        k1.mem.restore(r);
        k1.modules.restore(r);
        k1.modules_completed = r.read_vec();
        k1.program_settings = r.read_t();
        k1.functions.restore(r);
        for (id, function) in k1.functions.iter_with_ids() {
            if let Some(info) = function.specialization_info {
                k1.function_specializations
                    .entry((info.parent_function, info.type_arguments, info.fnlike_type_arguments))
                    .or_insert(id);
            }
        }
        k1.variables.restore(r);
        k1.types.restore(r);
        k1.type_hashes = restore_map_snap(r);
        k1.type_slices.restore(r);
        k1.rebuild_type_slice_dedup();
        k1.type_variable_counts.restore(r);
        k1.type_instance_info.restore(r);
        k1.type_defn_info = restore_map_snap(r);
        k1.type_specializations = restore_map_snap(r);
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
        k1.type_infos = restore_map_snap(r);
        k1.scopes = Scopes::restore(r);
        k1.messages = RefCell::new(r.read_vec());
        let mut ls_entities = FxHashMap::default();
        for _ in 0..r.read_len() {
            let file_id: FileId = r.read_t();
            ls_entities.insert(file_id, r.read_vec());
        }
        k1.ls_entities = RefCell::new(ls_entities);
        k1.namespaces.namespaces.restore(r);
        k1.abilities.restore(r);
        k1.ability_impls.restore(r);
        k1.ability_impl_table = restore_map_snap(r);
        k1.ability_impl_table_by_ability = restore_map_snap(r);
        k1.blanket_impls = restore_map_snap(r);
        k1.function_name_to_ability_names = restore_map_snap(r);
        k1.namespace_type_params = restore_map_snap(r);
        k1.namespace_ast_mappings = restore_map_snap(r);
        k1.function_ast_mappings = restore_map_snap(r);
        k1.macro_ast_mappings = restore_map_snap(r);
        k1.global_ast_mappings = restore_map_snap(r);
        k1.ability_impl_ast_mappings = restore_map_snap(r);
        k1.patterns.mem.restore(r);
        k1.emitted_sources = r.read_vec();
        k1.emitted_parse_cache.clear();
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
        let mut restored = match TypedProgram::restore(&first, self.config, self.lsp.clone()) {
            Ok(restored) => restored,
            Err(e) => panic!("snapshot restore failed: {e}"),
        };
        eprintln!(
            "roundtripping {}mb snapshot took ({:?})",
            first.len() as f64 / (1024.0 * 1024.0),
            snap_start.elapsed()
        );
        let second = restored.snap();
        crate::snap::assert_identical(&first, &second, "TypedProgram snapshot roundtrip");
        restored.inputs_hash = self.inputs_hash;
        restored.restored_module_count = self.restored_module_count;
        std::mem::swap(&mut restored.timing, &mut self.timing);
        std::mem::swap(self, &mut restored);
    }
}
