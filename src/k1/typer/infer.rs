// Copyright (c) 2026 knix
// All rights reserved.

/// Inference in the `typer` phase.
///
/// Obviously the lines are really blurry, and I honestly might just prefer to leave everything in
/// typer.rs, but tooling starts to have bad latency around 15k lines, and there's a clear enough
/// theme here that it feels ok to break it out. But this is by no means an encapsulation, just
/// a rough-cut grouping
///
/// <todo, one day a write up about our inference
use super::*;
use crate::spair;
use crate::{errf, failf};

/// Wiring for reusing argument evaluations done during inference (see infer_types):
/// when a value argument's expected type is already fully concrete, it can no longer be
/// changed by later learning, so the argument is evaluated for real (with the caller's
/// context, not throwaway inference mode) and recorded here for eval_function_call to
/// reuse instead of re-evaluating
pub(crate) struct InferArgStash<'a> {
    /// The caller's evaluation context; never inference-mode
    pub ctx: EvalExprContext,
    /// Pair index of the first value-argument pair; earlier pairs (explicitly passed
    /// type args, the return-type pair) are never stashable
    pub first_value_pair_index: u32,
    /// Value-argument indices that must not be stashed; fnlike type params get their
    /// values from determine_fnlike_type_args_for_call instead of the argument pass
    pub excluded_args: SV4<u32>,
    /// Output: (argument index, typed expr) for each argument evaluated for real
    pub stashed: &'a mut SV8<(u32, TypedExprId)>,
}

impl TypedProgram {
    /// Performs type inference given an input of, essentially, (expected type, actual type) and
    /// a series of parameters to solve for, and performs unification/matching on those pairs.
    ///
    /// allow_mismatch allows inference to continue when a term does not match with its position's
    /// type.
    /// It is currently used to avoid reporting a mismatch on the return type
    /// before we're able to learn more about the rest of the inference.
    /// We get a better error message if we wait to report the mismatch until the end
    pub(crate) fn infer_types(
        &mut self,
        all_type_params: &[NameAndType],
        // Sometimes, we don't need to solve everything. An example is when resolving
        // an ability call, we don't need to solve for the impl-side params, as they will come
        // once we look up the impl.
        //
        // But in this case we still instantiate all the params as type holes, so that we can
        // detect inconsistencies, and so that we don't have to bind type variables to themselves
        // as a separate step
        must_solve_params: NamedTypeSlice,
        inference_pairs: &[InferenceInputPair],
        span: SpanId,
        scope_id: ScopeId,
        stash: Option<InferArgStash>,
    ) -> K1Result<(NamedTypeSlice, NamedTypeSlice)> {
        if self.inference_context_stack.is_empty() {
            self.ictx_push();
        }

        self.ictx_mut().origin_stack.push(span);
        if self.ictx().origin_stack.len() == 1 {
            let raw = self.timing.clock.raw();
            self.ictx_mut().start_raw = raw;
        }

        let result = self.infer_types_inner(
            all_type_params,
            must_solve_params,
            inference_pairs,
            span,
            scope_id,
            stash,
        );

        let popped = self.ictx_mut().origin_stack.pop().unwrap();
        debug_assert_eq!(popped, span);
        if self.ictx().origin_stack.is_empty() {
            let elapsed = self.timing.clock.elapsed_nanos(self.ictx().start_raw);
            self.timing.total_infers += 1;
            self.timing.total_infer_nanos += elapsed as i64;
            debug!("Resetting inference context");
            self.ictx_pop();
        }
        result
    }

    fn infer_types_inner(
        &mut self,
        all_type_params: &[NameAndType],
        must_solve_params: NamedTypeSlice,
        inference_pairs: &[InferenceInputPair],
        span: SpanId,
        scope_id: ScopeId,
        mut stash: Option<InferArgStash>,
    ) -> K1Result<(NamedTypeSlice, NamedTypeSlice)> {
        let infer_depth = self.ictx().origin_stack.len();
        let frame_start = self.ictx().slots.len() as u32;
        for (idx, param) in all_type_params.iter().enumerate() {
            let hole_index = idx as u32 + frame_start;

            let static_type = self.types.get(param.type_id).as_tvar().unwrap().static_constraint;

            let type_hole = self.types.get_inference_hole(hole_index, static_type);

            self.ictx_mut().slots.push(InferenceSlot {
                param_type: param.type_id(),
                hole_type: type_hole,
                solution: None,
                fully_solved: false,
            });
        }

        // Used for the error message, mainly
        let mut argument_types: SmallVec<[TypeId; 8]> = smallvec![];
        // The heart of the inference process:
        // - For each pair of (<actual type> <expected type>), we 'pattern match' on the types
        //   and learn what we can about all of the type holes
        // - The parameter type is 'instantiated' first: each type parameter like 'T' maps
        //   directly to its current solution if we have one, or to its inference hole like '0
        //   otherwise. Getting the 'expected type so far' in one pass this way gives us
        //   better results for later arguments once we've learned from prior ones, and
        //   solved params never materialize types with holes in them.
        for (index, InferenceInputPair { arg: expr, param_type: gen_param, allow_mismatch }) in
            inference_pairs.iter().enumerate()
        {
            debug!(
                "[infer {infer_depth}] Inferring call argument {} / {}",
                index + 1,
                inference_pairs.len()
            );

            let frame_subst: SV8<TypeSubstitutionPair> = self.ictx().slots
                [frame_start as usize..]
                .iter()
                .map(|s| spair! { s.param_type => s.solution.unwrap_or(s.hole_type) })
                .collect();
            let expected_type_so_far = self.substitute_in_type(*gen_param, &frame_subst);
            debug!(
                "[infer {infer_depth}] Expected type is: {}",
                self.type_id_to_string(expected_type_so_far)
            );

            let (argument_type, argument_span) = match expr {
                TypeOrParsedExpr::Type(type_id) => (*type_id, span),
                TypeOrParsedExpr::Parsed(parsed_expr) => {
                    let expected_counts =
                        self.types.get_type_variable_counts(expected_type_so_far);
                    let expected_is_concrete = expected_counts.inference_variable_count == 0
                        && expected_counts.unresolved_static_count == 0;

                    // If the expected type is fully concrete, this argument can't teach us
                    // anything (unify would return NoHoles), and its expected type can no
                    // longer change: evaluate it for real, once, with the caller's context,
                    // and stash the result for the post-inference argument pass to reuse
                    let stash_arg_index = match &stash {
                        Some(st)
                            if expected_is_concrete
                                && index as u32 >= st.first_value_pair_index =>
                        {
                            let arg_index = index as u32 - st.first_value_pair_index;
                            if st.excluded_args.contains(&arg_index) {
                                None
                            } else {
                                Some(arg_index)
                            }
                        }
                        _ => None,
                    };

                    let evaluation_result = match stash_arg_index {
                        Some(arg_index) => {
                            let stash = stash.as_mut().unwrap();
                            let real_ctx = stash
                                .ctx
                                .with_expected_type(Some(expected_type_so_far))
                                .with_is_method_receiver(false);
                            match self.eval_expr(*parsed_expr, real_ctx) {
                                Err(e) => Err(e),
                                Ok(expr_id) => {
                                    match self.check_and_coerce_expr(
                                        expected_type_so_far,
                                        expr_id,
                                        real_ctx.scope_id,
                                        false,
                                    ) {
                                        Ok(coerced) => {
                                            stash.stashed.push((arg_index, coerced));
                                            Ok(coerced)
                                        }
                                        // On coercion failure, don't stash: the
                                        // post-inference pass re-evaluates and produces
                                        // its usual error
                                        Err(_) => Ok(expr_id),
                                    }
                                }
                            }
                        }
                        None => {
                            let inference_context = EvalExprContext::make(scope_id)
                                .with_inference(true)
                                .with_expected_type(Some(expected_type_so_far));
                            self.eval_expr_with_coercion(*parsed_expr, inference_context, false)
                        }
                    };
                    match evaluation_result {
                        Ok(expr_id) => {
                            let expr_type = self.exprs.get_type(expr_id);
                            let expr_span = self.exprs.get_span(expr_id);
                            debug!(
                                "[infer {infer_depth}] Actual type is: {}",
                                self.type_id_to_string(expr_type)
                            );
                            (expr_type, expr_span)
                        }
                        Err(e) => {
                            // Some expression types, like lambdas, fail really easily if we don't
                            // know the types of the inputs yet. When a lambda fails here, we
                            // choose to continue and report a different, likely more informative,
                            // error
                            //
                            // This is swallowing really important info when the lambda is just
                            // mis-written, so need to rethink this, toggling to false for now
                            // Eventually I think we need a signal score heuristic on errors to
                            // determine whether the lambda failure would stop inference here or not
                            let should_skip = match self.ast.exprs.get(*parsed_expr) {
                                ParsedExpr::Lambda(_) => false,
                                _ => false,
                            };
                            if should_skip {
                                continue;
                            } else {
                                return failf!(
                                    e.span,
                                    "{}\nOccurred while trying to determine type of argument for inference using expected type: {}",
                                    e.message,
                                    self.type_id_to_string(expected_type_so_far)
                                );
                            }
                        }
                    }
                }
            };
            argument_types.push(argument_type);
            debug!(
                "[infer {infer_depth}] unify {} =:= {}",
                self.type_id_to_string(argument_type),
                self.type_id_to_string(expected_type_so_far),
            );
            // unify_and_find_substitutions populates self.inferences_context.constraints
            if let TypeUnificationResult::NonMatching(msg) =
                self.unify_and_find_substitutions(argument_type, expected_type_so_far)
            {
                // allow_mismatch is used to avoid reporting a mismatch on the return type,
                // before we're able to learn more about the rest of the inference. We get a better
                // error message if we wait to report the mismatch until the end
                if !allow_mismatch {
                    return failf!(
                        argument_span,
                        "(unify fail) Passed value does not match expected type: expected {} but got {}\nReason: {msg}",
                        self.type_id_to_string(expected_type_so_far),
                        self.type_id_to_string(argument_type)
                    );
                }
            };

            // After each pair is 'walked', we apply the constraints of any param that just
            // became fully solved. Solving one param can transitively solve others (including
            // inside apply_constraints_to_inferred_type itself); the queue picks those up too
            while let Some(slot_index) = self.ictx_mut().newly_solved.pop() {
                let slot = self.ictx().slots[slot_index as usize];
                let solution = slot.solution.unwrap();
                debug!(
                    "[infer {infer_depth}] ****** GOT NEWLY SOLVED PARAM {} -> {}",
                    self.type_id_to_string(slot.param_type),
                    self.type_id_to_string(solution)
                );
                self.apply_constraints_to_inferred_type(
                    slot.param_type,
                    solution,
                    scope_id,
                    argument_span,
                )?;
            }
        }

        let mut solutions: List<NameAndType, _> = self.mem.new_list(must_solve_params.len());
        let mut unsolved_params: SV8<NameAndType> = smallvec![];
        let mut all_solutions: List<NameAndType, _> =
            self.mem.new_list(all_type_params.len() as u32);

        for (idx, param) in all_type_params.iter().enumerate() {
            let slot = self.ictx().slots[frame_start as usize + idx];
            debug_assert_eq!(slot.param_type, param.type_id());
            let is_must_solve = self.mem.slice_contains(must_solve_params, param);
            if let Some(solution) = slot.solution {
                all_solutions.push(NameAndType { name: param.name(), type_id: solution });
                if is_must_solve {
                    solutions.push(NameAndType { name: param.name(), type_id: solution });
                };
            } else if is_must_solve {
                unsolved_params.push(*param);
            }
        }
        if !unsolved_params.is_empty() {
            let solved_pairs: SV8<TypeSubstitutionPair> = self
                .ictx()
                .slots
                .iter()
                .filter_map(|s| s.solution.map(|sol| spair! { s.param_type => sol }))
                .collect();
            return failf!(
                span,
                "Could not solve for {} given arguments:\n{}\nSolutions:{}",
                unsolved_params
                    .iter()
                    .map(|p| self.ident_str(p.name()))
                    .collect::<Vec<_>>()
                    .join(", "),
                argument_types
                    .iter()
                    .zip(inference_pairs.iter())
                    .map(|(passed_type, pair)| {
                        format!(
                            "{}: {}",
                            self.type_id_to_string_ext(*passed_type, false),
                            self.type_id_to_string_ext(pair.param_type, false),
                        )
                    })
                    .collect::<Vec<_>>()
                    .join("\n"),
                self.pretty_print_type_substitutions(&solved_pairs, ", ")
            );
        }
        debug!("INFER DONE {}", self.pretty_print_named_types(&solutions, ", "));
        let solutions_handle = self.mem.list_to_handle(solutions);
        let all_solutions_handle = self.mem.list_to_handle(all_solutions);
        Ok((solutions_handle, all_solutions_handle))
    }

    /// Called from infer_types
    ///
    /// Each time we get a _concrete_ substitution for a param,
    /// if the param has constraints,
    /// we need to 'learn' from the constraints, adding an inference pair to the mix
    /// for each ability param and ability impl param in that constraint.
    /// Consider:
    /// fn find[T, I: Iterator[Item = T]](i: I, fn: \T -> bool)
    /// find(myList, \i i.isEven())
    ///      ^
    /// We need to learn T FROM THE ABILITY IMPL FOR ITERATOR FOR MYLIST, so that we can infer the
    /// type of the function param in \i i.isEven(). We can't learn it from just the callsite.
    pub(crate) fn apply_constraints_to_inferred_type(
        &mut self,
        type_param_id: TypeId,
        solution_type_id: TypeId,
        scope_id: ScopeId,
        span: SpanId,
    ) -> K1Result<()> {
        let infer_depth = self.ictx().origin_stack.len();
        if let Some(static_type) = self.types.get_type_parameter(type_param_id).static_constraint {
            debug!(
                "[infer {infer_depth}] > learning from STATIC param {} := {}. static type: {}",
                self.type_id_to_string(type_param_id),
                self.type_id_to_string(solution_type_id),
                self.type_id_to_string(static_type),
            );
            let Some(solution_static_type) = self.types.get_value_type_id_of_type(solution_type_id)
            else {
                error!(
                    "The solution was not a static type; this is probably crashworthy because we shouldn't have accepted it"
                );
                return Ok(());
            };
            let subst_set = self.make_inference_substitution_set();
            let static_type_subst = self.substitute_in_type(static_type, &subst_set);
            // TODO: This function should probably just report the findings, not apply new
            // substitutions, because there may be other things we need to do when we learn a type
            // like when we learn a static type param I think we may need to do more
            match self.unify_and_find_substitutions(solution_static_type, static_type_subst) {
                TypeUnificationResult::Matching => {
                    debug!("[infer {infer_depth}] unify succeeded",);
                }
                TypeUnificationResult::NoHoles => {}
                TypeUnificationResult::NonMatching(_) => {
                    eprintln!("did not match; likely we'll fail later")
                }
            }
            return Ok(());
        }
        let constraint_impls = self.get_constrained_ability_impls_for_type(type_param_id);

        if constraint_impls.is_empty() {
            return Ok(());
        }

        debug!(
            "[infer {infer_depth}] > learning from param {} := {}. constraints: {}",
            self.type_id_to_string(type_param_id),
            self.type_id_to_string(solution_type_id),
            constraint_impls.len()
        );
        let subst_set = self.make_inference_substitution_set();

        for constraint in &constraint_impls {
            let constraint_signature = self.ability_impls.get(constraint.full_impl_id).signature();

            let sig = self.substitute_in_ability_signature(
                &subst_set,
                constraint_signature,
                scope_id,
                span,
            );
            debug!(
                "[infer {infer_depth}] trying to learn from sig: {}",
                self.ability_signature_to_string(sig)
            );
            match self.find_or_generate_specialized_ability_impl_for_type(
                solution_type_id,
                sig.specialized_ability_id,
                false,
                scope_id,
                span,
            ) {
                Ok((impl_id, _)) => {
                    let the_impl = self.ability_impls.get(impl_id.full_impl_id);
                    let ability_arg_iterator = self
                        .mem
                        .getn(self.abilities.get(sig.specialized_ability_id).kind.arguments())
                        .iter()
                        .zip(
                            self.mem.getn(self.abilities.get(the_impl.ability_id).kind.arguments()),
                        );
                    let impl_arg_iterator = self
                        .mem
                        .getn(sig.impl_arguments)
                        .iter()
                        .zip(self.mem.getn(the_impl.impl_arguments));
                    for (constrained_type, found_impl_type) in
                        ability_arg_iterator.chain(impl_arg_iterator)
                    {
                        debug!(
                            "[infer {infer_depth}] I will unify impl args {} and {}",
                            self.named_type_to_string(constrained_type),
                            self.named_type_to_string(found_impl_type)
                        );
                        match self.unify_and_find_substitutions(
                            found_impl_type.type_id,
                            constrained_type.type_id,
                        ) {
                            TypeUnificationResult::Matching => {
                                debug!("[infer {infer_depth}] unify succeeded",);
                            }
                            TypeUnificationResult::NoHoles => {}
                            TypeUnificationResult::NonMatching(_) => {
                                eprintln!("did not match; likely we'll fail later")
                            }
                        }
                    }
                }
                Err(msg) => {
                    let signature = self.ability_impls.get(constraint.full_impl_id).signature();
                    return failf!(
                        span,
                        "Could not satisfy ability constraint {} for given type {} := {} due to: {msg}",
                        self.ability_signature_to_string(signature),
                        self.type_id_to_string(type_param_id),
                        self.type_id_to_string(solution_type_id)
                    );
                }
            };
        }
        Ok(())
    }

    /// Returns the solved type params, plus (argument index, typed expr) for each value
    /// argument that inference already evaluated for real and the caller should reuse
    /// instead of re-evaluating
    pub(crate) fn infer_and_constrain_call_type_args(
        &mut self,
        fn_call: &ParsedCall,
        generic_function_sig: FunctionSignature,
        ctx: EvalExprContext,
        args_and_params: &ArgsAndParams,
    ) -> K1Result<(NamedTypeSlice, SV8<(u32, TypedExprId)>)> {
        debug!("infer_and_constrain_call_type_args");
        debug_assert!(generic_function_sig.has_type_params());
        let passed_type_args = fn_call.type_args;
        let passed_type_args_count = passed_type_args.len();
        let type_params = generic_function_sig.type_params;
        // Fuse these two paths; where for a given type param,
        // - X *if ALL PASSED, special case to skip inference altogether of course
        if !passed_type_args.is_empty() && passed_type_args.len() != type_params.len() {
            return failf!(
                fn_call.span,
                "Expected {} type arguments but got {}",
                type_params.len(),
                passed_type_args_count
            );
        }
        let all_params_were_passed = passed_type_args.len() == type_params.len()
            && self.ast.mem.getn(passed_type_args).iter().all(|nt| nt.type_expr.is_some());
        debug!("all_passed={all_params_were_passed}");
        let mut stashed_args: SV8<(u32, TypedExprId)> = smallvec![];
        let solved_type_params = if all_params_were_passed {
            let mut evaled_params: List<NameAndType, _> = self.mem.new_list(type_params.len());
            for (type_param, type_arg) in
                self.mem.getn(type_params).iter().zip(self.ast.mem.getn(passed_type_args).iter())
            {
                let passed_expr = type_arg.type_expr.unwrap(); // checked by all_passed
                let passed_type = self.eval_type_expr(passed_expr, ctx.scope_id)?;
                evaled_params.push(NameAndType { name: type_param.name, type_id: passed_type });
            }
            self.mem.list_to_handle(evaled_params)
        } else {
            let generic_function_type =
                *self.types.get(generic_function_sig.function_type).as_function().unwrap();
            let generic_function_return_type = generic_function_type.return_type;

            let mut inference_pairs: SV8<_> = smallvec![];

            // We add an inference pair for every type the user explicitly passed in
            //
            // We add these first so that they get applied first, and following conflicts are
            // framed in terms of these being true; if users says T := Pointer, we later say
            // "expected Pointer" when other params dont line up.
            for (index, type_arg) in self.ast.mem.getn(passed_type_args).iter().enumerate() {
                if let Some(passed_type_expr) = type_arg.type_expr {
                    let matching_param = if let Some(passed_name) = type_arg.name {
                        let matching_param =
                            self.mem.find(type_params, |nt| nt.name == passed_name);
                        matching_param
                    } else {
                        // Use position
                        self.mem.get_nth_opt(type_params, index)
                    };
                    let passed_type = self.eval_type_expr(passed_type_expr, ctx.scope_id)?;
                    if let Some(matching_param) = matching_param {
                        debug!(
                            "Adding a pair {} {}",
                            self.ident_str(matching_param.name),
                            self.type_id_to_string(passed_type)
                        );
                        inference_pairs.push(InferenceInputPair {
                            param_type: matching_param.type_id,
                            arg: TypeOrParsedExpr::Type(passed_type),
                            allow_mismatch: false,
                        })
                    } else {
                        return failf!(fn_call.span, "Unable to line up your type arguments");
                    }
                }
            }

            // We add an inference pair for the return type and expected type as the next most
            // important
            if let Some(expected) = ctx.expected_type_id {
                // One Inference Pair for the return type
                inference_pairs.push(InferenceInputPair {
                    arg: TypeOrParsedExpr::Type(expected),
                    param_type: generic_function_return_type,
                    allow_mismatch: true,
                });
            }

            // An Inference Pair for each parameter/argument pair
            let first_value_pair_index = inference_pairs.len() as u32;
            inference_pairs.extend(args_and_params.iter(&self.tmp).map(|(expr, param)| {
                let passed_type = match expr {
                    MaybeTypedExpr::Parsed(expr_id) => TypeOrParsedExpr::Parsed(*expr_id),
                    MaybeTypedExpr::Typed(expr) => {
                        TypeOrParsedExpr::Type(self.exprs.get_type(*expr))
                    }
                };
                InferenceInputPair {
                    arg: passed_type,
                    param_type: param.type_id,
                    allow_mismatch: false,
                }
            }));

            // Nested inference keeps today's throwaway-eval path: real evaluation would
            // specialize callees the inference pass deliberately leaves abstract, and the
            // enclosing inference discards this whole call node anyway
            let stash = if ctx.is_inference() {
                None
            } else {
                let excluded_args: SV4<u32> = self
                    .mem
                    .getn(generic_function_sig.fnlike_type_params)
                    .iter()
                    .map(|ftp| ftp.value_param_index)
                    .collect();
                Some(InferArgStash {
                    ctx,
                    first_value_pair_index,
                    excluded_args,
                    stashed: &mut stashed_args,
                })
            };

            let (solutions, _all_solutions) = self
                .infer_types(
                    self.mem.getn(type_params),
                    type_params,
                    &inference_pairs,
                    fn_call.span,
                    ctx.scope_id,
                    stash,
                )
                .map_err(|e| {
                    errf!(
                        e.span,
                        "Invalid call to {}\n    {}",
                        self.ident_str_opt(generic_function_sig.name),
                        e.message,
                    )
                })?;
            solutions
        };

        // Enforce ability constraints
        let params_to_solutions_pairs: SV4<TypeSubstitutionPair> =
            self.zip_named_types_to_subst_pairs(type_params, solved_type_params);
        for (solution, type_param) in
            self.mem.getn(solved_type_params).iter().zip(self.mem.getn(type_params).iter())
        {
            self.check_type_constraints(
                type_param.name,
                type_param.type_id,
                solution.type_id,
                &params_to_solutions_pairs,
                ctx.scope_id,
                fn_call.span,
            )
            .map_err(|e| {
                errf!(
                    e.span,
                    "{}. Therefore, cannot call function '{}' with given types: {}",
                    e.message,
                    self.ident_str(fn_call.name.name),
                    self.pretty_print_named_type_slice(solved_type_params, ", ")
                )
            })?;
        }
        Ok((solved_type_params, stashed_args))
    }

    pub(crate) fn determine_fnlike_type_args_for_call(
        &mut self,
        original_function_sig: FunctionSignature,
        type_args: NamedTypeSlice,
        args_and_params: &ArgsAndParams,
        ctx: EvalExprContext,
    ) -> K1Result<(NamedTypeSlice, SV8<TypedExprId>)> {
        // Ok here's what we need for function params. We need to know just the _kind_ of function that
        // was passed: ref, lambda, or lambda obj, and we need to specialize the function shape on
        // the other type params, as in: some (T -> T) -> some (int -> int), THEN just create
        // a type using the _kind_ we need:
        // fn ref: some (int -> int) -> (int -> int)*
        // lambda: some (int -> int) -> unique(\int -> int) (is this the zero-sized param? Oh, its physically just passing the environment!)
        // lambda obj: some (int -> int) -> dyn[\int -> int] (passing environment AND fn ptr)
        //
        // This method is risk-free in terms of 'leaking' inference types out
        if original_function_sig.fnlike_type_params.is_empty() {
            return Ok((MSlice::empty(), smallvec![]));
        }

        let mut fnlike_type_args: List<NameAndType, _> =
            self.mem.new_list(original_function_sig.fnlike_type_params.len());
        let mut fnlike_type_arg_values: SV8<TypedExprId> = smallvec![];
        let subst_pairs: SV8<_> = self
            .mem
            .getn_zip(original_function_sig.type_params, type_args)
            .map(|(param, arg)| TypeSubstitutionPair { from: param.type_id, to: arg.type_id })
            .collect();

        for function_type_param in self.mem.getn(original_function_sig.fnlike_type_params) {
            let (corresponding_arg, corresponding_value_param) =
                args_and_params.get(function_type_param.value_param_index as usize, &self.tmp);
            debug!(
                "The param for function_type_param {} {} is {} and passed: {:?}",
                function_type_param.type_id,
                self.ident_str(function_type_param.name),
                self.ident_str(corresponding_value_param.name),
                corresponding_arg
            );

            enum PhysicalPassedFunction {
                Lambda(TypedExprId, TypeId),
                FunctionPointer(TypedExprId),
                LambdaObject(TypedExprId, TypeId),
            }
            let physical_passed_function = match corresponding_arg {
                MaybeTypedExpr::Typed(_) => {
                    unreachable!("Synthesizing calls with function type params is unsupported")
                }
                MaybeTypedExpr::Parsed(p) => match self.ast.exprs.get(p) {
                    ParsedExpr::Lambda(_lam) => {
                        debug!(
                            "substituting type for an ftp lambda so that it can infer (is_inf={})",
                            ctx.is_inference()
                        );
                        let substituted_param_type =
                            self.substitute_in_type(function_type_param.type_id, &subst_pairs);
                        let the_lambda = self
                            .eval_expr(p, ctx.with_expected_type(Some(substituted_param_type)))?;
                        let lambda_type = self.exprs.get_type(the_lambda);
                        debug!("Using a Lambda as an ftp: {}", self.type_id_to_string(lambda_type));
                        PhysicalPassedFunction::Lambda(the_lambda, lambda_type)
                    }
                    _other => {
                        let expr = self.eval_expr(p, ctx.with_no_expected_type())?;
                        let type_id = self.exprs.get_type(expr);
                        match self.types.get(type_id) {
                            Type::Lambda(_) => PhysicalPassedFunction::Lambda(expr, type_id),
                            Type::LambdaObject(_) => {
                                debug!("Using a LambdaObject as an ftp");
                                PhysicalPassedFunction::LambdaObject(expr, type_id)
                            }
                            Type::FunctionPointer(_) => {
                                debug!("Using a FunctionPointer as an ftp");
                                PhysicalPassedFunction::FunctionPointer(expr)
                            }
                            _ => {
                                let span = self.exprs.get_span(expr);
                                return failf!(
                                    span,
                                    "Expected {}, which is an existential function type (lambdas, dynamic lambdas, and function pointers all work), but got: {}",
                                    self.type_id_to_string(function_type_param.type_id),
                                    self.type_id_to_string(type_id)
                                );
                            }
                        }
                    }
                },
            };
            let (final_type, final_value) = match physical_passed_function {
                PhysicalPassedFunction::Lambda(value, lambda_type) => {
                    // Can use as-is since we rebuilt this lambda already
                    (lambda_type, value)
                }
                PhysicalPassedFunction::FunctionPointer(expr) => {
                    let ftp = self
                        .types
                        .get(function_type_param.type_id)
                        .as_function_type_parameter()
                        .unwrap();
                    let original_param_function_type = ftp.function_type;
                    let substituted_function_type =
                        self.substitute_in_type(original_param_function_type, &subst_pairs);
                    let fp_type = self.types.add_function_pointer_type(substituted_function_type);
                    (fp_type, expr)
                }
                PhysicalPassedFunction::LambdaObject(expr, lambda_object_type) => {
                    // Replace the function type
                    let substituted_lambda_object_type =
                        self.substitute_in_type(lambda_object_type, &subst_pairs);
                    (substituted_lambda_object_type, expr)
                }
            };
            fnlike_type_arg_values.push(final_value);
            fnlike_type_args
                .push(NameAndType { name: function_type_param.name, type_id: final_type });
        }
        if !fnlike_type_args.is_empty() {
            debug!(
                "We're passing fnlike_type_args! {}",
                self.pretty_print_named_types(&fnlike_type_args, ", ")
            );
        }
        let fnlike_type_args_handle = self.mem.list_to_handle(fnlike_type_args);
        Ok((fnlike_type_args_handle, fnlike_type_arg_values))
    }

    /// Record that inference hole `hole_type` must be `to`.
    ///
    /// Setting an already-solved slot overwrites it, as solutions can improve
    fn set_hole_solution(&mut self, hole_type: TypeId, to: TypeId) {
        debug!(
            "set_hole_solution {} := {}",
            self.type_id_to_string(hole_type),
            self.type_id_to_string(to),
        );
        if hole_type == to {
            return;
        }
        let Type::InferenceHole(hole) = self.types.get(hole_type) else {
            unreachable!("set_hole_solution called on a non-hole")
        };
        let index = hole.index as usize;
        let Some(slot) = self.ictx().slots.get(index).copied() else {
            // A stale hole leaked from a sibling (already-popped) inference context whose
            // numbering exceeded ours; there is nothing meaningful to bind it to
            debug_assert!(false, "set_hole_solution index out of range");
            return;
        };

        match slot.solution {
            Some(existing) if existing == to => {}
            Some(existing) => {
                self.set_slot_solution(index, to);

                // If the previous solution is another unsolved hole,
                // set that hole's solution as well. Example:
                // '1 -> '2
                // set_hole_solution('1, int)
                //  -> set_hole_solution('2, int)
                if matches!(self.types.get(existing), Type::InferenceHole(_)) {
                    self.set_hole_solution(existing, to);
                }
            }
            None => self.set_slot_solution(index, to),
        }
    }

    /// Assign `to` as slot `index`'s solution and keep every slot maximally resolved:
    /// the incoming solution is normalized against what's already solved, then the
    /// newly learned fact is propagated into other slots' partial solutions.
    /// Slots that become hole-free are queued on `newly_solved` for constraint application
    fn set_slot_solution(&mut self, index: usize, to: TypeId) {
        // Normalize the incoming solution against everything solved so far, so that
        // solutions stay maximally resolved regardless of learning order
        let to = if self.types.get_type_variable_counts(to).inference_variable_count > 0 {
            let known: SV8<TypeSubstitutionPair> = self
                .ictx()
                .slots
                .iter()
                .filter_map(|s| s.solution.map(|sol| spair! { s.hole_type => sol }))
                .collect();
            self.substitute_in_type(to, &known)
        } else {
            to
        };

        let hole_type = {
            let slot = &mut self.ictx_mut().slots[index];
            slot.solution = Some(to);
            slot.hole_type
        };
        if self.types.get_type_variable_counts(to).inference_variable_count == 0 {
            let slot = &mut self.ictx_mut().slots[index];
            if !slot.fully_solved {
                slot.fully_solved = true;
                self.ictx_mut().newly_solved.push(index as u32);
            }
        }

        // Propagate the new fact into other slots' still-partial solutions
        let pair = [spair! { hole_type => to }];
        for i in 0..self.ictx().slots.len() {
            if i == index {
                continue;
            }
            let s = self.ictx().slots[i];
            let Some(sol) = s.solution else { continue };
            if s.fully_solved {
                continue;
            }
            let new_sol = self.substitute_in_type(sol, &pair);
            if new_sol == sol {
                continue;
            }
            let now_solved =
                self.types.get_type_variable_counts(new_sol).inference_variable_count == 0;
            let slot = &mut self.ictx_mut().slots[i];
            slot.solution = Some(new_sol);
            if now_solved {
                slot.fully_solved = true;
                self.ictx_mut().newly_solved.push(i as u32);
            }
        }
    }

    /// Used for fixing up the constraint signatures:
    /// For each param they mention, if its solved, use the solution, otherwise use the
    /// inference hole so we can learn more about it
    fn make_inference_substitution_set(&self) -> SV8<TypeSubstitutionPair> {
        self.ictx()
            .slots
            .iter()
            .map(|slot| {
                let solved = if slot.fully_solved { slot.solution } else { None };
                spair! { slot.param_type => solved.unwrap_or(slot.hole_type) }
            })
            .collect()
    }

    fn unify_and_find_substitutions(
        &mut self,
        passed_type: TypeId,
        slot_type: TypeId,
    ) -> TypeUnificationResult {
        // eprintln!("unify_and_find_substitutions slot {}", self.type_id_to_string(slot_type));
        let result = self.unify_and_find_substitutions_rec(passed_type, slot_type);
        result
    }

    pub(crate) fn unify_and_find_substitutions_rec(
        &mut self,
        passed_type: TypeId,
        slot_type: TypeId,
    ) -> TypeUnificationResult {
        // passed_type           slot_type          -> result
        //
        // int                    '0                 -> '0 := int
        // List[int]              List['0]           -> '0 := int
        // Pair[int, string]      Pair['0, '1]        -> '0 := int, '1 := string
        // fn(int) -> int         Fn('0) -> '0        -> '0 := int
        // fn() -> List[string]   Fn() -> List['0]   -> '0 := string
        //
        // Recursive
        //
        // one day: Higher-order types (I dont see why not?)
        // List[int]              F[int]            -> F := List
        debug!(
            "unify_and_find_substitutions.\n  passed {}\n  slot   {}",
            self.type_id_to_string(passed_type).blue(),
            self.type_id_to_string(slot_type).blue()
        );
        let counts = self.types.type_variable_counts.get(slot_type);
        if counts.inference_variable_count == 0 {
            debug!("no type holes: {}", self.type_id_to_string(slot_type));
            return TypeUnificationResult::NoHoles;
        }

        // This special case is removable, all tests pass, but I believe its currently
        // a slight optimization, and would be more of one with more complex types
        if let (Some(passed_info), Some(arg_info)) =
            (self.types.get_instance_info(passed_type), self.types.get_instance_info(slot_type))
        {
            // expr: NewList[int] arg: NewList['0]
            if passed_info.generic_parent == arg_info.generic_parent {
                debug!(
                    "comparing generic instances of {}",
                    self.type_id_to_string(arg_info.generic_parent)
                );
                // We can directly 'solve' every appearance of a type param here
                for (passed_type, arg_slot) in
                    self.types.mem.getn_zip(passed_info.type_args, arg_info.type_args)
                {
                    self.unify_and_find_substitutions_rec(*passed_type, *arg_slot);
                }
            } else {
                debug!("compared generic instances but they didn't match parent types");
            }
            return TypeUnificationResult::Matching;
        }

        let passed_type = match self.types.get(passed_type) {
            Type::TypeParameter(tp) if tp.static_constraint.is_some() => {
                tp.static_constraint.unwrap()
            }
            _ => passed_type,
        };

        let slot_type = match self.types.get(slot_type) {
            Type::TypeParameter(tp) if tp.static_constraint.is_some() => {
                tp.static_constraint.unwrap()
            }
            _ => slot_type,
        };

        debug!(
            "  RESOLVED unify_and_find_substitutions.\n  passed {}\n  slot   {}",
            self.type_id_to_string(passed_type).blue(),
            self.type_id_to_string(slot_type).blue()
        );

        match (self.types.get(passed_type), self.types.get(slot_type)) {
            _ if passed_type == slot_type => TypeUnificationResult::Matching,
            (Type::InferenceHole(_passed_hole), _slot_type) => {
                // Note: We may eventually need an 'occurs' check to prevent recursive
                // substitutions; for now they don't seem to be occurring though, and it'll
                // be obvious if they ever do
                self.set_hole_solution(passed_type, slot_type);
                TypeUnificationResult::Matching
            }
            (_passed_type, Type::InferenceHole(_slot_hole)) => {
                // Note: We may eventually need an 'occurs' check to prevent recursive
                // substitutions; for now they don't seem to be occurring though, and it'll
                // be obvious if they ever do
                self.set_hole_solution(slot_type, passed_type);
                TypeUnificationResult::Matching
            }
            (Type::Reference(passed_ref), slot) => {
                // For the purposes of inference, we ignore the difference between
                // write and read pointers, since they'll still fail typecheck in the end
                // but we'd like to infer successfully to get a good message
                match slot.as_reference() {
                    // This performs a single-layer deref.
                    // This improves error messages, and also allows us to infer correctly even when an auto-deref is needed
                    None => self.unify_and_find_substitutions_rec(passed_ref.inner_type, slot_type),

                    // Here in infer/unify, we're really after capturing the programmer's intent as
                    // best as we can, and typechecking/coercion takes care of whether to ultimately
                    // let the code through, so we can be a little generous
                    Some(slot_ref) => self.unify_and_find_substitutions_rec(
                        passed_ref.inner_type,
                        slot_ref.inner_type,
                    ),
                }
            }
            (passed, Type::Reference(slot_reference)) if passed.as_reference().is_none() => {
                // We expect a reference and provide a non-reference
                // Unify as if the address_of rule is applied, but only if the inner type would match
                // Actually we have to just do it, because it could be a hole or a type variable
                self.unify_and_find_substitutions_rec(passed_type, slot_reference.inner_type)
            }
            (Type::Struct(passed_struct), Type::Struct(struc)) => {
                // Structs must have all same field names in same order

                // One day, we could do better, infer on as many fields match...
                let passed_fields = passed_struct.fields;
                let fields = struc.fields;
                if passed_fields.len() != fields.len() {
                    return TypeUnificationResult::NonMatching("field count");
                }
                for (idx, field) in self.types.mem.getn(fields).iter().enumerate() {
                    let passed_field = self.types.mem.get_nth(passed_fields, idx);
                    if field.name != passed_field.name {
                        return TypeUnificationResult::NonMatching("field names");
                    }
                    self.unify_and_find_substitutions_rec(passed_field.type_id, field.type_id);
                }
                TypeUnificationResult::Matching
            }
            (Type::Sum(passed_enum), Type::Sum(param_enum_type)) => {
                // Enum example
                // type Result<T, E> = enum Ok(T) | Err(E)
                // fn unwrap<T, E>(self: Result<T, E>): T {
                //  (self as Result<T,E>.Ok).payload
                // }
                // unwrap(Result<int, string>.Ok(1))
                // passed_expr: Result<int, string>, argument_type: Result<T, E>
                // passed_expr: enum Ok(int), Err(string), argument_type: enum Ok(T), Err(E)
                // Enum must have same variants with same tags, walk each variant and recurse on its payload
                let passed_variants = passed_enum.variants;
                let variants = param_enum_type.variants;
                if passed_variants.len() != variants.len() {
                    return TypeUnificationResult::NonMatching("variant count");
                }
                for (variant, passed_variant) in
                    self.types.mem.getn(variants).iter().zip(self.types.mem.getn(passed_variants))
                {
                    if variant.name != passed_variant.name {
                        return TypeUnificationResult::NonMatching("variant names");
                    }
                    if let Some(passed_payload) = passed_variant.payload {
                        if let Some(param_payload) = variant.payload {
                            self.unify_and_find_substitutions_rec(passed_payload, param_payload);
                        } else {
                            return TypeUnificationResult::NonMatching("payloads");
                        }
                    }
                }

                TypeUnificationResult::Matching
            }
            (Type::Array(passed_array), Type::Array(slot_array)) => {
                let passed_array_element_type = passed_array.element_type;
                let slot_array_element_type = slot_array.element_type;
                self.unify_and_find_substitutions_rec(passed_array.size_type, slot_array.size_type);
                self.unify_and_find_substitutions_rec(
                    passed_array_element_type,
                    slot_array_element_type,
                );
                TypeUnificationResult::Matching
            }
            (Type::Function(passed_fn), Type::Function(slot_fn)) => {
                if passed_fn.logical_params().len() == slot_fn.logical_params().len() {
                    let passed_fn = *passed_fn;
                    let slot_fn = *slot_fn;
                    for (passed_param, slot_param) in self
                        .types
                        .mem
                        .getn(passed_fn.logical_params())
                        .iter()
                        .zip(self.types.mem.getn(slot_fn.logical_params()))
                    {
                        self.unify_and_find_substitutions_rec(
                            passed_param.type_id,
                            slot_param.type_id,
                        );
                    }
                    self.unify_and_find_substitutions_rec(
                        passed_fn.return_type,
                        slot_fn.return_type,
                    )
                } else {
                    TypeUnificationResult::NonMatching(
                        "Functions take a different number of arguments",
                    )
                }
            }
            (Type::FunctionPointer(fp1), Type::FunctionPointer(fp2)) => {
                self.unify_and_find_substitutions_rec(fp1.function_type_id, fp2.function_type_id)
            }
            (passed, Type::FunctionTypeParameter(slot_function_type_param)) => {
                if let Some(passed_function_type) =
                    self.extract_function_type_from_functionlike(passed)
                {
                    self.unify_and_find_substitutions_rec(
                        passed_function_type,
                        slot_function_type_param.function_type,
                    )
                } else {
                    TypeUnificationResult::NonMatching(
                        "Expected a function type parameter; passed unrelated",
                    )
                }
            }
            (Type::Lambda(passed_lambda_id), Type::LambdaObject(slot_lambda_obj)) => {
                let passed_lambda = self.types.lambda_types.get(*passed_lambda_id);
                self.unify_and_find_substitutions_rec(
                    passed_lambda.function_type,
                    slot_lambda_obj.function_type,
                )
            }
            (Type::LambdaObject(passed_lambda), Type::LambdaObject(param_lambda)) => self
                .unify_and_find_substitutions_rec(
                    passed_lambda.function_type,
                    param_lambda.function_type,
                ),
            (Type::StaticValue(passed_value_type), Type::StaticValue(param_value_type)) => self
                .unify_and_find_substitutions_rec(
                    passed_value_type.family_type_id,
                    param_value_type.family_type_id,
                ),
            (Type::StaticValue(passed_static), _non_static) => {
                // We infer 'through' statics, even though they'll fail typecheck without a cast.
                // This improves error messages, as we essentially end up solving the user's intent,
                // then telling them why their intent doesn't compile
                self.unify_and_find_substitutions_rec(passed_static.family_type_id, slot_type)
            }
            (_non_value_passed, Type::StaticValue(value_type_slot))
                if value_type_slot.value_id.is_none() =>
            {
                // We infer 'through' value types, even though they'll fail typecheck without a cast.
                // This improves error messages, as we essentially end up solving the user's intent,
                // then telling them why their intent doesn't compile
                self.unify_and_find_substitutions_rec(passed_type, value_type_slot.family_type_id)
            }

            // --------------- MISS CASES: we detect further explicit cases for better error message ---------------------
            (passed_type, Type::Reference(_slot_t)) if passed_type.as_reference().is_none() => {
                TypeUnificationResult::NonMatching("Consider passing a reference")
            }
            _ => {
                debug!("  -> Non-Matching");
                // If you see 'Unrelated types', consider coming here and adding a case; its
                // a little rude and sometimes not even true
                TypeUnificationResult::NonMatching("Unrelated types")
            }
        }
    }

    pub(super) fn zip_named_types_to_subst_pairs<const N: usize>(
        &self,
        from: MSlice<NameAndType, TypedProgram>,
        to: MSlice<NameAndType, TypedProgram>,
    ) -> SmallVec<[TypeSubstitutionPair; N]>
    where
        [TypeSubstitutionPair; N]: smallvec::Array<Item = TypeSubstitutionPair>,
    {
        let mut pairs = smallvec![];
        for (from, to) in self.mem.getn(from).iter().zip(self.mem.getn(to)) {
            pairs.push(spair! { from.type_id => to.type_id });
        }
        pairs
    }
}
