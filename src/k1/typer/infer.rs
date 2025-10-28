// Copyright (c) 2025 knix
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
    ) -> TyperResult<(NamedTypeSlice, NamedTypeSlice)> {
        if self.inference_context_stack.is_empty() {
            self.ictx_push();
        }

        let ictx = self.ictx_mut();
        ictx.origin_stack.push(span);
        let infer_depth = ictx.origin_stack.len();
        if infer_depth == 1 {
            let raw = self.timing.clock.raw();
            self.ictx_mut().start_raw = raw;
        }

        let mut self_ = scopeguard::guard(self, |self_| {
            let clock = &self_.timing.clock;
            let start = self_.ictx().start_raw;
            let end = clock.raw();
            let elapsed = clock.delta_as_nanos(start, end);

            let ictx = self_.ictx_mut();
            let id = ictx.origin_stack.pop().unwrap();
            debug_assert_eq!(id, span);
            if ictx.origin_stack.is_empty() {
                self_.timing.total_infers += 1;
                self_.timing.total_infer_nanos += elapsed;

                debug!("Resetting inference context");
                self_.ictx_pop();
            } else {
                debug!(
                    "Not resetting inference buffers: inference depth is nonzero: {}",
                    ictx.origin_stack.len()
                );
            }
        });

        // Stores the mapping from the function (or type's) type parameters to their
        // corresponding instantiated type holes for this inference context
        let mut params_to_holes: SV8<TypeSubstitutionPair> =
            SmallVec::with_capacity(all_type_params.len());

        let inference_var_count = self_.ictx().inference_vars.len() as u32;
        self_.ictx_mut().params.extend(all_type_params.iter().map(|nt| nt.type_id));
        for (idx, param) in all_type_params.iter().enumerate() {
            let hole_index = idx as u32 + inference_var_count;

            let type_hole = self_
                .types
                .add_anon(Type::InferenceHole(InferenceHoleType { index: hole_index as u32 }));

            // This was an experiment in instantiating the type to something that preserved
            // structure for statically constrained type params to guide inference better, and
            // recover info via a constraint. The core idea worked, unfortunately it was
            // ill-conceived in that it produced scenarios that were impossible to solve correctly
            // let is_static = self_
            //     .types
            //     .get(param.type_id)
            //     .as_type_parameter()
            //     .unwrap()
            //     .static_constraint
            //     .is_some();
            // let instantiated_param = if is_static {
            // If the type parameter is of the following format: [T: static string](t: T)
            // The instantiation set should be T -> '0, constrain '0 -> static['1]
            // And we refer to th

            // let type_hole_2 = self_.types.add_anon(Type::InferenceHole(InferenceHoleType {
            //     index: idx as u32 + type_hole_offset + 100,
            // })); // '2
            // type_hole_offset += 1;
            // let static_wrapper = self_.types.add_static_type(type_hole_2, None);
            // eprintln!(
            //     "Adding constraint for extra hole: {}",
            //     self_.pretty_print_type_substitutions(
            //         &[spair! { type_hole => static_wrapper }],
            //         ", "
            //     )
            // );
            // self_.ictx_mut().constraints.push(spair! { type_hole => static_wrapper });
            // static_wrapper // static['1]
            //     type_hole
            // } else {
            //     type_hole
            // };
            // let instantiated_param = type_hole;
            //instantiation_set.push(spair! { param.type_id() => instantiated_param });
            self_.ictx_mut().inference_vars.push(type_hole);
            params_to_holes.push(spair! { param.type_id() => type_hole });
        }
        debug!(
            "[infer {infer_depth}] Instantiation set is:\n{}",
            self_.pretty_print_type_substitutions(&params_to_holes, "\n")
        );

        // Used for the error message, mainly
        let mut argument_types: SmallVec<[TypeId; 8]> = smallvec![];
        // The heart of the inference process:
        // - For each pair of (<actual type> <expected type>), we 'pattern match' on the types
        //   and learn what we can about all of the type holes
        // - We have to 'instantiate' the parameter type first, which means replacing occurrences
        //   of type parameters like 'T' with inference holes like '0
        for (index, InferenceInputPair { arg: expr, param_type: gen_param, allow_mismatch }) in
            inference_pairs.iter().enumerate()
        {
            let instantiated_param_type = self_.substitute_in_type(*gen_param, &params_to_holes);
            debug!(
                "[infer {infer_depth}] Instantiated parameter type for inference. Was: {}, is: {}",
                self_.type_id_to_string(*gen_param),
                self_.type_id_to_string(instantiated_param_type)
            );

            debug!(
                "[infer {infer_depth}] Inferring call argument {} / {}",
                index + 1,
                inference_pairs.len()
            );

            let s = std::mem::take(&mut self_.ictx_mut().substitutions_vec);
            // Calculating the 'expected_type_so_far' is an extra step that gives us better
            // results; we're able to get 'better' types from later arguments if we've already
            // learned some things from prior ones.
            let expected_type_so_far = self_.substitute_in_type(instantiated_param_type, &s);
            debug!(
                "[infer {infer_depth}] Set is \n{}",
                self_.pretty_print_type_substitutions(&s, "\n"),
            );
            debug!(
                "[infer {infer_depth}] Expected type is: {}",
                self_.type_id_to_string(expected_type_so_far)
            );
            self_.ictx_mut().substitutions_vec = s;

            let (argument_type, argument_span) = match expr {
                TypeOrParsedExpr::Type(type_id) => (*type_id, span),
                TypeOrParsedExpr::Parsed(parsed_expr) => {
                    let inference_context = EvalExprContext::make(scope_id)
                        .with_inference(true)
                        .with_expected_type(Some(expected_type_so_far));

                    let evaluation_result =
                        self_.eval_expr_with_coercion(*parsed_expr, inference_context, false);
                    match evaluation_result {
                        Ok(expr_id) => {
                            let expr = self_.exprs.get(expr_id);
                            debug!(
                                "[infer {infer_depth}] Actual type is: {}",
                                self_.type_id_to_string(expr.get_type())
                            );
                            (expr.get_type(), expr.get_span())
                        }
                        Err(e) => {
                            // Some expression types, like lambdas, fail really easily if we don't
                            // know the types of the inputs yet. When a lambda fails here, we
                            // choose to continue and report a different, likely more informative,
                            // error
                            let should_skip = match self_.ast.exprs.get(*parsed_expr) {
                                ParsedExpr::Lambda(_) => true,
                                _ => false,
                            };
                            if should_skip {
                                continue;
                            } else {
                                return failf!(
                                    e.span,
                                    "{}\n  Occurred while trying to determine type of argument for inference using expected type: {}",
                                    e.message,
                                    self_.type_id_to_string(expected_type_so_far)
                                );
                            }
                        }
                    }
                }
            };
            argument_types.push(argument_type);
            debug!(
                "[infer {infer_depth}] unify {} =:= {}",
                self_.type_id_to_string(argument_type),
                self_.type_id_to_string(expected_type_so_far),
            );
            // unify_and_find_substitutions populates self.inferences_context.constraints
            if let TypeUnificationResult::NonMatching(msg) =
                self_.unify_and_find_substitutions(argument_type, expected_type_so_far)
            {
                // allow_mismatch is used to avoid reporting a mismatch on the return type,
                // before we're able to learn more about the rest of the inference. We get a better
                // error message if we wait to report the mismatch until the end
                if !allow_mismatch {
                    return failf!(
                        argument_span,
                        "Passed value does not match expected type: expected {} but got {}. {msg}",
                        self_.type_id_to_string(expected_type_so_far),
                        self_.type_id_to_string(argument_type)
                    );
                }
            };

            // After each pair is 'walked', we 'apply' what we learned by calling calculate_inference_substitutions
            let newly_solved_params = self_.calculate_inference_substitutions(span)?;
            for newly_solved_param in &newly_solved_params {
                debug!(
                    "[infer {infer_depth}] ****** GOT NEWLY SOLVED PARAM {} -> {}",
                    self_.type_id_to_string(newly_solved_param.from),
                    self_.type_id_to_string(newly_solved_param.to)
                );
                self_.apply_constraints_to_inferred_type(
                    newly_solved_param.from,
                    newly_solved_param.to,
                    scope_id,
                    argument_span,
                )?;
            }

            debug!(
                "[infer {infer_depth}] all constraints\n\t{}",
                self_.pretty_print_type_substitutions(&self_.ictx().constraints, "\n\t"),
            );
        }

        // TODO: enrich this error, probably do the same thing we're doing below for unsolved
        let final_substitutions = &self_.ictx().substitutions;

        let mut solutions: SV4<NameAndType> = SmallVec::with_capacity(must_solve_params.len());
        let mut unsolved_params: SV8<NameAndType> = smallvec![];
        let mut all_solutions: SV4<NameAndType> = SmallVec::with_capacity(must_solve_params.len());
        for param in all_type_params.iter() {
            let param_to_hole = params_to_holes.iter().find(|p| p.from == param.type_id()).unwrap();
            let corresponding_hole = param_to_hole.to;
            let is_must_solve = self_.named_types.slice_contains(must_solve_params, param);
            if let Some(solution) = final_substitutions.get(&corresponding_hole) {
                all_solutions.push(NameAndType { name: param.name(), type_id: *solution });
                if is_must_solve {
                    solutions.push(NameAndType { name: param.name(), type_id: *solution });
                };
            } else {
                if is_must_solve {
                    unsolved_params.push(*param);
                }
            }
        }
        if !unsolved_params.is_empty() {
            return failf!(
                span,
                "Could not solve for {} given arguments:\n{}\nSolutions:{}",
                unsolved_params
                    .iter()
                    .map(|p| self_.ident_str(p.name()))
                    .collect::<Vec<_>>()
                    .join(", "),
                argument_types
                    .iter()
                    .zip(inference_pairs.iter())
                    .map(|(passed_type, pair)| {
                        format!(
                            "{}: {}",
                            self_.type_id_to_string_ext(*passed_type, false),
                            self_.type_id_to_string_ext(pair.param_type, false),
                        )
                    })
                    .collect::<Vec<_>>()
                    .join("\n"),
                self_.pretty_print_type_substitutions(&self_.ictx().solutions_so_far, ", ")
            );
        }
        debug!("INFER DONE {}", self_.pretty_print_named_types(&solutions, ", "));
        let solutions_handle = self_.named_types.add_slice_copy(&solutions);
        let all_solutions_handle = self_.named_types.add_slice_copy(&all_solutions);
        Ok((solutions_handle, all_solutions_handle))
    }

    /// Called from infer_types
    ///
    /// Each time we get a _concrete_ substitution for a param,
    /// if the param has constraints,
    /// we need to 'learn' from the constraints, adding a pair to the mix
    /// for each ability param and ability impl param in the constraint
    /// Consider:
    /// fn find[T, I: Iterator[Item = T]](i: I, fn: \T -> bool)
    /// find(myList, \i i.isEven())
    ///      ^
    /// We need to learn T from the ability impl for Iterator for myList, so that we can infer the
    /// type of the function param in \i i.isEven()
    pub(crate) fn apply_constraints_to_inferred_type(
        &mut self,
        type_param_id: TypeId,
        solution_type_id: TypeId,
        scope_id: ScopeId,
        span: SpanId,
    ) -> TyperResult<()> {
        let infer_depth = self.ictx().origin_stack.len();
        if let Some(static_type) = self.types.get_type_parameter(type_param_id).static_constraint {
            debug!(
                "[infer {infer_depth}] > learning from STATIC param {} := {}. static type: {}",
                self.type_id_to_string(type_param_id),
                self.type_id_to_string(solution_type_id),
                self.type_id_to_string(static_type),
            );
            let Some(solution_static_type) =
                self.types.get_static_type_id_of_type(solution_type_id)
            else {
                error!("The solution was not a static type; this is probably crashworthy");
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
                    self.calculate_inference_substitutions(span)?;
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
                scope_id,
                span,
            ) {
                Ok(impl_id) => {
                    let the_impl = self.ability_impls.get(impl_id.full_impl_id);
                    let ability_arg_iterator = self
                        .named_types
                        .copy_slice_sv::<4>(
                            self.abilities.get(sig.specialized_ability_id).kind.arguments(),
                        )
                        .into_iter()
                        .zip(self.named_types.copy_slice_sv::<4>(
                            self.abilities.get(the_impl.ability_id).kind.arguments(),
                        ));
                    let impl_arg_iterator = self
                        .named_types
                        .copy_slice_sv::<4>(sig.impl_arguments)
                        .into_iter()
                        .zip(self.named_types.copy_slice_sv::<4>(the_impl.impl_arguments));
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
                                self.calculate_inference_substitutions(span)?;
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

    pub(crate) fn infer_and_constrain_call_type_args(
        &mut self,
        fn_call: &ParsedCall,
        generic_function_sig: FunctionSignature,
        ctx: EvalExprContext,
        args_and_params: &ArgsAndParams,
    ) -> TyperResult<NamedTypeSlice> {
        debug!("infer_and_constrain_call_type_args");
        debug_assert!(generic_function_sig.has_type_params());
        let passed_type_args = fn_call.type_args;
        let passed_type_args_count = passed_type_args.len();
        let type_params_handle = generic_function_sig.type_params;
        let type_params = self.named_types.copy_slice_sv8(generic_function_sig.type_params);
        // Fuse these two paths; where for a given type param,
        // - X *if ALL PASSED, special case to skip inference altogether of course
        let passed_type_args = self.ast.p_type_args.copy_slice_sv8(passed_type_args);
        if !passed_type_args.is_empty() && passed_type_args.len() != type_params.len() {
            return failf!(
                fn_call.span,
                "Expected {} type arguments but got {}",
                type_params.len(),
                passed_type_args_count
            );
        }
        let all_passed = passed_type_args.len() == type_params.len()
            && passed_type_args.iter().all(|nt| nt.type_expr.is_some());
        debug!("all_passed={all_passed}");
        let solved_type_params = if all_passed {
            let mut evaled_params: SV4<NameAndType> =
                SmallVec::with_capacity(passed_type_args_count);
            for (type_param, type_arg) in type_params.iter().zip(passed_type_args.iter()) {
                let passed_expr = type_arg.type_expr.unwrap(); // checked by all_passed
                let passed_type = self.eval_type_expr(passed_expr, ctx.scope_id)?;
                evaled_params.push(NameAndType { name: type_param.name, type_id: passed_type });
            }
            self.named_types.add_slice_copy(&evaled_params)
        } else {
            let generic_function_type =
                self.types.get(generic_function_sig.function_type).as_function().unwrap().clone();
            let generic_function_return_type = generic_function_type.return_type;

            let mut inference_pairs: SV8<_> = smallvec![];

            // We add an inference pair for every type the user explicitly passed in
            //
            // We add these first so that they get applied first, and following conflicts are
            // framed in terms of these being true; if users says T := Pointer, we later say
            // "expected Pointer" when other params dont line up.
            for (index, type_arg) in passed_type_args.iter().enumerate() {
                if let Some(passed_type_expr) = type_arg.type_expr {
                    let matching_param = if let Some(passed_name) = type_arg.name {
                        let matching_param = type_params.iter().find(|nt| nt.name == passed_name);
                        matching_param
                    } else {
                        // Use position
                        type_params.get(index)
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
            inference_pairs.extend(args_and_params.iter().map(|(expr, param)| {
                let passed_type = match expr {
                    MaybeTypedExpr::Parsed(expr_id) => TypeOrParsedExpr::Parsed(*expr_id),
                    MaybeTypedExpr::Typed(expr) => {
                        TypeOrParsedExpr::Type(self.exprs.get(*expr).get_type())
                    }
                };
                InferenceInputPair {
                    arg: passed_type,
                    param_type: param.type_id,
                    allow_mismatch: false,
                }
            }));

            let (solutions, _all_solutions) = self
                .infer_types(
                    &type_params,
                    type_params_handle,
                    &inference_pairs,
                    fn_call.span,
                    ctx.scope_id,
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
            self.zip_named_types_to_subst_pairs(type_params_handle, solved_type_params);
        for (solution, type_param) in
            self.named_types.copy_slice_sv4(solved_type_params).iter().zip(type_params.iter())
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
        Ok(solved_type_params)
    }

    pub(crate) fn determine_function_type_args_for_call(
        &mut self,
        original_function_sig: FunctionSignature,
        type_args: NamedTypeSlice,
        args_and_params: &ArgsAndParams,
        ctx: EvalExprContext,
    ) -> TyperResult<NamedTypeSlice> {
        // Ok here's what we need for function params. We need to know just the _kind_ of function that
        // was passed: ref, lambda, or lambda obj, and we need to specialize the function shape on
        // the other type params, as in: some (T -> T) -> some (int -> int), THEN just create
        // a type using the _kind_ we need:
        // fn ref: some (int -> int) -> (int -> int)*
        // lambda: some (int -> int) -> unique(\int -> int) (is this the zero-sized param? Oh, its physically just passing the environment!)
        // lambda obj: some (int -> int) -> dyn[\int -> int] (passing environment AND fn ptr)
        //
        // This method is risk-free in terms of 'leaking' inference types out
        let mut function_type_args: SmallVec<[NameAndType; 8]> = SmallVec::new();
        if !original_function_sig.function_type_params.is_empty() {
            let subst_pairs: SmallVec<[_; 8]> = self
                .named_types
                .get_slice(original_function_sig.type_params)
                .iter()
                .zip(self.named_types.get_slice(type_args).iter())
                .map(|(param, arg)| TypeSubstitutionPair { from: param.type_id, to: arg.type_id })
                .collect();
            for function_type_param in self
                .function_type_params
                .copy_slice_sv::<4>(original_function_sig.function_type_params)
                .iter()
            {
                let (corresponding_arg, corresponding_value_param) =
                    args_and_params.get(function_type_param.value_param_index as usize);
                debug!(
                    "The param for function_type_param {} {} is {} and passed: {:?}",
                    function_type_param.type_id,
                    self.ident_str(function_type_param.name),
                    self.ident_str(corresponding_value_param.name),
                    corresponding_arg
                );

                enum PhysicalPassedFunction {
                    Lambda(TypeId),
                    FunctionPointer,
                    LambdaObject(TypeId),
                }
                let physical_passed_function = match corresponding_arg {
                    MaybeTypedExpr::Typed(_) => {
                        unreachable!("Synthesizing calls with function type params is unsupported")
                    }
                    MaybeTypedExpr::Parsed(p) => match self.ast.exprs.get(p) {
                        ParsedExpr::Lambda(_lam) => {
                            debug!("substituting type for an ftp lambda so that it can infer");
                            let substituted_param_type =
                                self.substitute_in_type(function_type_param.type_id, &subst_pairs);
                            let the_lambda = self.eval_expr(
                                p,
                                ctx.with_expected_type(Some(substituted_param_type)),
                            )?;
                            let lambda_type = self.exprs.get(the_lambda).get_type();
                            debug!(
                                "Using a Lambda as an ftp: {}",
                                self.type_id_to_string(lambda_type)
                            );
                            PhysicalPassedFunction::Lambda(lambda_type)
                        }
                        _other => {
                            let t = self.eval_expr(p, ctx.with_no_expected_type())?;
                            let type_id = self.exprs.get(t).get_type();
                            match self.types.get(type_id) {
                                Type::Lambda(_) => PhysicalPassedFunction::Lambda(type_id),
                                Type::LambdaObject(_) => {
                                    debug!("Using a LambdaObject as an ftp");
                                    PhysicalPassedFunction::LambdaObject(type_id)
                                }
                                Type::FunctionPointer(_) => {
                                    debug!("Using a FunctionPointer as an ftp");
                                    PhysicalPassedFunction::FunctionPointer
                                }
                                _ => {
                                    let span = self.exprs.get(t).get_span();
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
                let final_parameter_type = match physical_passed_function {
                    PhysicalPassedFunction::Lambda(lambda) => {
                        // Can use as-is since we rebuilt this lambda already
                        lambda
                    }
                    PhysicalPassedFunction::FunctionPointer => {
                        let ftp = self
                            .types
                            .get(function_type_param.type_id)
                            .as_function_type_parameter()
                            .unwrap();
                        let original_param_function_type = ftp.function_type;
                        let substituted_function_type =
                            self.substitute_in_type(original_param_function_type, &subst_pairs);
                        self.types.add_function_pointer_type(substituted_function_type)
                    }
                    PhysicalPassedFunction::LambdaObject(lambda_object_type) => {
                        // Replace the function type
                        let substituted_lambda_object_type =
                            self.substitute_in_type(lambda_object_type, &subst_pairs);
                        substituted_lambda_object_type
                    }
                };
                function_type_args.push(NameAndType {
                    name: function_type_param.name,
                    type_id: final_parameter_type,
                });
            }
        }
        if !function_type_args.is_empty() {
            debug!(
                "We're passing function_type_args! {}",
                self.pretty_print_named_types(&function_type_args, ", ")
            );
        }
        let function_type_args_handle = self.named_types.add_slice_copy(&function_type_args);
        Ok(function_type_args_handle)
    }

    fn add_substitution(&mut self, pair: TypeSubstitutionPair) {
        debug!(
            "Applying substitution {} -> {} to set {}",
            self.type_id_to_string(pair.from),
            self.type_id_to_string(pair.to),
            self.pretty_print_type_substitutions(&self.ictx().constraints, ", ")
        );
        if pair.from == pair.to {
            return;
        }
        let mut set = std::mem::take(&mut self.ictx_mut().constraints);
        set.iter_mut().for_each(|existing| {
            if existing.from == pair.from {
                existing.from = pair.to
            } else if existing.to == pair.from {
                existing.to = pair.to
            } else {
                existing.from = self.substitute_in_type(existing.from, &[pair]);
                existing.to = self.substitute_in_type(existing.to, &[pair]);
            };
        });
        set.retain(|pair| pair.from != pair.to);
        set.push(pair);

        debug!("Got set {}", self.pretty_print_type_substitutions(&set, ", "));

        self.ictx_mut().constraints = set;
    }

    /// Returns: Any newly, fully solved params after applying constraints
    fn calculate_inference_substitutions(
        &mut self,
        span: SpanId,
    ) -> TyperResult<SV4<TypeSubstitutionPair>> {
        let mut ctx = self.ictx_take();

        debug!(
            "calculate_inference_substitutions. constraints: [{}]",
            self.pretty_print_type_substitutions(&ctx.constraints, ", ")
        );
        ctx.substitutions.clear();
        ctx.substitutions_vec.clear();

        let final_pairs = &mut ctx.substitutions;
        for subst in &ctx.constraints {
            // 1. Validity
            // This may be unnecessary since we are passing in our 'current guess'
            // as the expected type once we have one, so we'll just get a failure when
            // evaluating that node rather than an inconsistent substitution

            //match self.types.get(subst.from) {
            //    Type::TypeVariable(tv) if tv.is_inference_variable => {}
            //    from => match self.types.get(subst.to) {
            //        Type::TypeVariable(tv) if tv.is_inference_variable => {}
            //        to => {
            //            if subst.from != subst.to {
            //                return failf!(
            //                    span,
            //                    "Contradicting substitution: {} -> {}",
            //                    self.type_id_to_string(subst.from),
            //                    self.type_id_to_string(subst.to)
            //                );
            //            }
            //        }
            //    },
            //}

            // 2. Consistency
            match final_pairs.entry(subst.from) {
                std::collections::hash_map::Entry::Vacant(e) => {
                    ctx.substitutions_vec.push(*subst);
                    e.insert(subst.to);
                }
                std::collections::hash_map::Entry::Occupied(occ) => {
                    let dest = occ.get();
                    if *dest != subst.to {
                        // TODO: We should include attribution spans on substitutions so that we
                        // can report to the user _why_ we expect such and such a value to be of
                        // a certain type
                        let e = failf!(
                            span,
                            "Type {} needs to be {} but also needs to be {}",
                            self.type_id_to_string(subst.from),
                            self.type_id_to_string(subst.to),
                            self.type_id_to_string(*dest),
                        );
                        self.inference_context_stack.push(ctx);
                        return e;
                    }
                }
            }
        }

        // Look for new 'fully solved' params among the set. This is used to trigger behaviors
        // sometimes. For example, once we solve a type parameter, we then look up its constraints
        // and add information from them into the inference context
        let mut newly_solved_params: SV4<TypeSubstitutionPair> = smallvec![];
        for (solved_from, solved_to) in final_pairs.iter() {
            let is_fully_solved =
                self.types.get_contained_type_variable_counts(*solved_to).inference_variable_count
                    == 0;
            if !is_fully_solved {
                continue;
            }

            // If the thing we've solved is one of the param holes themselves
            // e.g., '0
            let Some(inference_var_index) =
                ctx.inference_vars.iter().position(|t| *t == *solved_from)
            else {
                continue;
            };
            let original_param = ctx.params[inference_var_index];

            debug!(
                "final_pair {} -> {}",
                self.type_id_to_string(*solved_from),
                self.type_id_to_string(*solved_to)
            );

            // Then find the type param matching the 'from' type, T
            // let Some(type_param) = self
            //     .named_types
            //     .get_slice(all_type_params)
            //     .iter()
            //     .find(|nt| nt.type_id == original_param.from)
            //     .copied()
            // else {
            //     continue;
            // };
            //

            if !ctx.solutions_so_far.iter().any(|pair| pair.from == original_param) {
                newly_solved_params.push(spair! { original_param => *solved_to });
            }
            ctx.solutions_so_far.push(spair! { original_param => *solved_to })
        }
        self.inference_context_stack.push(ctx);
        Ok(newly_solved_params)
    }

    /// Used for fixing up the constraint signatures:
    /// For each param they mention, if its solved, use the solution, otherwise use the
    /// inference hole so we can learn more about it
    /// This is probably a re-usable concept for inference context, I think its probably quite
    /// often that we want this mapping for each param
    fn make_inference_substitution_set(&self) -> SV8<TypeSubstitutionPair> {
        let mut subst_set: SV8<TypeSubstitutionPair> = smallvec![];
        let ictx = self.ictx();
        for (param, inference_hole) in ictx.params.iter().zip(ictx.inference_vars.iter()) {
            if let Some(solution) = ictx.solutions_so_far.iter().find(|p| p.from == *param) {
                subst_set.push(*solution)
            } else {
                subst_set.push(spair! { *param => *inference_hole })
            }
        }
        subst_set
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
                for (passed_type, arg_slot) in self
                    .types
                    .type_slices
                    .copy_slice_sv4(passed_info.type_args)
                    .iter()
                    .zip(self.types.type_slices.copy_slice_sv4(arg_info.type_args).iter())
                {
                    self.unify_and_find_substitutions_rec(*passed_type, *arg_slot);
                }
            } else {
                debug!("compared generic instances but they didn't match parent types");
            }
            return TypeUnificationResult::Matching;
        }

        let passed_type = match self.types.get_no_follow(passed_type) {
            Type::TypeParameter(tp) if tp.static_constraint.is_some() => {
                tp.static_constraint.unwrap()
            }
            _ => passed_type,
        };

        let slot_type = match self.types.get_no_follow(slot_type) {
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

        match (self.types.get_no_follow(passed_type), self.types.get_no_follow(slot_type)) {
            (Type::InferenceHole(_passed_hole), _slot_type) => {
                // Note: We may eventually need an 'occurs' check to prevent recursive
                // substitutions; for now they don't seem to be occurring though, and it'll
                // be obvious if they ever do
                self.add_substitution(TypeSubstitutionPair { from: passed_type, to: slot_type });
                TypeUnificationResult::Matching
            }
            (_passed_type, Type::InferenceHole(_slot_hole)) => {
                // Note: We may eventually need an 'occurs' check to prevent recursive
                // substitutions; for now they don't seem to be occurring though, and it'll
                // be obvious if they ever do
                self.add_substitution(TypeSubstitutionPair { from: slot_type, to: passed_type });
                TypeUnificationResult::Matching
            }
            (Type::Reference(passed_refer), Type::Reference(refer)) => self
                // For the purposes of inference, we ignore the difference between
                // write and read pointers, since they'll still fail typecheck in the end
                // but we'd like to infer successfully to get a good message
                .unify_and_find_substitutions_rec(passed_refer.inner_type, refer.inner_type),
            (Type::Struct(passed_struct), Type::Struct(struc)) => {
                // Struct example:
                // type Pair<T, U> = { a: T, b: U }
                // fn get_first<T, U>(p: Pair<T, U>): T { p.a }
                // get_first({ a: 1, b: 2})
                // passed_expr: Pair<int, int>, argument_type: Pair<T, U>
                // passed expr: { a: int, b: int }, argument_type: { a: T, b: U }
                //
                // Structs must have all same field names in same order
                let passed_fields = passed_struct.fields;
                let fields = struc.fields;
                if passed_fields.len() != fields.len() {
                    return TypeUnificationResult::NonMatching("field count");
                }
                for (idx, field) in self.types.mem.get_slice(fields).iter().enumerate() {
                    let passed_field = self.types.mem.get_nth(passed_fields, idx);
                    if field.name != passed_field.name {
                        return TypeUnificationResult::NonMatching("field names");
                    }
                    self.unify_and_find_substitutions_rec(passed_field.type_id, field.type_id);
                }
                TypeUnificationResult::Matching
            }
            (Type::Enum(passed_enum), Type::Enum(param_enum_type)) => {
                // Enum example
                // type Result<T, E> = enum Ok(T) | Err(E)
                // fn unwrap<T, E>(self: Result<T, E>): T {
                //  (self as Result<T,E>.Ok).payload
                // }
                // unwrap(Result<int, string>.Ok(1))
                // passed_expr: Result<int, string>, argument_type: Result<T, E>
                // passed_expr: enum Ok(int), Err(string), argument_type: enum Ok(T), Err(E)
                // Enum must have same variants with same tags, walk each variant and recurse on its payload
                let passed_variants = passed_enum.variants.clone();
                let variants = param_enum_type.variants.clone();
                if passed_variants.len() != variants.len() {
                    return TypeUnificationResult::NonMatching("variant count");
                }
                for (idx, variant) in variants.iter().enumerate() {
                    let passed_variant = &passed_variants[idx];
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
            (Type::EnumVariant(passed_enum_variant), Type::Enum(_param_enum_type_variant)) => {
                self.unify_and_find_substitutions_rec(passed_enum_variant.enum_type_id, slot_type)
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
                    let passed_fn = passed_fn.clone();
                    let slot_fn = slot_fn.clone();
                    for (passed_param, slot_param) in self
                        .types
                        .mem
                        .get_slice(passed_fn.logical_params())
                        .iter()
                        .zip(self.types.mem.get_slice(slot_fn.logical_params()))
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
            (Type::Lambda(passed_lambda), Type::LambdaObject(param_lambda)) => self
                .unify_and_find_substitutions_rec(
                    passed_lambda.function_type,
                    param_lambda.function_type,
                ),
            (Type::LambdaObject(passed_lambda), Type::LambdaObject(param_lambda)) => self
                .unify_and_find_substitutions_rec(
                    passed_lambda.function_type,
                    param_lambda.function_type,
                ),
            (Type::Static(passed_static), Type::Static(param_static)) => self
                .unify_and_find_substitutions_rec(
                    passed_static.inner_type_id,
                    param_static.inner_type_id,
                ),
            (Type::Static(passed_static), _non_static) => {
                self.unify_and_find_substitutions_rec(passed_static.inner_type_id, slot_type)
            }
            (_non_static_passed, Type::Static(static_slot)) if static_slot.value_id.is_none() => {
                self.unify_and_find_substitutions_rec(passed_type, static_slot.inner_type_id)
            }
            _ if passed_type == slot_type => TypeUnificationResult::Matching,
            _ => {
                debug!("  -> Non-Matching");
                TypeUnificationResult::NonMatching("Unrelated types")
            }
        }
    }

    pub(super) fn zip_named_types_to_subst_pairs<const N: usize>(
        &self,
        from: SliceHandle<NameAndTypeId>,
        to: SliceHandle<NameAndTypeId>,
    ) -> SmallVec<[TypeSubstitutionPair; N]>
    where
        [TypeSubstitutionPair; N]: smallvec::Array<Item = TypeSubstitutionPair>,
    {
        let mut pairs = smallvec![];
        for (from, to) in
            self.named_types.get_slice(from).iter().zip(self.named_types.get_slice(to))
        {
            pairs.push(spair! { from.type_id => to.type_id });
        }
        pairs
    }
}
