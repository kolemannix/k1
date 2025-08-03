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
        all_type_params: NamedTypeSlice,
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
        self.inference_context.origin_stack.push(span);
        let infer_depth = self.inference_context.origin_stack.len();

        let mut self_ = scopeguard::guard(self, |self_| {
            let id = self_.inference_context.origin_stack.pop().unwrap();
            debug_assert!(id == span);
            if self_.inference_context.origin_stack.is_empty() {
                debug!("Resetting inference buffers since stack is empty");
                self_.inference_context.constraints.clear();
                self_.inference_context.params.clear();
                self_.inference_context.solutions_so_far.clear();
                self_.inference_context.inference_vars.clear();
                self_.inference_context.substitutions.clear();
                self_.inference_context.substitutions_vec.clear();
            } else {
                debug!(
                    "Not resetting inference buffers: inference depth is nonzero: {}",
                    self_.inference_context.origin_stack.len()
                );
            }
        });

        // Stores the mapping from the function (or type's) type parameters to their
        // corresponding instantiated type holes for this inference context
        let mut instantiation_set: SV8<TypeSubstitutionPair> =
            SmallVec::with_capacity(all_type_params.len());

        let inference_var_count = self_.inference_context.inference_vars.len();
        let all_type_params_slice = self_.named_types.copy_slice_sv::<8>(all_type_params);
        self_.inference_context.params.extend(all_type_params_slice.iter().map(|nt| nt.type_id));
        for (idx, param) in all_type_params_slice.iter().enumerate() {
            let hole_index = idx + inference_var_count;

            let type_hole = self_
                .types
                .add_anon(Type::InferenceHole(InferenceHoleType { index: hole_index as u32 }));
            self_.inference_context.inference_vars.push(type_hole);
            instantiation_set.push(TypeSubstitutionPair { from: param.type_id(), to: type_hole });
        }

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
            let instantiated_param_type = self_.substitute_in_type(*gen_param, &instantiation_set);
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

            let s = std::mem::take(&mut self_.inference_context.substitutions_vec);
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
            self_.inference_context.substitutions_vec = s;

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
                                    "Failed to determine type of argument for inference. {} (used expected type: {})",
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
                self_.pretty_print_type_substitutions(&self_.inference_context.constraints, "\n\t"),
            );
        }

        // TODO: enrich this error, probably do the same thing we're doing below for unsolved
        let final_substitutions = &self_.inference_context.substitutions;

        let mut solutions: SV4<NameAndType> = SmallVec::with_capacity(must_solve_params.len());
        let mut unsolved_params: SV8<NameAndType> = smallvec![];
        let mut all_solutions: SV4<NameAndType> = SmallVec::with_capacity(must_solve_params.len());
        for param in self_.named_types.get_slice(all_type_params) {
            let param_to_hole =
                instantiation_set.iter().find(|p| p.from == param.type_id()).unwrap();
            let corresponding_hole = param_to_hole.to;
            let is_must_solve = self_.named_types.slice_contains(must_solve_params, param);
            if let Some(solution) = final_substitutions.get(&corresponding_hole) {
                if is_must_solve {
                    solutions.push(NameAndType { name: param.name(), type_id: *solution });
                };
                all_solutions.push(NameAndType { name: param.name(), type_id: *solution });
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
                self_.pretty_print_type_substitutions(
                    &self_.inference_context.solutions_so_far,
                    ", "
                )
            );
        }
        debug!("INFER DONE {}", self_.pretty_print_named_types(&solutions, ", "));
        let solutions_handle = self_.named_types.add_slice_from_copy_slice(&solutions);
        let all_solutions_handle = self_.named_types.add_slice_from_copy_slice(&all_solutions);
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
        let infer_depth = self.inference_context.origin_stack.len();
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
        known_args: Option<(&[TypeId], &[TypedExprId])>,
        ctx: EvalExprContext,
    ) -> TyperResult<NamedTypeSlice> {
        debug_assert!(generic_function_sig.is_generic());
        let passed_type_args = fn_call.type_args;
        let passed_type_args_count = passed_type_args.len();
        let solved_type_params = if !passed_type_args.is_empty() {
            if passed_type_args_count != generic_function_sig.type_params.len() {
                return failf!(
                    fn_call.span,
                    "Expected {} type arguments but got {}",
                    generic_function_sig.type_params.len(),
                    passed_type_args_count
                );
            }
            let mut evaled_params: SV4<NameAndType> =
                SmallVec::with_capacity(passed_type_args_count);
            let type_args_owned = self.ast.p_type_args.copy_slice_sv::<4>(passed_type_args);
            for (type_param, type_arg) in self
                .named_types
                .copy_slice_sv::<4>(generic_function_sig.type_params)
                .iter()
                .zip(type_args_owned.iter())
            {
                let passed_type = self.eval_type_expr(type_arg.type_expr, ctx.scope_id)?;
                evaled_params.push(NameAndType { name: type_param.name, type_id: passed_type });
            }
            self.named_types.add_slice_from_copy_slice(&evaled_params)
        } else {
            let generic_function_type =
                self.types.get(generic_function_sig.function_type).as_function().unwrap().clone();
            let generic_function_return_type = generic_function_type.return_type;
            let generic_function_params = generic_function_type.logical_params();
            let args_and_params = self.align_call_arguments_with_parameters(
                fn_call,
                generic_function_params,
                known_args.map(|(_known_types, known_args)| known_args),
                ctx.scope_id,
                true,
            )?;
            let mut inference_pairs: SmallVec<[_; 8]> = match ctx.expected_type_id {
                None => SmallVec::with_capacity(args_and_params.len()),
                Some(expected) => {
                    let mut v = SmallVec::with_capacity(args_and_params.len() + 1);
                    // One Inference Pair for the return type
                    v.push(InferenceInputPair {
                        arg: TypeOrParsedExpr::Type(expected),
                        param_type: generic_function_return_type,
                        allow_mismatch: true,
                    });
                    v
                }
            };
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

            // Inference pairs for everything we learn from the constraints

            let (solutions, _all_solutions) = self
                .infer_types(
                    generic_function_sig.type_params,
                    generic_function_sig.type_params,
                    &inference_pairs,
                    fn_call.span,
                    ctx.scope_id,
                )
                .map_err(|e| {
                    errf!(
                        e.span,
                        "Invalid call to {}. {}",
                        self.ident_str_opt(generic_function_sig.name),
                        e.message,
                    )
                })?;
            solutions
        };

        // Enforce ability constraints
        let params_to_solutions_pairs: SV4<TypeSubstitutionPair> = self
            .zip_named_types_to_subst_pairs(generic_function_sig.type_params, solved_type_params);
        for (solution, type_param) in self
            .named_types
            .copy_slice_sv::<4>(solved_type_params)
            .iter()
            .zip(self.named_types.copy_slice_sv::<4>(generic_function_sig.type_params).iter())
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
                .existential_type_params
                .get_slice_to_smallvec::<4>(original_function_sig.function_type_params)
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
                    FunctionReference,
                    LambdaObject(TypeId),
                }
                let physical_passed_function = match corresponding_arg {
                    MaybeTypedExpr::Typed(_) => {
                        unreachable!("Synthesizing calls with function type params is unsupported")
                    }
                    MaybeTypedExpr::Parsed(p) => match self.ast.exprs.get(*p) {
                        ParsedExpr::Lambda(_lam) => {
                            debug!("substituting type for an ftp lambda so that it can infer");
                            let substituted_param_type =
                                self.substitute_in_type(function_type_param.type_id, &subst_pairs);
                            let the_lambda = self.eval_expr(
                                *p,
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
                            let t = self.eval_expr(*p, ctx.with_no_expected_type())?;
                            let type_id = self.exprs.get(t).get_type();
                            match self.types.get(type_id) {
                                Type::Lambda(_) => PhysicalPassedFunction::Lambda(type_id),
                                Type::LambdaObject(_) => {
                                    debug!("Using a LambdaObject as an ftp");
                                    PhysicalPassedFunction::LambdaObject(type_id)
                                }
                                Type::Reference(_) => {
                                    debug!("Using a LambdaObject as an ftp");
                                    PhysicalPassedFunction::FunctionReference
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
                    PhysicalPassedFunction::FunctionReference => {
                        let ftp = self
                            .types
                            .get(function_type_param.type_id)
                            .as_function_type_parameter()
                            .unwrap();
                        let original_param_function_type = ftp.function_type;
                        let substituted_function_type =
                            self.substitute_in_type(original_param_function_type, &subst_pairs);
                        self.types.add_reference_type(substituted_function_type)
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
        let function_type_args_handle =
            self.named_types.add_slice_from_copy_slice(&function_type_args);
        Ok(function_type_args_handle)
    }

    pub(crate) fn determine_static_type_args_for_call(
        &mut self,
        original_function_sig: FunctionSignature,
        args_and_params: &ArgsAndParams,
        ctx: EvalExprContext,
    ) -> TyperResult<NamedTypeSlice> {
        let mut static_type_args: SmallVec<[NameAndType; 8]> = SmallVec::new();
        if !original_function_sig.static_type_params.is_empty() {
            for static_type_param in self
                .existential_type_params
                .get_slice_to_smallvec::<4>(original_function_sig.static_type_params)
                .iter()
            {
                let (corresponding_arg, corresponding_value_param) =
                    args_and_params.get(static_type_param.value_param_index as usize);
                debug!(
                    "The param for static_type_param {} {} is {} and passed: {:?}",
                    static_type_param.type_id,
                    self.ident_str(static_type_param.name),
                    self.ident_str(corresponding_value_param.name),
                    corresponding_arg
                );

                let expr_id = match corresponding_arg {
                    MaybeTypedExpr::Parsed(p) => {
                        self.eval_expr(*p, ctx.with_expected_type(Some(static_type_param.type_id)))?
                    }
                    MaybeTypedExpr::Typed(typed_expr_id) => *typed_expr_id,
                };
                let expr_type_id = self.exprs.get(expr_id).get_type();
                let final_parameter_type = if let Type::Static(_passed_static_type) =
                    self.types.get_no_follow_static(expr_type_id)
                {
                    expr_type_id
                } else if let Ok(lifted) = self.attempt_static_lift(expr_id) {
                    self.exprs.get(lifted).get_type()
                } else {
                    return failf!(
                        self.exprs.get(expr_id).get_span(),
                        "Expected a static value for static type parameter {}, but got {}",
                        self.ident_str(static_type_param.name),
                        self.type_id_to_string(expr_type_id)
                    );
                };
                static_type_args.push(NameAndType {
                    name: static_type_param.name,
                    type_id: final_parameter_type,
                });
            }
        }
        if !static_type_args.is_empty() {
            debug!(
                "We're passing function_type_args! {}",
                self.pretty_print_named_types(&static_type_args, ", ")
            );
        }
        let static_type_args_handle = self.named_types.add_slice_from_copy_slice(&static_type_args);
        Ok(static_type_args_handle)
    }

    fn add_substitution(&self, set: &mut Vec<TypeSubstitutionPair>, pair: TypeSubstitutionPair) {
        debug!(
            "Applying substitution {} -> {} to set {}",
            self.type_id_to_string(pair.from),
            self.type_id_to_string(pair.to),
            self.pretty_print_type_substitutions(set, ", ")
        );
        if pair.from == pair.to {
            return;
        }
        set.iter_mut().for_each(|existing| {
            if existing.from == pair.from {
                existing.from = pair.to
            } else if existing.to == pair.from {
                existing.to = pair.to
            };
        });
        set.retain(|pair| pair.from != pair.to);
        set.push(pair);

        debug!("Got set {}", self.pretty_print_type_substitutions(set, ", "));
    }

    /// Returns: Any newly, fully solved params after applying constraints
    fn calculate_inference_substitutions(
        &mut self,
        span: SpanId,
    ) -> TyperResult<SV4<TypeSubstitutionPair>> {
        let mut ctx = std::mem::take(&mut self.inference_context);
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
                        self.inference_context = ctx;
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
        self.inference_context = ctx;
        Ok(newly_solved_params)
    }

    /// Used for fixing up the constraint signatures:
    /// For each param they mention, if its solved, use the solution, otherwise use the
    /// inference hole so we can learn more about it
    /// This is probably a re-usable concept for inference context, I think its probably quite
    /// often that we want this mapping for each param
    fn make_inference_substitution_set(&self) -> SV8<TypeSubstitutionPair> {
        let mut subst_set: SV8<TypeSubstitutionPair> = smallvec![];
        for (param, inference_hole) in
            self.inference_context.params.iter().zip(self.inference_context.inference_vars.iter())
        {
            if let Some(solution) =
                self.inference_context.solutions_so_far.iter().find(|p| p.from == *param)
            {
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
        let mut inference_substitutions = std::mem::take(&mut self.inference_context.constraints);
        let result = self.unify_and_find_substitutions_rec(
            &mut inference_substitutions,
            passed_type,
            slot_type,
            false,
        );
        self.inference_context.constraints = inference_substitutions;
        result
    }

    pub(crate) fn unify_and_find_substitutions_rec(
        &self,
        substitutions: &mut Vec<TypeSubstitutionPair>,
        passed_type: TypeId,
        slot_type: TypeId,
        // `type_param_enabled`: Whether or not we should look for TypeParameters. By default,
        // we just look for InferenceHoles. But there are some scenarios
        // where its useful to apply this algorithm over type parameters
        type_param_enabled: bool,
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
            "unify_and_find_substitutions passed {} in slot {}",
            self.type_id_to_string(passed_type).blue(),
            self.type_id_to_string(slot_type).blue()
        );
        let counts = self.types.type_variable_counts.get(slot_type);
        if type_param_enabled {
            if counts.type_parameter_count == 0 {
                return TypeUnificationResult::NoHoles;
            }
        } else if counts.inference_variable_count == 0 {
            debug!("no type holes: {}", self.type_id_to_string(slot_type));
            return TypeUnificationResult::NoHoles;
        }

        // This special case is removable, all tests pass, but I believe its currently
        // a slight optimization, and would be more of one with more complex types
        if let (Some(passed_info), Some(arg_info)) = (
            self.types.get_generic_instance_info(passed_type),
            self.types.get_generic_instance_info(slot_type),
        ) {
            // expr: NewList[int] arg: NewList['0]
            if passed_info.generic_parent == arg_info.generic_parent {
                debug!(
                    "comparing generic instances of {}",
                    self.type_id_to_string(arg_info.generic_parent)
                );
                // We can directly 'solve' every appearance of a type param here
                for (passed_type, arg_slot) in
                    passed_info.type_args.iter().zip(arg_info.type_args.iter())
                {
                    self.unify_and_find_substitutions_rec(
                        substitutions,
                        *passed_type,
                        *arg_slot,
                        type_param_enabled,
                    );
                }
            } else {
                debug!("compared generic instances but they didn't match parent types");
            }
            return TypeUnificationResult::Matching;
        }

        match (self.types.get_no_follow(passed_type), self.types.get_no_follow(slot_type)) {
            (Type::InferenceHole(_actual_hole), _expected_type) => {
                // Note: We may eventually need an 'occurs' check to prevent recursive
                // substitutions; for now they don't seem to be occurring though, and it'll
                // be obvious if they ever do
                self.add_substitution(
                    substitutions,
                    TypeSubstitutionPair { from: passed_type, to: slot_type },
                );
                TypeUnificationResult::Matching
            }
            (_actual_type, Type::InferenceHole(_expected_hole)) => {
                // Note: We may eventually need an 'occurs' check to prevent recursive
                // substitutions; for now they don't seem to be occurring though, and it'll
                // be obvious if they ever do
                self.add_substitution(
                    substitutions,
                    TypeSubstitutionPair { from: slot_type, to: passed_type },
                );
                TypeUnificationResult::Matching
            }
            (Type::Reference(passed_refer), Type::Reference(refer)) => self
                .unify_and_find_substitutions_rec(
                    substitutions,
                    passed_refer.inner_type,
                    refer.inner_type,
                    type_param_enabled,
                ),
            (Type::Struct(passed_struct), Type::Struct(struc)) => {
                // Struct example:
                // type Pair<T, U> = { a: T, b: U }
                // fn get_first<T, U>(p: Pair<T, U>): T { p.a }
                // get_first({ a: 1, b: 2})
                // passed_expr: Pair<int, int>, argument_type: Pair<T, U>
                // passed expr: { a: int, b: int }, argument_type: { a: T, b: U }
                //
                // Structs must have all same field names in same order
                let passed_fields = &passed_struct.fields;
                let fields = &struc.fields;
                if passed_fields.len() != fields.len() {
                    return TypeUnificationResult::NonMatching("field count");
                }
                for (idx, field) in fields.iter().enumerate() {
                    let passed_field = &passed_fields[idx];
                    if field.name != passed_field.name {
                        return TypeUnificationResult::NonMatching("field names");
                    }
                    self.unify_and_find_substitutions_rec(
                        substitutions,
                        passed_field.type_id,
                        field.type_id,
                        type_param_enabled,
                    );
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
                let passed_variants = &passed_enum.variants;
                let variants = &param_enum_type.variants;
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
                            self.unify_and_find_substitutions_rec(
                                substitutions,
                                passed_payload,
                                param_payload,
                                type_param_enabled,
                            );
                        } else {
                            return TypeUnificationResult::NonMatching("payloads");
                        }
                    }
                }

                TypeUnificationResult::Matching
            }
            (Type::EnumVariant(passed_enum_variant), Type::Enum(_param_enum_type_variant)) => self
                .unify_and_find_substitutions_rec(
                    substitutions,
                    passed_enum_variant.enum_type_id,
                    slot_type,
                    type_param_enabled,
                ),
            (Type::Array(passed_array), Type::Array(slot_array)) => {
                self.unify_and_find_substitutions_rec(
                    substitutions,
                    passed_array.size_type,
                    slot_array.size_type,
                    type_param_enabled,
                );
                self.unify_and_find_substitutions_rec(
                    substitutions,
                    passed_array.element_type,
                    slot_array.element_type,
                    type_param_enabled,
                );
                TypeUnificationResult::Matching
            }
            (passed, Type::FunctionTypeParameter(slot_function_type_param)) => {
                if let Some(passed_function_type) =
                    self.extract_function_type_from_functionlike(passed)
                {
                    self.unify_and_find_substitutions_rec(
                        substitutions,
                        passed_function_type,
                        slot_function_type_param.function_type,
                        type_param_enabled,
                    )
                } else {
                    TypeUnificationResult::NonMatching(
                        "Expected a function type parameter; passed unrelated",
                    )
                }
            }
            (Type::Function(passed_fn), Type::Function(param_fn)) => {
                if passed_fn.logical_params().len() == param_fn.logical_params().len() {
                    for (passed_param, param_param) in
                        passed_fn.logical_params().iter().zip(param_fn.logical_params().iter())
                    {
                        self.unify_and_find_substitutions_rec(
                            substitutions,
                            passed_param.type_id,
                            param_param.type_id,
                            type_param_enabled,
                        );
                    }
                    self.unify_and_find_substitutions_rec(
                        substitutions,
                        passed_fn.return_type,
                        param_fn.return_type,
                        type_param_enabled,
                    )
                } else {
                    TypeUnificationResult::NonMatching(
                        "Functions take a different number of arguments",
                    )
                }
            }
            (Type::Lambda(passed_lambda), Type::LambdaObject(param_lambda)) => self
                .unify_and_find_substitutions_rec(
                    substitutions,
                    passed_lambda.function_type,
                    param_lambda.function_type,
                    type_param_enabled,
                ),
            (Type::LambdaObject(passed_lambda), Type::LambdaObject(param_lambda)) => self
                .unify_and_find_substitutions_rec(
                    substitutions,
                    passed_lambda.function_type,
                    param_lambda.function_type,
                    type_param_enabled,
                ),
            (Type::Static(passed_static), Type::Static(param_static)) => self
                .unify_and_find_substitutions_rec(
                    substitutions,
                    passed_static.inner_type_id,
                    param_static.inner_type_id,
                    type_param_enabled,
                ),
            (Type::Static(static_type), _) => self.unify_and_find_substitutions_rec(
                substitutions,
                static_type.inner_type_id,
                slot_type,
                type_param_enabled,
            ),
            (Type::TypeParameter(_actual_param), _expected_type) if type_param_enabled => {
                // Note: We may eventually need an 'occurs' check to prevent recursive
                // substitutions; for now they don't seem to be occurring though, and it'll
                // be obvious if they ever do
                self.add_substitution(
                    substitutions,
                    TypeSubstitutionPair { from: passed_type, to: slot_type },
                );
                TypeUnificationResult::Matching
            }
            (_actual_type, Type::TypeParameter(_expected_param)) if type_param_enabled => {
                // Note: We may eventually need an 'occurs' check to prevent recursive
                // substitutions; for now they don't seem to be occurring though, and it'll
                // be obvious if they ever do
                self.add_substitution(
                    substitutions,
                    TypeSubstitutionPair { from: slot_type, to: passed_type },
                );
                TypeUnificationResult::Matching
            }
            _ if passed_type == slot_type => TypeUnificationResult::Matching,
            _ => TypeUnificationResult::NonMatching("Unrelated types"),
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
