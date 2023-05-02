#' DerivationExtractor
#'
#' Construct an object for extracting the derivations within a
#' \code{\link{Model}}.
#'
#' @param model Object of class \code{\link{Model}}
#'
#' @return An object of class \code{DerivationExtractor} with the
#' following methods.
#'
#' ## Methods
#'
#' * `$expand_derivation(derivation)` -- Expand a single derivation by name.
#' * `$expand_derivations()` -- Example all derivations in the model.
#'
#' @export
DerivationExtractor = function(model){
  self = Base()
  self$model = model

  # @param derivation an element in a derivations file
  # @return variables required for the derivation, with all partitions
  self$.filtered_variables = function(derivation){
    if (!is.null(derivation$filter_partition)) {
      filtered_variables = self$model$variables$all()$filter(
        derivation$filter_names,
        .wrt = derivation$filter_partition
      )
    } else filtered_variables = self$model$variables$all()
    return(filtered_variables)
  }

  # @param derivation an element in a derivations file
  # @return list of variables required for each group in the derivation,
  #         with all partitions
  self$.group_variables = function(derivation){
    if (!is.null(derivation$group_partition)) {
      group_variables = lapply(derivation$group_names
        , self$.filtered_variables(derivation)$filter
        , .wrt = derivation$group_partition
        , .comparison_function = all_consistent
      )
    }
    else {
      group_variables = list(self$.filtered_variables(derivation))
    }
    return(group_variables)
  }

  # @param derivation an element in a derivations file
  # @return list of labels for the outputs of each group, using only the
  #         required partitions
  self$.group_outputs = function(derivation){
    rp = self$model$settings$names()
    if (!is.null(derivation$output_partition)) {
      group_output = lapply(derivation$output_names
        , self$.filtered_variables(derivation)$filter
        , .wrt = derivation$output_partition
      )
    }
    else {
      group_output = lapply(derivation$output_names
        , self$.filtered_variables(derivation)$filter
        , .wrt = rp
      )
    }
    group_output = (group_output
      |> method_apply("select", rp)
      |> method_apply("labels")
    )
    return(group_output)
  }

  self$.group_inputs = function(derivation){
    if (!is.null(derivation$input_partition)) {
      return(derivation$input_partition)
    }
    return(self$model$settings$names())
  }

  self$.number_of_groups = function(derivation){
    only_one_grp = (
        is.null(derivation$output_partition)
      | is.null(derivation$group_names)
    )
    if (only_one_grp) return(1)
    return(length(derivation$group_names))
  }

  self$.filtered_group_variables = function(derivation){
    filtered_group_variables = list()
    if (!is.null(derivation$arguments)) {
      rp = self$model$settings$names()
      for (j in 1:self$.n_grps) {
        unordered_group_variables = self$.grp_vars[[j]]$filter(
          derivation$arguments,
          .wrt = self$.grp_inpt
        )
        ordered_group_variables = unordered_group_variables$filter_ordered(
          derivation$arguments,
          .wrt = self$.grp_inpt
        )
        filtered_group_variables = c(
          filtered_group_variables,
          list(as.list(ordered_group_variables$select(rp)$labels()))
        )
      }
    }
    return(filtered_group_variables)
  }

  self$.filtered_group_variable_dots = function(derivation){
    filtered_group_variable_dots = list()
    if (!is.null(derivation$argument_dots)) {
      rp = self$model$settings$required_partitions
      for (j in 1:self$.n_grps) {
        filtered_group_variable_dots = c(
          filtered_group_variable_dots,
          list(as.list(self$.grp_vars[[j]]$filter(
            derivation$argument_dots,
            .wrt = self$.grp_inpt
          )$select(rp)$labels())))
      }
    }
    return(filtered_group_variable_dots)
  }

  self$extract_derivation = function(derivation){
    self$.grp_vars = self$.group_variables(derivation)
    self$.grp_inpt = self$.group_inputs(derivation)
    self$.n_grps = self$.number_of_groups(derivation)
    return(list(
      simulation_phase = derivation$simulation_phase,
      expression = derivation$expression,
      arguments = derivation$arguments,
      outputs = self$.group_outputs(derivation),
      variables = self$.filtered_group_variables(derivation),
      variable_dots = self$.filtered_group_variable_dots(derivation)
    ))
  }

  self$extract_derivations = function(){
    derivation_list = self$model$derivations()
    return(lapply(derivation_list, self$extract_derivation))
  }

  return_object(self, "DerivationExtractor")
}


#' Scalar2Vector
#'
#' Construct an object for replacing scalar names within a \code{\link{Model}}
#' model, with the equivalent vector name.
#'
#' @param derivation_extractor Object of type \code{\link{DerivationExtractor}}
#'
#' ## Methods
#'
#' * `$vectorizer(expanded_derivation)`
#' * `$vectorize()`
#'
#' @export
Scalar2Vector = function(derivation_extractor){
  self = Base()
  self$model = derivation_extractor$model
  self$extracted_derivations = derivation_extractor$extract_derivations()
  self$.state_pointer = function(scalar_name){
    return(as.numeric(which(scalar_name == self$model$def$settings()[["state_variables"]])) - 1)
  }
  self$.state_replacer = function(scalar_name){
    return(paste0("state[", paste0(self$.state_pointer(scalar_name), "]")))
  }
  self$.flow_pointer = function(scalar_name){
    return(as.numeric(which(scalar_name == self$model$def$settings()[["flow_variables"]])) - 1)
  }
  self$.flow_replacer = function(scalar_name){
    return(paste0("flow[", paste0(self$.flow_pointer(scalar_name), "]")))
  }
  self$.replacer = function(scalar_name){
    if (any(scalar_name == self$model$def$settings()[["state_variables"]])) {
      return(self$.state_replacer(scalar_name))
    }
    else if (any(scalar_name == self$model$def$settings()[["flow_variables"]])) {
      return(self$.flow_replacer(scalar_name))
    }
    else return(scalar_name)
  }
  self$.list_replacer = function(scalar_name_list){
    return(lapply(scalar_name_list, self$.replacer))
  }
  self$.output_revisor = function(extracted_derivation){
    old_derivation = list(
      simulation_phase = extracted_derivation$simulation_phase,
      expression = extracted_derivation$expression,
      arguments = extracted_derivation$arguments,
      outputs = list(),
      variables = list(),
      variable_dots = list()
    )
    new_derivation = list(
      simulation_phase = extracted_derivation$simulation_phase,
      outputs = list(),
      variables = list(),
      variable_dots = list()
    )
    new_derivation$arguments = c(
      "vect_name", "vect_index",
      extracted_derivation$arguments
    )
    new_derivation$expression = paste0("assign(vect_name, vect_index, 0, ", paste0(extracted_derivation$expression, ")"))
    s = self$model$def$settings()
    for (i in 1:length(extracted_derivation$outputs)) {
      if (any(extracted_derivation$outputs[[i]] == s[["state_variables"]])) {
        new_derivation$outputs = c(new_derivation$outputs, "dummy")
        if (length(extracted_derivation$variables) != 0L) {
          new_derivation$variables = c(
            new_derivation$variables,
            list(
              c(
                "state",
                self$.state_pointer(extracted_derivation$outputs[[i]]),
                extracted_derivation$variables[[i]])
              )
            )
        }
        if (length(extracted_derivation$variable_dots) != 0) new_derivation$variable_dots = c(new_derivation$variable_dots, list(extracted_derivation$variable_dots[[i]]))
      }
      else if (any(extracted_derivation$outputs[[i]] == s[["flow_variables"]])) {
        new_derivation$outputs = c(new_derivation$outputs, "dummy")
        if (length(extracted_derivation$variables) != 0L) new_derivation$variables = c(new_derivation$variables, list(c("flow", self$.flow_pointer(extracted_derivation$outputs[[i]]), extracted_derivation$variables[[i]])))
        if (length(extracted_derivation$variable_dots) != 0) new_derivation$variable_dots = c(new_derivation$variable_dots, list(extracted_derivation$variable_dots[[i]]))
      }
      else {
        old_derivation$outputs = c(old_derivation$outputs, extracted_derivation$outputs[[i]])
        if (length(extracted_derivation$variables) != 0L) old_derivation$variables = c(old_derivation$variables, list(extracted_derivation$variables[[i]]))
        if (length(extracted_derivation$variable_dots) != 0) old_derivation$variable_dots = c(old_derivation$variable_dots, list(extracted_derivation$variable_dots[[i]]))
      }
    }
    if ((length(new_derivation$outputs) != 0L) & length(old_derivation$outputs) != 0L) revised_derivations = c(new_derivation, old_derivation)
    else if (length(new_derivation$outputs) != 0L) revised_derivations = new_derivation
    else revised_derivations = old_derivation
    return(revised_derivations)
  }
  self$vectorizer = function(extracted_derivation){
    extracted_derivation$variables = lapply(extracted_derivation$variables, self$.list_replacer)
    extracted_derivation$variable_dots = lapply(extracted_derivation$variable_dots, self$.list_replacer)
    extracted_derivation = self$.output_revisor(extracted_derivation)
    return(extracted_derivation)
  }
  self$vectorize = function(){
    return(lapply(self$extracted_derivations, self$vectorizer))
  }
  return_object(self, "Scalar2Vector")
}


#' UserExpr
#'
#' Evaluate user input expressions
#'
#' @param model Object created by \code{\link{Model}}.
#'
#' @export
UserExpr = function(model){
  self = Base()
  self$model = model
  self$derivation_extractor = DerivationExtractor(self$model)
  self$vectorized_derivations = Scalar2Vector(self$derivation_extractor)$vectorize()
  self$scalarized_derivations = self$derivation_extractor$extract_derivations()
  self$.vars_check = function(extracted_derivation){
    return(!is.null(extracted_derivation$variables) & !(length(extracted_derivation$variables) == 0L))
  }
  self$.dots_check = function(extracted_derivation){
    return(!is.null(extracted_derivation$variable_dots) & !(length(extracted_derivation$variable_dots) == 0L))
  }
  # self$.vect_check = function(output){
  #
  # }
  self$.make_expression = function(extracted_derivation){
    if (self$.vars_check(extracted_derivation) & self$.dots_check(extracted_derivation)) {
      return(MathExpressionFromStrings(extracted_derivation$expression, extracted_derivation$arguments, include_dots = TRUE))
    }
    else if (self$.vars_check(extracted_derivation)) {
      return(MathExpressionFromStrings(extracted_derivation$expression, extracted_derivation$arguments, include_dots = FALSE))
    }
    else if (self$.dots_check(extracted_derivation)) {
      return(MathExpressionFromStrings(extracted_derivation$expression, include_dots = TRUE))
    }
    else stop("Derivations file appears invalid, no arguments or argument dots.")
  }
  self$.argument_collector = function(extracted_derivation) {
    if (self$.vars_check(extracted_derivation) & self$.dots_check(extracted_derivation)) {
      args = mapply(c, extracted_derivation$variables, extracted_derivation$variable_dots, SIMPLIFY = FALSE)
    }
    else if (self$.vars_check(extracted_derivation)) {
      args = extracted_derivation$variables
    }
    else if (self$.dots_check(extracted_derivation)) {
      args = extracted_derivation$variable_dots
    }
    else stop("Derivations file appears invalid, no arguments or argument dots.")
    return(args)
  }
  self$.evaluate_expression = function(extracted_derivation) {
    formula = self$.make_expression(extracted_derivation)
    evaluator = function(args) return(do.call(formula$symbolic$evaluate, args))
    evaluated_expressions = lapply(self$.argument_collector(extracted_derivation), evaluator)
    return(evaluated_expressions)
  }
  self$.format_expression = function(extracted_derivation) {
    expressions = self$.evaluate_expression(extracted_derivation)
    arguments = self$.argument_collector(extracted_derivation) #This is inefficient as the same method is already called as part of self$.evaluate_expression
    outputs = extracted_derivation$outputs
    sim_phases = rep(extracted_derivation$simulation_phase, length(outputs))
    return(mapply(list, output_names = outputs, expression = expressions, arguments = arguments, simulation_phase = sim_phases, SIMPLIFY = FALSE))
  }
  self$expand_scalar_expressions = function() {
    return(do.call(c, lapply(self$scalarized_derivations, self$.format_expression)))
  }
  self$expand_vector_expressions = function() {
    return(do.call(c, lapply(self$vectorized_derivations, self$.format_expression)))
  }
  return_object(self, "UserExpr")
}


Indices = function(model) {
  self = Base()
  self$model = model

  # C++ uses zero-based indexing
  self$.match_zero_based = function(...) match(...) - 1L

  self$indices = function(scalar_name, variable_collection_name) {
    valid$char1$check(variable_collection_name)
    variable_collection = self$model$def$settings()[[variable_collection_name]]
    required_collections = sprintf("%s_variables", c("state", "flow"))
    if (!is.null(variable_collection)) {
      indices = self$.match_zero_based(
        valid$char1$assert(scalar_name),
        variable_collection
      )
    } else if (variable_collection_name %in% required_collections) {
      stop("\nthe"
        , variable_collection_name
        , "\nfield is missing from the settings.json file"
        , "\nin this model."
      )
    } else {
      indices = integer()
    }
    return(indices)
  }
  return_object(self, "Indices")
}


#' StandardExpr
#'
#' Evaluate standard model expressions
#'
#' @param model Object created by \code{\link{Model}}.
#'
#' @export
StandardExpr = function(model){
  self = Indices(model)
  self$.expanded_flows = self$model$flows_expanded()
  self$.state_length = length(self$model$settings$state())
  self$.flow_types = list("per_capita",
                          "absolute",
                          "per_capita_inflow",
                          "per_capita_outflow",
                          "absolute_inflow",
                          "absolute_outflow")
  self$.inflow_flow_types = list("per_capita",
                                 "absolute",
                                 "per-capita_inflow",
                                 "absolute_inflow")
  self$.outflow_flow_types = list("per_capita",
                                  "absolute",
                                  "per_capita_outflow",
                                  "absolute_outflow")
  self$.flow_seperator = function(flow_type){
    return(self$.expanded_flows[self$.expanded_flows$type == flow_type,])
  }
  self$.seperated_flows = lapply(self$.flow_types, self$.flow_seperator)
  self$.init_index_vector = function(flow_frame){
    from = lapply(flow_frame$from, self$indices, "state_variables")
    to = lapply(flow_frame$to, self$indices, "state_variables")
    flow = lapply(flow_frame$flow, self$indices, "flow_variables")
    return(list(from = from, to = to, flow = flow))
  }
  self$.init_index_vectors = function(){
    output_list = lapply(self$.seperated_flows, self$.init_index_vector)
    names(output_list) = self$.flow_types
    return(output_list)
  }
  self$.flow_tester = function(flow_type) {
    valid$char1$assert(flow_type) %in% self$.expanded_flows$type
  }

  # Return a list of derivations, each of which describes a
  # standard expression in the order in which they must appear
  # in the model.
  #
  # Each element in the list contains the following fields:
  #   - output_names
  #   - expression
  #   - arguments
  #   - simulation_phase
  # See the spec document on model definition files for what these mean.
  self$.init_derivations_list = function(){
    present_flows = unlist(lapply(self$.flow_types, self$.flow_tester), use.names = FALSE)
    present_inflows = unlist(lapply(self$.inflow_flow_types, self$.flow_tester), use.names = FALSE)
    present_outflows = unlist(lapply(self$.outflow_flow_types, self$.flow_tester), use.names = FALSE)

    total_inflow_expression_vct = c(
      "groupSums(per_capita, per_capita_to, state_length)",
      "groupSums(absolute, absolute_to, state_length)",
      "groupSums(per_capita_outflow, per_capita_outflow_from, state_length)",
      "groupSums(absolute_inflow, absolute_inflow_to, state_length)"
    )
    total_inflow_argument_list = list(
      "per_capita", "per_capita_to", "absolute", "absolute_to", "per_capita_inflow",
      "per_capita_inflow_to", "absolute_inflow", "absolute_inflow_to", "state_length"
    )

    total_outflow_expression_vct = c(
      "groupSums(per_capita, per_capita_from, state_length)",
      "groupSums(absolute, absolute_from, state_length)",
      "groupSums(per_capita_outflow, per_capita_outflow_from, state_length)",
      "groupSums(absolute_outflow, absolute_outflow_from, state_length)"
    )
    total_outflow_argument_list = list(
      "per_capita", "per_capita_from", "absolute", "absolute_from", "per_capita_outflow",
      "per_capita_outflow_from", "absolute_outflow", "absolute_outflow_from", "state_length"
    )

    per_capita_list = list(
      output_names = "per_capita",
      expression = "state[per_capita_from]*flow[per_capita_flow]",
      arguments = list("state", "per_capita_from", "flow", "per_capita_flow"),
      simulation_phase = "during_update"
    )
    absolute_list = list(
      output_names = "absolute",
      expression = "flow[absolute_flow]",
      arguments = list("flow", "absolute_flow"),
      simulation_phase = "during_update"
    )
    per_capita_inflow_list = list(
      output_names = "per_capita_inflow",
      expression = "state[per_capita_inflow_from]*flow[per_capita_inflow_flow]",
      arguments = list("state", "per_capita_inflow_from", "flow", "per_capita_inflow_flow"),
      simulation_phase = "during_update"
    )
    per_capita_outflow_list = list(
      output_names = "per_capita_outflow",
      expression = "state[per_capita_outflow_from]*flow[per_capita_outflow_flow]",
      arguments = list("state", "per_capita_outflow_from", "flow", "per_capita_outflow_flow"),
      simulation_phase = "during_update"
    )
    absolute_inflow_list = list(
      output_names = "absolute_inflow",
      expression = "flow[absolute_inflow_flow]",
      arguments = list("flow", "absolute_inflow_flow"),
      simulation_phase = "duing_update"
    )
    absolute_outflow_list = list(
      output_names = "absolute_outflow",
      expression = "flow[absolute_outflow_flow]",
      arguments = list("flow", "absolute_outflow_flow"),
      simulation_phase = "during_update"
    )
    total_inflow_list = list(
      output_names = "total_inflow",
      expression = paste0(total_inflow_expression_vct[present_inflows], collapse = "+") ,
      arguments = total_inflow_argument_list[c(rep(present_inflows, each = 2), TRUE)] ,
      simulation_phase = "during_update"
    )
    total_outflow_list = list(
      output_names = "total_outflow",
      expression = paste0(total_outflow_expression_vct[present_outflows], collapse = "+"),
      arguments = total_outflow_argument_list[c(rep(present_outflows, each = 2), TRUE)] ,
      simulation_phase = "during_update"
    )
    state_list = list(
      output_names = "state",
      expression = "state - total_outflow + total_inflow",
      arguments = list("state", "total_outflow", "total_inflow"),
      simulation_phase = "during_update"
    )

    optional_derivations = list(
      per_capita_list, absolute_list, per_capita_inflow_list,
      per_capita_outflow_list, absolute_inflow_list, absolute_outflow_list
    )
    required_derivations = list(total_inflow_list, total_outflow_list, state_list)

    c(optional_derivations[present_flows], required_derivations)
  }
  self$.index_vector_evaluator = function(prefix_string, index_vector, formula){
    return(list(
      list(output_names = paste0(prefix_string, "_from"), expression = do.call(formula$symbolic$evaluate, index_vector$from), arguments = index_vector$from, simulation_phase = "before"),
      list(output_names = paste0(prefix_string, "_to"), expression = do.call(formula$symbolic$evaluate, index_vector$to), arguments = index_vector$to, simulation_phase = "before"),
      list(output_names = paste0(prefix_string, "_flow"), expression = do.call(formula$symbolic$evaluate, index_vector$flow), arguments = index_vector$flow, simulation_phase = "before")
    ))
  }
  self$.index_subvector_evaluator = function(subvector_name, vector_name, formula) {
    l = list()
    labels = self$model$labels
    subvector = labels[[valid$char1$assert(subvector_name)]]()
    vector = labels[[valid$char1$assert(vector_name)]]()
    if (!any(is.null(subvector), is.null(vector))) {
      indices = self$.match_zero_based(subvector, vector)
      l = append(l, list(
        output_names = subvector_name,
        expression = do.call(formula$symbolic$evaluate, as.list(indices)),
        arguments = as.list(indices),
        simulation_phase = "before"
      ))
    }
    return(l)
  }
  self$.index_vectors_evaluator = function(){
    prefix_strings = unique(self$.expanded_flows$type) # self$.flow_types
    #prefix_strings = self$.flow_types
    index_vectors = self$.init_index_vectors()[prefix_strings]
    formula = MathExpressionFromStrings("c(...)", include_dots = TRUE)
    out_args = mapply(self$.index_vector_evaluator
      , prefix_strings
      , index_vectors
      , MoreArgs = list(formula = formula)
      , SIMPLIFY = FALSE
    )
    state_subvector_names = c("infectious_state")
    flow_subvector_names = c("infection_flow")
    out_args = append(out_args, lapply(
      state_subvector_names,
      self$.index_subvector_evaluator,
      "state",
      formula
    ))
    out_args = append(out_args, lapply(
      flow_subvector_names,
      self$.index_subvector_evaluator,
      "flow",
      formula
    ))
    return(do.call(c, out_args))
  }
  self$.derivation_evaluator = function(derivation){
    formula = MathExpressionFromStrings(derivation$expression, derivation$arguments)
    derivation$expression = do.call(formula$symbolic$evaluate, derivation$arguments)
    return(derivation)
  }
  self$.derivations_evaluator = function(){
    return(lapply(self$.init_derivations_list(), self$.derivation_evaluator))
  }
  self$standard_expressions = function(){
    return(self$.derivations_evaluator())
    return(c(self$.index_vectors_evaluator(), self$.derivations_evaluator()))
  }
  return_object(self, "StandardExpr")
}

#' Convert Derivation Lists to Expression Lists
#'
#' @param user_expr \code{\link{UserExpr}} object.
#' @param standard_expr \code{\link{StandardExpr}} object.
#'
#' @return Object of class \code{Derivations2ExprList} with the following
#' methods.
#'
#' ## Methods
#'
#' * `$expr_list()` -- An alternate constructor of \code{\link{ExprList}}
#' objects from a set of derivations.
#'
#' ## Arguments
#'
#' * `.simulate_exprs` -- See the argument of the same name in
#' \code{\link{ExprList}}.
#'
#' @export
Derivations2ExprList = function(user_expr, standard_expr) {
  self = Base()
  self$.user_expr_list = user_expr$expand_vector_expressions()
  self$.standard_expr_list = standard_expr$standard_expressions()

  self$.expression_formatter = function(expression_list_element){
    as.formula(
      paste(
        expression_list_element$output_names,
        expression_list_element$expression,
        sep = " ~ "
      )
    )
  }

  self$.expression_phase_sorter = function(
    phase = c(
      "before",
      "during_pre_update", "during_update", "during_post_update",
      "after")
    ) {

    phase = match.arg(phase)
    user_phases = vapply(
      self$.user_expr_list,
      getElement,
      character(1L),
      "simulation_phase"
    )
    standard_phases = vapply(
      self$.standard_expr_list,
      getElement,
      character(1L),
      "simulation_phase"
    )
    c(
      self$.user_expr_list[user_phases == phase],
      self$.standard_expr_list[standard_phases == phase]
    )
  }
  self$.expr_list_per_phase = function(
      phase = c("before", "during", "after")
    ) {
    #browser()
    phases = match.arg(phase)
    if (phases == "during") {
      phases = c("during_pre_update", "during_update", "during_post_update")
    }

    l = list()
    for (phase in phases) {
      l[[phase]] = lapply(
        self$.expression_phase_sorter(phase),
        self$.expression_formatter
      )
    }
    do.call(c, l)
  }
  self$expr_list = function(.simulate_exprs = character(0L)) {
    ExprList(
      before = self$.expr_list_per_phase("before"),
      during = self$.expr_list_per_phase("during"),
      after = self$.expr_list_per_phase("after"),
      .simulate_exprs = .simulate_exprs
    )
  }
  return_object(self, "Derivations2ExprList")
}
