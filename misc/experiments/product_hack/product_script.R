library(macpan2)
library(oor)

StandardExprAlt = function(model){
  self = macpan2:::Indices(model)
  self$.init_derivations_list = function(){
    #This expression is only required to ensure every model has at least one expression.
    #There is currently a bug where it is not possible to create a simulator for a model with no expressions at all
    hack_expression_list = list(
      output_names = "state",
      expression = "1*state",
      arguments = list("state"),
      simulation_phase = "before"
    )
    
    required_derivations = list(hack_expression_list)
    
    return(required_derivations)
  }
  self$.derivation_evaluator = function(derivation){
    formula = MathExpressionFromStrings(derivation$expression, derivation$arguments)
    derivation$expression = do.call(formula$symbolic$evaluate, derivation$arguments)
    return(derivation)
  }
  self$.derivations_evaluator = function(){
    return(lapply(self$.init_derivations_list(), self$.derivation_evaluator))
  }
  self$standard_expressions = self$.derivations_evaluator
  self$as_derivations = function() lapply(self$standard_expressions(), lapply, as.character)
  return_object(self, "StandardExpr")
}

Derivations2ExprListAlt = function(user_expr, standard_expr) {
  self = Base()
  self$standard_expr = standard_expr
  self$user_expr = user_expr
  self$.standard_expr_list = standard_expr$standard_expressions()
  self$.user_expr_list = user_expr$expand_vector_expressions()
  
  self$.expression_formatter = function(expression_list_element){
    macpan2:::two_sided(
      expression_list_element$output_names,
      expression_list_element$expression
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
  self$expr_list_per_phase = function(
    phase = c("before", "during", "after", "during_pre_update", "during_update", "during_post_update")
  ) {
    #browser()
    phases = match.arg(phase)
    if (phases == "during") {
      phases = c("during_pre_update", "during_update", "during_post_update")
    }
    
    l = list()
    for (phase in phases) {
      l = append(l, lapply(
        self$.expression_phase_sorter(phase),
        self$.expression_formatter
      ))
    }
    return(l)
  }
  self$expr_list = function(.simulate_exprs = character(0L)) {
    ExprList(
      before = self$expr_list_per_phase("before"),
      during = self$expr_list_per_phase("during"),
      after = self$expr_list_per_phase("after"),
      .simulate_exprs = .simulate_exprs
    )
  }
  self$math_expr_list = function(
    phase = c("before", "during", "after", "during_pre_update", "during_update", "during_post_update")
  ) {
    
  }
  return_object(self, "Derivations2ExprList")
}

ModelAlt = function(definition) {
  # Inheritance
  self = oor:::Base()
  
  # Args / Composition
  self$def = definition ## ModelFiles object
  
  # Compositions
  self$settings = macpan2:::Settings(self)
  self$variables = macpan2:::Variables(self)
  self$labels = macpan2:::VariableLabels(self$variables)
  self$indices = macpan2:::VariableIndices(self$labels)
  
  # Standard Methods
  self$flows = function() self$def$flows()
  self$flows_expanded = function() {
    expander = FlowExpander(self$def)
    expander$expand_flows()
  }
  self$flows_explicit = function() {
    optional_fields = c("from_partition", "to_partition", "flow_partition",
                        "from_to_partition", "from_flow_partition", "to_flow_partition")
    
    required_partition = self$settings$name()
    null_partition = self$settings$null()
    
    default_entries = data.frame(required_partition, required_partition, required_partition, "", "", null_partition)
    names(default_entries) = optional_fields
    
    default_entries = do.call("rbind", replicate(nrow(self$flows()), default_entries, simplify = FALSE))
    
    is_missing = function(field_name){
      return(!any(names(self$flows()) == field_name))
    }
    missing_fields = which(lapply(optional_fields, is_missing) == TRUE)
    return(cbind(self$flows(), default_entries[,missing_fields]))
  }
  self$derivations = self$def$derivations  ## look like a field but actually method forwarding
  self$expr_list = function() {
    Derivations2ExprListAlt(UserExpr(self), StandardExprAlt(self))$expr_list()
  }
  
  # Composition
  self$simulators = Simulators(self)
  
  # Set the cache in the underlying ModelFiles object
  # so that when the model definition files change
  # on disk, the caches that depend on these files
  # are invalidated.
  self$def$cache = macpan2:::CacheList(
    self$variables$cache,
    self$labels$cache,
    self$indices$flow ## invalidate method outside of the cache for convenience
  )
  
  # Validate and Return
  (self
    |> macpan2:::assert_variables()
    |> oor:::return_object("Model")
  )
}

CompartmentalAlt = function(model_directory){
  self = ModelAlt(ModelFiles(model_directory))
  return_object(self, "Compartmental")
}

InsertEulerExpressions = function(model_simulator, during_phase_length, expanded_flows){
  flow_types = list("per_capita",
                    "absolute",
                    "per_capita_inflow",
                    "per_capita_outflow",
                    "absolute_inflow",
                    "absolute_outflow")
  inflow_flow_types = list("per_capita",
                           "absolute",
                           "per_capita_inflow",
                           "absolute_inflow")
  outflow_flow_types = list("per_capita",
                            "absolute",
                            "per_capita_outflow",
                            "absolute_outflow")
  flow_tester = function(flow_type) {
    macpan2:::valid$char1$assert(flow_type) %in% expanded_flows$type
  }
  present_flows = unlist(lapply(flow_types, flow_tester), use.names = FALSE)
  present_inflows = unlist(lapply(inflow_flow_types, flow_tester), use.names = FALSE)
  present_outflows = unlist(lapply(outflow_flow_types, flow_tester), use.names = FALSE)
  
  args_per_capita = list(
    as.formula("per_capita ~ state[per_capita_from]*flow[per_capita_flow]"),
    .at = during_phase_length,
    .phase = "during",
    .vec_by_flows = "flows",
    .vec_by_states = "state"
  )
  
  args_absolute = list(
    as.formula("absolute ~ flow[absolute_flow]"),
    .at = during_phase_length,
    .phase = "during",
    .vec_by_flows = "flows",
    .vec_by_states = "state"
  )
  
  args_per_capita_inflow = list(
    as.formula("per_capita_inflow ~ state[per_capita_inflow_from]*flow[per_capita_inflow_flow]"),
    .at = during_phase_length,
    .phase = "during",
    .vec_by_flows = "flows",
    .vec_by_states = "state"
  )
  
  args_per_capita_outflow = list(
    as.formula("per_capita_outflow ~ state[per_capita_outflow_from]*flow[per_capita_outflow_flow]"),
    .at = during_phase_length,
    .phase = "during",
    .vec_by_flows = "flow",
    .vec_by_states = "state"
  )
  
  args_absolute_inflow = list(
    as.formula("absolute_inflow ~ flow[absolute_inflow_flow]"),
    .at = during_phase_length,
    .phase = "during",
    .vec_by_flows = "flows",
    .vec_by_states = "state"
  )
  
  args_absolute_outflow = list(
    as.formula("absolute_outflow ~ flow[absolute_outflow_flow]"),
    .at = during_phase_length ,
    .phase = "during",
    .vec_by_flows = "flows",
    .vec_by_states = "state"
  )

  args_vct = c(args_per_capita, args_absolute, args_per_capita_inflow,
               args_per_capita_outflow, args_absolute_inflow, args_absolute_outflow)
  during_phase_length_increment = 1L
  for(arguments in args_vct[present_flows]){
    arguments$.at = arguments$.at + during_phase_length_increment
    model_simulator = do.call(model_simulator$insert$expressions, arguments)
    during_phase_length_increment = during_phase_length_increment + 1L
  }

  total_outflow_expression_vct = c(
    "groupSums(per_capita, per_capita_from, state_length)",
    "groupSums(absolute, absolute_from, state_length)",
    "groupSums(per_capita_outflow, per_capita_outflow_from, state_length)",
    "groupSums(absolute_outflow, absolute_outflow_from, state_length)"
  )
  
  if(!any(present_outflows)){
    args_total_outflow = list(
      as.formula("total_outflow ~ 0"),
      .at = during_phase_length + during_phase_length_increment +1L,
      .phase = "during",
      .vec_by_flows = "flows",
      .vec_by_states = "state"
    )
  } else {
    args_total_outflow = list(
      as.formula(paste0("total_outflow ~ ", paste0(total_outflow_expression_vct[present_outflows], collapse = "+"))),
      .at = during_phase_length + during_phase_length_increment +1L,
      .phase = "during",
      .vec_by_flows = "flows",
      .vec_by_states = "state"
    )
  }
  model_simulator = do.call(model_simulator$insert$expressions, args_total_outflow)
  during_phase_length_increment = during_phase_length_increment +1L
  
  total_inflow_expression_vct = c(
    "groupSums(per_capita, per_capita_to, state_length)",
    "groupSums(absolute, absolute_to, state_length)",
    "groupSums(per_capita_inflow, per_capita_inflow_to, state_length)",
    "groupSums(absolute_inflow, absolute_inflow_to, state_length)"
  ) 
  
  if(!any(present_inflows)){
    args_total_inflow = list(
      as.formula("total_inflow ~ 0"),
      .at = during_phase_length + during_phase_length_increment +1L,
      .phase = "during",
      .vec_by_flows = "flows",
      .vec_by_states = "state"
    )
  } else {
    args_total_inflow = list(
      as.formula(paste0("total_inflow ~ ", paste0(total_inflow_expression_vct[present_inflows], collapse = "+"))),
      .at = during_phase_length + during_phase_length_increment +1L,
      .phase = "during",
      .vec_by_flows = "flows",
      .vec_by_states = "state"
    )
  }
  model_simulator = do.call(model_simulator$insert$expressions, args_total_inflow)
  during_phase_length_increment = during_phase_length_increment +1L
  
  args_state_update = list(
    as.formula("state ~ state + total_inflow - total_outflow"),
    .at = during_phase_length + during_phase_length_increment +1L,
    .phase = "during",
    .vec_by_flows = "flows",
    .vec_by_states = "state"
  )
  model_simulator = do.call(model_simulator$insert$expressions, args_state_update)
  
  return(model_simulator)
}

# Note that using the RK4 method requires 5 new matrices be created initial_state, state_rk1, state_rk2, state_rk3, state_rk4.
# All should be initialized as empty_matrix.
InsertRK4Expressions = function(model_simulator, during_phase_length, expanded_flows){
  flow_types = list("per_capita",
                    "absolute",
                    "per_capita_inflow",
                    "per_capita_outflow",
                    "absolute_inflow",
                    "absolute_outflow")
  inflow_flow_types = list("per_capita",
                           "absolute",
                           "per_capita_inflow",
                           "absolute_inflow")
  outflow_flow_types = list("per_capita",
                            "absolute",
                            "per_capita_outflow",
                            "absolute_outflow")
  total_inflow_expression_vct = c(
    "groupSums(per_capita, per_capita_to, state_length)",
    "groupSums(absolute, absolute_to, state_length)",
    "groupSums(per_capita_inflow, per_capita_inflow_to, state_length)",
    "groupSums(absolute_inflow, absolute_inflow_to, state_length)"
  ) 
  total_outflow_expression_vct = c(
    "groupSums(per_capita, per_capita_from, state_length)",
    "groupSums(absolute, absolute_from, state_length)",
    "groupSums(per_capita_outflow, per_capita_outflow_from, state_length)",
    "groupSums(absolute_outflow, absolute_outflow_from, state_length)"
  ) 
  
  flow_tester = function(flow_type) {
    macpan2:::valid$char1$assert(flow_type) %in% expanded_flows$type
  }
  present_flows = unlist(lapply(flow_types, flow_tester), use.names = FALSE)
  present_inflows = unlist(lapply(inflow_flow_types, flow_tester), use.names = FALSE)
  present_outflows = unlist(lapply(outflow_flow_types, flow_tester), use.names = FALSE)
  
  during_phase_length_increment = 1L
  
  args_initial_state = list(
    as.formula("initial_state ~ state"),
    .at = during_phase_length + during_phase_length_increment,
    .phase = "during",
    .vec_by_flows = "flows",
    .vec_by_states = "state"
  )
  model_simulator = do.call(model_simulator$insert$expressions, args_initial_state)
  during_phase_length_increment = during_phase_length_increment + 1L
  
  args_per_capita = list(
    as.formula("per_capita ~ state[per_capita_from]*flow[per_capita_flow]"),
    .at = during_phase_length,
    .phase = "during",
    .vec_by_flows = "flows",
    .vec_by_states = "state"
  )
  
  args_absolute = list(
    as.formula("absolute ~ flow[absolute_flow]"),
    .at = during_phase_length,
    .phase = "during",
    .vec_by_flows = "flows",
    .vec_by_states = "state"
  )
  
  args_per_capita_inflow = list(
    as.formula("per_capita_inflow ~ state[per_capita_inflow_from]*flow[per_capita_inflow_flow]"),
    .at = during_phase_length,
    .phase = "during",
    .vec_by_flows = "flows",
    .vec_by_states = "state"
  )
  
  args_per_capita_outflow = list(
    as.formula("per_capita_outflow ~ state[per_capita_outflow_from]*flow[per_capita_outflow_flow]"),
    .at = during_phase_length,
    .phase = "during",
    .vec_by_flows = "flow",
    .vec_by_states = "state"
  )
  
  args_absolute_inflow = list(
    as.formula("absolute_inflow ~ flow[absolute_inflow_flow]"),
    .at = during_phase_length,
    .phase = "during",
    .vec_by_flows = "flows",
    .vec_by_states = "state"
  )
  
  args_absolute_outflow = list(
    as.formula("absolute_outflow ~ flow[absolute_outflow_flow]"),
    .at = during_phase_length ,
    .phase = "during",
    .vec_by_flows = "flows",
    .vec_by_states = "state"
  )
  
  args_vct = c(args_per_capita, args_absolute, args_per_capita_inflow,
               args_per_capita_outflow, args_absolute_inflow, args_absolute_outflow)

  for(arguments in args_vct[present_flows]){
    arguments$.at = arguments$.at + during_phase_length_increment
    model_simulator = do.call(model_simulator$insert$expressions, arguments)
    during_phase_length_increment = during_phase_length_increment + 1L
  }
  
  if(!any(present_outflows)){
    args_total_outflow = list(
      as.formula("total_outflow ~ 0"),
      .at = during_phase_length + during_phase_length_increment +1L,
      .phase = "during",
      .vec_by_flows = "flows",
      .vec_by_states = "state"
    )
  } else {
    args_total_outflow = list(
      as.formula(paste0("total_outflow ~ ", paste0(total_outflow_expression_vct[present_outflows], collapse = "+"))),
      .at = during_phase_length + during_phase_length_increment +1L,
      .phase = "during",
      .vec_by_flows = "flows",
      .vec_by_states = "state"
    )
  }
  model_simulator = do.call(model_simulator$insert$expressions, args_total_outflow)
  during_phase_length_increment = during_phase_length_increment +1L
  
  if(!any(present_inflows)){
    args_total_inflow = list(
      as.formula("total_inflow ~ 0"),
      .at = during_phase_length + during_phase_length_increment +1L,
      .phase = "during",
      .vec_by_flows = "flows",
      .vec_by_states = "state"
    )
  } else {
    args_total_inflow = list(
      as.formula(paste0("total_inflow ~ ", paste0(total_inflow_expression_vct[present_inflows], collapse = "+"))),
      .at = during_phase_length + during_phase_length_increment +1L,
      .phase = "during",
      .vec_by_flows = "flows",
      .vec_by_states = "state"
    )
  }
  model_simulator = do.call(model_simulator$insert$expressions, args_total_inflow)
  during_phase_length_increment = during_phase_length_increment +1L
  
  args_state_rk1_update = list(
    as.formula("state_rk1 ~ initial_state + total_inflow - total_outflow"),
    .at = during_phase_length + during_phase_length_increment + 1L,
    .phase = "during",
    .vec_by_flows = "flows",
    .vec_by_states = "state"
  )
  model_simulator = do.call(model_simulator$insert$expressions, args_state_rk1_update)
  during_phase_length_increment = during_phase_length_increment +1L
  
  ##end of state_rk1 computation
  
  args_tmp_state_update_1 = list(
    as.formula("state ~ initial_state + (state_rk1/2)"),
    .at = during_phase_length + during_phase_length_increment + 1L,
    .phase = "during",
    .vec_by_flows = "flows",
    .vec_by_states = "state"
  )
  model_simulator = do.call(model_simulator$insert$expressions, args_tmp_state_update_1)
  during_phase_length_increment = during_phase_length_increment + 1L
  
  args_per_capita = list(
    as.formula("per_capita ~ state[per_capita_from]*flow[per_capita_flow]"),
    .at = during_phase_length,
    .phase = "during",
    .vec_by_flows = "flows",
    .vec_by_states = "state"
  )
  
  args_absolute = list(
    as.formula("absolute ~ flow[absolute_flow]"),
    .at = during_phase_length,
    .phase = "during",
    .vec_by_flows = "flows",
    .vec_by_states = "state"
  )
  
  args_per_capita_inflow = list(
    as.formula("per_capita_inflow ~ state[per_capita_inflow_from]*flow[per_capita_inflow_flow]"),
    .at = during_phase_length,
    .phase = "during",
    .vec_by_flows = "flows",
    .vec_by_states = "state"
  )
  
  args_per_capita_outflow = list(
    as.formula("per_capita_outflow ~ state[per_capita_outflow_from]*flow[per_capita_outflow_flow]"),
    .at = during_phase_length,
    .phase = "during",
    .vec_by_flows = "flow",
    .vec_by_states = "state"
  )
  
  args_absolute_inflow = list(
    as.formula("absolute_inflow ~ flow[absolute_inflow_flow]"),
    .at = during_phase_length,
    .phase = "during",
    .vec_by_flows = "flows",
    .vec_by_states = "state"
  )
  
  args_absolute_outflow = list(
    as.formula("absolute_outflow ~ flow[absolute_outflow_flow]"),
    .at = during_phase_length ,
    .phase = "during",
    .vec_by_flows = "flows",
    .vec_by_states = "state"
  )
  
  args_vct = c(args_per_capita, args_absolute, args_per_capita_inflow,
               args_per_capita_outflow, args_absolute_inflow, args_absolute_outflow)

  for(arguments in args_vct[present_flows]){
    arguments$.at = arguments$.at + during_phase_length_increment
    model_simulator = do.call(model_simulator$insert$expressions, arguments)
    during_phase_length_increment = during_phase_length_increment + 1L
  }
  
  if(!any(present_outflows)){
    args_total_outflow = list(
      as.formula("total_outflow ~ 0"),
      .at = during_phase_length + during_phase_length_increment +1L,
      .phase = "during",
      .vec_by_flows = "flows",
      .vec_by_states = "state"
    )
  } else {
    args_total_outflow = list(
      as.formula(paste0("total_outflow ~ ", paste0(total_outflow_expression_vct[present_outflows], collapse = "+"))),
      .at = during_phase_length + during_phase_length_increment +1L,
      .phase = "during",
      .vec_by_flows = "flows",
      .vec_by_states = "state"
    )
  }
  model_simulator = do.call(model_simulator$insert$expressions, args_total_outflow)
  during_phase_length_increment = during_phase_length_increment +1L
  
  if(!any(present_inflows)){
    args_total_inflow = list(
      as.formula("total_inflow ~ 0"),
      .at = during_phase_length + during_phase_length_increment +1L,
      .phase = "during",
      .vec_by_flows = "flows",
      .vec_by_states = "state"
    )
  } else {
    args_total_inflow = list(
      as.formula(paste0("total_inflow ~ ", paste0(total_inflow_expression_vct[present_inflows], collapse = "+"))),
      .at = during_phase_length + during_phase_length_increment +1L,
      .phase = "during",
      .vec_by_flows = "flows",
      .vec_by_states = "state"
    )
  }
  model_simulator = do.call(model_simulator$insert$expressions, args_total_inflow)
  during_phase_length_increment = during_phase_length_increment +1L
  
  args_state_rk2_update = list(
    as.formula("state_rk2 ~ initial_state + ((total_inflow - total_outflow)/2)"),
    .at = during_phase_length + during_phase_length_increment + 1L,
    .phase = "during",
    .vec_by_flows = "flows",
    .vec_by_states = "state"
  )
  model_simulator = do.call(model_simulator$insert$expressions, args_state_rk2_update)
  during_phase_length_increment = during_phase_length_increment +1L
  
  ##end of state_rk2 computation
  
  args_tmp_state_update_2 = list(
    as.formula("state ~ initial_state + (state_rk2/2)"),
    .at = during_phase_length + during_phase_length_increment + 1L,
    .phase = "during",
    .vec_by_flows = "flows",
    .vec_by_states = "state"
  )
  model_simulator = do.call(model_simulator$insert$expressions, args_tmp_state_update_2)
  during_phase_length_increment = during_phase_length_increment + 1L
  
  args_per_capita = list(
    as.formula("per_capita ~ state[per_capita_from]*flow[per_capita_flow]"),
    .at = during_phase_length,
    .phase = "during",
    .vec_by_flows = "flows",
    .vec_by_states = "state"
  )
  
  args_absolute = list(
    as.formula("absolute ~ flow[absolute_flow]"),
    .at = during_phase_length,
    .phase = "during",
    .vec_by_flows = "flows",
    .vec_by_states = "state"
  )
  
  args_per_capita_inflow = list(
    as.formula("per_capita_inflow ~ state[per_capita_inflow_from]*flow[per_capita_inflow_flow]"),
    .at = during_phase_length,
    .phase = "during",
    .vec_by_flows = "flows",
    .vec_by_states = "state"
  )
  
  args_per_capita_outflow = list(
    as.formula("per_capita_outflow ~ state[per_capita_outflow_from]*flow[per_capita_outflow_flow]"),
    .at = during_phase_length,
    .phase = "during",
    .vec_by_flows = "flow",
    .vec_by_states = "state"
  )
  
  args_absolute_inflow = list(
    as.formula("absolute_inflow ~ flow[absolute_inflow_flow]"),
    .at = during_phase_length,
    .phase = "during",
    .vec_by_flows = "flows",
    .vec_by_states = "state"
  )
  
  args_absolute_outflow = list(
    as.formula("absolute_outflow ~ flow[absolute_outflow_flow]"),
    .at = during_phase_length ,
    .phase = "during",
    .vec_by_flows = "flows",
    .vec_by_states = "state"
  )
  
  args_vct = c(args_per_capita, args_absolute, args_per_capita_inflow,
               args_per_capita_outflow, args_absolute_inflow, args_absolute_outflow)

  for(arguments in args_vct[present_flows]){
    arguments$.at = arguments$.at + during_phase_length_increment
    model_simulator = do.call(model_simulator$insert$expressions, arguments)
    during_phase_length_increment = during_phase_length_increment + 1L
  }
  
  if(!any(present_outflows)){
    args_total_outflow = list(
      as.formula("total_outflow ~ 0"),
      .at = during_phase_length + during_phase_length_increment +1L,
      .phase = "during",
      .vec_by_flows = "flows",
      .vec_by_states = "state"
    )
  } else {
    args_total_outflow = list(
      as.formula(paste0("total_outflow ~ ", paste0(total_outflow_expression_vct[present_outflows], collapse = "+"))),
      .at = during_phase_length + during_phase_length_increment +1L,
      .phase = "during",
      .vec_by_flows = "flows",
      .vec_by_states = "state"
    )
  }
  model_simulator = do.call(model_simulator$insert$expressions, args_total_outflow)
  during_phase_length_increment = during_phase_length_increment +1L
  
  if(!any(present_inflows)){
    args_total_inflow = list(
      as.formula("total_inflow ~ 0"),
      .at = during_phase_length + during_phase_length_increment +1L,
      .phase = "during",
      .vec_by_flows = "flows",
      .vec_by_states = "state"
    )
  } else {
    args_total_inflow = list(
      as.formula(paste0("total_inflow ~ ", paste0(total_inflow_expression_vct[present_inflows], collapse = "+"))),
      .at = during_phase_length + during_phase_length_increment +1L,
      .phase = "during",
      .vec_by_flows = "flows",
      .vec_by_states = "state"
    )
  }
  model_simulator = do.call(model_simulator$insert$expressions, args_total_inflow)
  during_phase_length_increment = during_phase_length_increment +1L
  
  args_state_rk3_update = list(
    as.formula("state_rk3 ~ initial_state + ((total_inflow - total_outflow)/2)"),
    .at = during_phase_length + during_phase_length_increment + 1L,
    .phase = "during",
    .vec_by_flows = "flows",
    .vec_by_states = "state"
  )
  model_simulator = do.call(model_simulator$insert$expressions, args_state_rk3_update)
  during_phase_length_increment = during_phase_length_increment +1L
  
  ##end of state_rk3 computation
  
  args_tmp_state_update_3 = list(
    as.formula("state ~ initial_state + state_rk3"),
    .at = during_phase_length + during_phase_length_increment + 1L,
    .phase = "during",
    .vec_by_flows = "flows",
    .vec_by_states = "state"
  )
  model_simulator = do.call(model_simulator$insert$expressions, args_tmp_state_update_3)
  during_phase_length_increment = during_phase_length_increment + 1L
  
  args_per_capita = list(
    as.formula("per_capita ~ state[per_capita_from]*flow[per_capita_flow]"),
    .at = during_phase_length,
    .phase = "during",
    .vec_by_flows = "flows",
    .vec_by_states = "state"
  )
  
  args_absolute = list(
    as.formula("absolute ~ flow[absolute_flow]"),
    .at = during_phase_length,
    .phase = "during",
    .vec_by_flows = "flows",
    .vec_by_states = "state"
  )
  
  args_per_capita_inflow = list(
    as.formula("per_capita_inflow ~ state[per_capita_inflow_from]*flow[per_capita_inflow_flow]"),
    .at = during_phase_length,
    .phase = "during",
    .vec_by_flows = "flows",
    .vec_by_states = "state"
  )
  
  args_per_capita_outflow = list(
    as.formula("per_capita_outflow ~ state[per_capita_outflow_from]*flow[per_capita_outflow_flow]"),
    .at = during_phase_length,
    .phase = "during",
    .vec_by_flows = "flow",
    .vec_by_states = "state"
  )
  
  args_absolute_inflow = list(
    as.formula("absolute_inflow ~ flow[absolute_inflow_flow]"),
    .at = during_phase_length,
    .phase = "during",
    .vec_by_flows = "flows",
    .vec_by_states = "state"
  )
  
  args_absolute_outflow = list(
    as.formula("absolute_outflow ~ flow[absolute_outflow_flow]"),
    .at = during_phase_length ,
    .phase = "during",
    .vec_by_flows = "flows",
    .vec_by_states = "state"
  )
  
  args_vct = c(args_per_capita, args_absolute, args_per_capita_inflow,
               args_per_capita_outflow, args_absolute_inflow, args_absolute_outflow)

  for(arguments in args_vct[present_flows]){
    arguments$.at = arguments$.at + during_phase_length_increment
    model_simulator = do.call(model_simulator$insert$expressions, arguments)
    during_phase_length_increment = during_phase_length_increment + 1L
  }
  
  if(!any(present_outflows)){
    args_total_outflow = list(
      as.formula("total_outflow ~ 0"),
      .at = during_phase_length + during_phase_length_increment +1L,
      .phase = "during",
      .vec_by_flows = "flows",
      .vec_by_states = "state"
    )
  } else {
    args_total_outflow = list(
      as.formula(paste0("total_outflow ~ ", paste0(total_outflow_expression_vct[present_outflows], collapse = "+"))),
      .at = during_phase_length + during_phase_length_increment +1L,
      .phase = "during",
      .vec_by_flows = "flows",
      .vec_by_states = "state"
    )
  }
  model_simulator = do.call(model_simulator$insert$expressions, args_total_outflow)
  during_phase_length_increment = during_phase_length_increment +1L
  
  if(!any(present_inflows)){
    args_total_inflow = list(
      as.formula("total_inflow ~ 0"),
      .at = during_phase_length + during_phase_length_increment +1L,
      .phase = "during",
      .vec_by_flows = "flows",
      .vec_by_states = "state"
    )
  } else {
    args_total_inflow = list(
      as.formula(paste0("total_inflow ~ ", paste0(total_inflow_expression_vct[present_inflows], collapse = "+"))),
      .at = during_phase_length + during_phase_length_increment +1L,
      .phase = "during",
      .vec_by_flows = "flows",
      .vec_by_states = "state"
    )
  }
  model_simulator = do.call(model_simulator$insert$expressions, args_total_inflow)
  during_phase_length_increment = during_phase_length_increment +1L
  
  args_state_rk4_update = list(
    as.formula("state_rk4 ~ initial_state + total_inflow - total_outflow"),
    .at = during_phase_length + during_phase_length_increment + 1L,
    .phase = "during",
    .vec_by_flows = "flows",
    .vec_by_states = "state"
  )
  model_simulator = do.call(model_simulator$insert$expressions, args_state_rk4_update)
  during_phase_length_increment = during_phase_length_increment +1L
  
  ##end of state_rk4 computation
  
  args_state_update = list(
    as.formula("state ~ (state_rk1 + 2*(state_rk2 + state_rk3) + state_rk4)/6"),
    .at = during_phase_length + during_phase_length_increment + 1L,
    .phase = "during",
    .vec_by_flows = "flow",
    .vec_by_states = "state"
  )
  model_simulator = do.call(model_simulator$insert$expressions, args_state_update)
  
  return(model_simulator)
}

SimulatorConstructor = function(model, ...){
  
  during_phase_length = length(model$expr_list()$during)
  model_simulator = model$simulators$tmb(...)
  infection_flow = model$settings$infection_flow()
  infectious_state = model$settings$infectious_state()

  expanded_flows = model$flows_expanded()

  if(length(infection_flow)>0){
    args = list(
      as.formula(paste0(c(infection_flow), paste0(" ~ per_capita_transmission %*% ", c(infectious_state)))),
      .at = during_phase_length + 1L,
      .phase = "during",
      .vec_by_flows = "flow",
      .vec_by_states = "state"
    )
    model_simulator = do.call(model_simulator$insert$expressions, args)
    during_phase_length = during_phase_length + 1L
  }
  model_simulator = InsertEulerExpressions(model_simulator, during_phase_length, expanded_flows)
  #model_simulator = InsertRK4Expressions(model_simulator, during_phase_length, expanded_flows)
  
  return(model_simulator)
}

age_dir = "Age_model"
epi_dir = "Epi_model"

age_model = CompartmentalAlt(age_dir)
epi_model = CompartmentalAlt(epi_dir)


age_model$expr_list()$print_exprs()
epi_model$expr_list()$print_exprs()

epi_simulator = SimulatorConstructor(epi_model,
                                     time_steps = 25L,
                                     state = c(S = 999, E = 1, I = 0, R = 0),
                                     flow = c(total_foi = NA, progression = 0.1, recovery = 0.05),
                                     N = empty_matrix,
                                     transmissability = 0.75,
                                     per_capita_transmission = empty_matrix,
                                     #initial_state = empty_matrix,
                                     #state_rk1 = empty_matrix,
                                     #state_rk2 = empty_matrix,
                                     #state_rk3 = empty_matrix,
                                     #state_rk4 = empty_matrix,
                                     .mats_to_return = c("state")
                                     )
epi_simulator$report()

age_simulator = SimulatorConstructor(age_model,
                                     time_steps = 25L,
                                     state = c(young = 333, medium = 333, old = 333),
                                     flow = c(ageing_rate = 0.03, birth_rate = 5, death_rate = 0.01),
                                     per_capita_transmission = empty_matrix,
                                     #initial_state = empty_matrix,
                                     #state_rk1 = empty_matrix,
                                     #state_rk2 = empty_matrix,
                                     #state_rk3 = empty_matrix,
                                     #state_rk4 = empty_matrix,
                                     .mats_to_return = c("state")
                                     )


age_simulator$report()


age_model_2 = Compartmental(age_dir)
age_simulator_2 = age_model_2$simulators$tmb(
                                            time_steps = 25L,
                                            state = c(young = 333, medium = 333, old = 333),
                                            flow = c(ageing_rate = 0.03, birth_rate = 5, death_rate = 0.01),
                                            per_capita_transmission = empty_matrix,
                                            .mats_to_return = c("state")
                                            )
age_simulator_2$report()
