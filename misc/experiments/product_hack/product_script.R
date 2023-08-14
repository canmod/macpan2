library(macpan2)
library(oor)

Derivations2ExprListAlt = function(user_expr) {
  self = Base()
  self$user_expr = user_expr
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
    c(
      self$.user_expr_list[user_phases == phase]
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
    Derivations2ExprListAlt(UserExpr(self))$expr_list()
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
  args = list(
    as.formula("per_capita ~ state[per_capita_from]*flow[per_capita_flow]"),
    .at = during_phase_length + 1L,
    .phase = "during",
    .vec_by_flows = "flows",
    .vec_by_states = "state"
  )
  model_simulator = do.call(model_simulator$insert$expressions, args)
  
  args = list(
    as.formula("absolute ~ flow[absolute_flow]"),
    .at = during_phase_length + 2L,
    .phase = "during",
    .vec_by_flows = "flows",
    .vec_by_states = "state"
  )
  
  args = list(
    as.formula("per_capita_inflow ~ state[per_capita_inflow_from]*flow[per_capita_inflow_flow]"),
    .at = during_phase_length + 3L,
    .phase = "during",
    .vec_by_flows = "flows",
    .vec_by_states = "state"
  )
  model_simulator = do.call(model_simulator$insert$expressions, args)
  
  args = list(
    as.formula("per_capita_outflow ~ state[per_capita_outflow_from]*flow[per_capita_outflow_flow]"),
    .at = during_phase_length + 4L,
    .phase = "during",
    .vec_by_flows = "flow",
    .vec_by_states = "state"
  )
  model_simulators = do.call(model_simulator$insert$expressions, args)
  
  args = list(
    as.formula("absolute_inflow ~ flow[absolute_inflow_flow]"),
    .at = during_phase_length + 5L,
    .phase = "during",
    .vec_by_flows = "flows",
    .vec_by_states = "state"
  )
  model_simulator = do.call(model_simulator$insert$expressions, args)
  
  args = list(
    as.formula("absolute_outflow ~ flow[absolute_outflow_flow]"),
    .at = during_phase_length + 6L,
    .phase = "during",
    .vec_by_flows = "flows",
    .vec_by_states = "state"
  )
  model_simulator = do.call(model_simulator$insert$expressions, args)
  
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
  #present_flows = unlist(lapply(flow_types, flow_tester), use.names = FALSE)
  present_inflows = unlist(lapply(inflow_flow_types, flow_tester), use.names = FALSE)
  present_outflows = unlist(lapply(outflow_flow_types, flow_tester), use.names = FALSE)
  
  
  
  total_outflow_expression_vct = c(
    "groupSums(per_capita, per_capita_from, state_length)",
    "groupSums(absolute, absolute_from, state_length)",
    "groupSums(per_capita_outflow, per_capita_outflow_from, state_length)",
    "groupSums(absolute_outflow, absolute_outflow_from, state_length)"
  )
  
  if(!any(unlist(present_outflows))){
    args = list(
      as.formula("total_outflow ~ 0"),
      .at = during_phase_length + 7L,
      .phase = "during",
      .vec_by_flows = "flows",
      .vec_by_states = "state"
    )
  } else {
    args = list(
      as.formula(paste0("total_outflow ~ ", paste0(total_outflow_expression_vct[present_outflows], collapse = "+"))),
      .at = during_phase_length + 7L,
      .phase = "during",
      .vec_by_flows = "flows",
      .vec_by_states = "state"
    )
  }
  model_simulator = do.call(model_simulator$insert$expressions, args)
  
  total_inflow_expression_vct = c(
    "groupSums(per_capita, per_capita_to, state_length)",
    "groupSums(absolute, absolute_to, state_length)",
    "groupSums(per_capita_inflow, per_capita_inflow_to, state_length)",
    "groupSums(absolute_inflow, absolute_inflow_to, state_length)"
  ) 
  
  if(!any(unlist(present_inflows))){
    args = list(
      as.formula("total_inflow ~ 0"),
      .at = during_phase_length + 8L,
      .phase = "during",
      .vec_by_flows = "flows",
      .vec_by_states = "state"
    )
  } else {
    args = list(
      as.formula(paste0("total_inflow ~ ", paste0(total_inflow_expression_vct[present_inflows], collapse = "+"))),
      .at = during_phase_length + 8L,
      .phase = "during",
      .vec_by_flows = "flows",
      .vec_by_states = "state"
    )
  }
  model_simulator = do.call(model_simulator$insert$expressions, args)
  
  args = list(
    as.formula("state ~ state + total_inflow - total_outflow"),
    .at = during_phase_length + 9L,
    .phase = "during",
    .vec_by_flows = "flows",
    .vec_by_states = "state"
  )
  model_simulator = do.call(model_simulator$insert$expressions, args)
  
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
                                     .mats_to_return = c("state")
                                     )
epi_simulator$report()

age_simulator = SimulatorConstructor(age_model,
                                     time_steps = 25L,
                                     state = c(young = 333, medium = 333, old = 333),
                                     flow = c(ageing_rate = 0.03, birth_rate = 5, death_rate = 0.01),
                                     per_capita_transmission = empty_matrix,
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
