InsertTotalFLowExpressions = function(model_simulator, expanded_flows){
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
  
  infection_flow = model_simulator$compartmental_model$settings$infection_flow()
  infectious_state = model_simulator$compartmental_model$settings$infectious_state()
    
  if(length(infection_flow)>0){
  args_infection_flow = list(
      as.formula(paste0(c(infection_flow), paste0(" ~ per_capita_transmission %*% ", c(infectious_state)))),
      .at = Inf,
      .phase = "during",
      .vec_by_flows = "flow",
      .vec_by_states = "state"
    )
  }
  
  args_per_capita = list(
    as.formula("per_capita ~ state[per_capita_from]*flow[per_capita_flow]"),
    .at = Inf,
    .phase = "during",
    .vec_by_flows = "flows",
    .vec_by_states = "state"
  )
  
  args_absolute = list(
    as.formula("absolute ~ flow[absolute_flow]"),
    .at = Inf,
    .phase = "during",
    .vec_by_flows = "flows",
    .vec_by_states = "state"
  )
  
  args_per_capita_inflow = list(
    as.formula("per_capita_inflow ~ state[per_capita_inflow_from]*flow[per_capita_inflow_flow]"),
    .at = Inf,
    .phase = "during",
    .vec_by_flows = "flows",
    .vec_by_states = "state"
  )
  
  args_per_capita_outflow = list(
    as.formula("per_capita_outflow ~ state[per_capita_outflow_from]*flow[per_capita_outflow_flow]"),
    .at = Inf,
    .phase = "during",
    .vec_by_flows = "flow",
    .vec_by_states = "state"
  )
  
  args_absolute_inflow = list(
    as.formula("absolute_inflow ~ flow[absolute_inflow_flow]"),
    .at = Inf,
    .phase = "during",
    .vec_by_flows = "flows",
    .vec_by_states = "state"
  )
  
  args_absolute_outflow = list(
    as.formula("absolute_outflow ~ flow[absolute_outflow_flow]"),
    .at = Inf,
    .phase = "during",
    .vec_by_flows = "flows",
    .vec_by_states = "state"
  )
  
  total_inflow_expression_vct = c(
    "groupSums(per_capita, per_capita_to, state_length)",
    "groupSums(absolute, absolute_to, state_length)",
    "groupSums(per_capita_inflow, per_capita_inflow_to, state_length)",
    "groupSums(absolute_inflow, absolute_inflow_to, state_length)"
  ) 
  
  if(!any(present_inflows)){
    args_total_inflow = list(
      as.formula("total_inflow ~ 0"),
      .at = Inf,
      .phase = "during",
      .vec_by_flows = "flows",
      .vec_by_states = "state"
    )
  } else {
    args_total_inflow = list(
      as.formula(paste0("total_inflow ~ ", paste0(total_inflow_expression_vct[present_inflows], collapse = "+"))),
      .at = Inf,
      .phase = "during",
      .vec_by_flows = "flows",
      .vec_by_states = "state"
    )
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
      .at = Inf,
      .phase = "during",
      .vec_by_flows = "flows",
      .vec_by_states = "state"
    )
  } else {
    args_total_outflow = list(
      as.formula(paste0("total_outflow ~ ", paste0(total_outflow_expression_vct[present_outflows], collapse = "+"))),
      .at = Inf,
      .phase = "during",
      .vec_by_flows = "flows",
      .vec_by_states = "state"
    )
  }
  
  if(length(infection_flow)>0){
    args_list = list(args_infection_flow, args_per_capita, args_absolute, args_per_capita_inflow,
                     args_per_capita_outflow, args_absolute_inflow, args_absolute_outflow, args_total_inflow, args_total_outflow)
    present_args = c(TRUE, present_flows, rep(TRUE, times = 2))
  }
  else{
    args_list = list(args_per_capita, args_absolute, args_per_capita_inflow,
                  args_per_capita_outflow, args_absolute_inflow, args_absolute_outflow, args_total_inflow, args_total_outflow)
    present_args = c(present_flows, rep(TRUE, times = 2))
  }
  for(arguments in args_list[present_args]){
    model_simulator = do.call(model_simulator$insert$expressions, arguments)
  }
  
  return(model_simulator)
}

InsertEulerExpressions = function(model_simulator, expanded_flows){

  model_simulator = InsertTotalFLowExpressions(model_simulator, expanded_flows)
  
  args_state_update = list(
    as.formula("state ~ state + total_inflow - total_outflow"),
    .at = Inf,
    .phase = "during",
    .vec_by_flows = "flow",
    .vec_by_states = "state"
  )
  model_simulator = do.call(model_simulator$insert$expressions, args_state_update)
  
  return(model_simulator)
}

# Note that using the RK4 method requires 5 new matrices be created initial_state, rk1, rk2, rk3, rk4.
# All should be initialized as empty_matrix.
# Currently this method is incomplete. In principle "per_capita_transmission" should
# be recomputed before each call to "InsertTotalFlowExpressions". This method should still work provided 
# "per_capita_transmission" is constant or approximately constant at the scale of a single time step.
InsertRK4Expressions = function(model_simulator, expanded_flows){
  
  args_record_initial_state = list(
    as.formula("initial_state ~ state"),
    .at = Inf,
    .phase = "during",
    .vec_by_flows = "flow",
    .vec_by_states = "state"
  )
  model_simulator = do.call(model_simulator$insert$expressions, args_record_initial_state)
  
  model_simulator = InsertTotalFLowExpressions(model_simulator, expanded_flows)
  
  
  args_rk1 = list(
    as.formula("rk1 ~ total_inflow - total_outflow"),
    .at = Inf,
    .phase = "during",
    .vec_by_flows = "flow",
    .vec_by_states = "state"
  )
  model_simulator = do.call(model_simulator$insert$expressions, args_rk1)
  
  args_rk2_state_update = list(
    as.formula("state ~ initial_state + (rk1/2)"),
    .at = Inf,
    .phase = "during",
    .vec_by_flows = "flow",
    .vec_by_states = "state"
  )
  model_simulator = do.call(model_simulator$insert$expressions, args_rk2_state_update)
  

  model_simulator = InsertTotalFLowExpressions(model_simulator, expanded_flows)
  
  args_rk2 = list(
    as.formula("rk2 ~ (total_inflow - total_outflow)/2"), 
    .at = Inf,
    .phase = "during",
    .vec_by_flows = "flow",
    .vec_by_states = "state"
  )
  model_simulator = do.call(model_simulator$insert$expressions, args_rk2)
  
  args_rk3_state_update = list(
    as.formula("state ~ initial_state + (rk2/2)"),
    .at = Inf,
    .phase = "during",
    .vec_by_flows = "flow",
    .vec_by_states = "state"
  )
  model_simulator = do.call(model_simulator$insert$expressions, args_rk3_state_update)
  

  model_simulator = InsertTotalFLowExpressions(model_simulator, expanded_flows)
  
  args_rk3 = list(
    as.formula("rk3 ~ (total_inflow - total_outflow)/2"),
    .at = Inf,
    .phase = "during",
    .vec_by_flows = "flow",
    .vec_by_states = "state"
  )
  model_simulator = do.call(model_simulator$insert$expressions, args_rk3)
  
  args_rk4_state_update = list(
    as.formula("state ~ initial_state + rk3"),
    .at = Inf,
    .phase = "during",
    .vec_by_flows = "flow",
    .vec_by_states = "state"
  )
  model_simulator = do.call(model_simulator$insert$expressions, args_rk4_state_update)
  

  model_simulator = InsertTotalFLowExpressions(model_simulator, expanded_flows)
  
  
  args_rk4 = list(
    as.formula("rk4 ~ total_inflow - total_outflow"),
    .at = Inf,
    .phase = "during",
    .vec_by_flows = "flow",
    .vec_by_states = "state"
  )
  model_simulator = do.call(model_simulator$insert$expressions, args_rk4)
  
  args_final_state_update = list(
    as.formula("state ~ initial_state +(rk1 + 2*(rk2+rk3) + rk4)/6"),
    .at = Inf,
    .phase = "during",
    .vec_by_flows = "flow",
    .vec_by_states = "state"
  )
  model_simulator = do.call(model_simulator$insert$expressions, args_final_state_update)
  
  return(model_simulator)
}