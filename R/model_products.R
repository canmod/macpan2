VariablesProduct = function(model1, model2){
  output_vars = union_vars(
    cartesian(model1$variables$state(), model2$variables$state()),
    cartesian(model1$variables$state(), model2$variables$flow()),
    cartesian(model1$variables$flow(), model2$variables$state())
    #model1$variables$other(),
    #model2$variables$other()
  )
  return(output_vars)
}


FlowsProduct = function(model1, model2){

  is_empty = function(x) nchar(x) == 0L
  #dot = function(...) paste0(..., collapse = ".")
  cdot = function(x, y) {
    ifelse(
      is_empty(x),
      y,
      paste(x, y, sep = ".")
    )
  }

  m1_flows = model1$flows_explicit()
  m1_required_partitions = model1$settings$name()

  m2_flows = model2$flows_explicit()
  m2_required_partitions = model2$settings$name()

  m1_flows$from_to_partition = cdot(m1_flows$from_to_partition, m2_required_partitions)
  m2_flows$from_to_partition = cdot(m2_flows$from_to_partition, m1_required_partitions)
  m1_flows$from_flow_partition = cdot(m1_flows$from_flow_partition, m2_required_partitions)
  m2_flows$from_flow_partition = cdot(m2_flows$from_flow_partition, m1_required_partitions)

  return(rbind(m1_flows, m2_flows))
}


SettingsProduct = function(model1, model2, product_variables) {
  model1_required_partitions = model1$settings$name()
  model2_required_partitions = model2$settings$name()
  
  required_partitions = c(model1$settings$names(), model2$settings$names())
  
  null_partitions = "Null"
  
  state_variables = cartesian(model1$variables$state(), model2$variables$state())$labels()
  
  flow_variables = union_vars(cartesian(model1$variables$state(), model2$variables$flow()),
                              cartesian(model1$variables$flow(), model2$variables$state()))$labels()
  
  infectious_state_variables = union_vars(cartesian(model1$variables$infectious_state(), model2$variables$state()),
                                          cartesian(model1$variables$state(), model2$variables$infectious_state()))$labels()
  
  infected_state_variables = union_vars(cartesian(model1$variables$infected_state(), model2$variables$state()),
                                        cartesian(model1$variables$state(), model2$variables$infected_state()))$labels()
  
  infectious_flow_variables = union_vars(cartesian(model1$variables$infection_flow(), model2$variables$state()),
                                         cartesian(model1$variables$state(), model2$variables$infection_flow()))$labels()
  
  output_settings = list(required_partitions, null_partitions, state_variables, infectious_state_variables,
                         infected_state_variables, infectious_flow_variables)
  
  names(output_settings) = c("required_partitions", "null_partitions", "state_variables", "infectious_state_variables",
                             "infected_state_variables", "infectious_flow_variables")
  
  return(output_settings)
}

ModelProduct = function(model1, model2){
  variables = VariablesProduct(model1, model2)
  flows = FlowsProduct(model1, model2)
  settings = SettingsProduct(model1, model2, variables)
  derivations = list()

  #TODO$ write the above to a model definition directory
}