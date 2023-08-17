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
  m1_required_partitions = model1$settings$name()
  m2_required_partitions = model2$settings$name()
  
  
  required_partitions = c(model1$settings$names(), model2$settings$names())
  null_partition = "Null"
  state_variables = cartesian(model1$variables$state(), model2$variables$state())$dotted()
  flow_variables = union_vars(cartesian(model1$variables$state(), model2$variables$flows()),
                              cartesian(model1$variables$flows(), model2$variables$state()))$dotted()
  infectious_state_variables = c(product_variables$filter(model1$settings$infectious_state(), .wrt = m1_required_partitions)$dotted(),
                                 product_variables$filter(model2$settings$infectious_state(), .wrt = m2_required_partitions)$dotted())
  infected_state_variables = c(product_variables$filter(model1$settings$infected_state(), .wrt = model1_required_partitions)$dotted(),
                               product_variables$filter(model2$settings$infected_state(), .wrt = model2_required_partitions)$dotted())
  infectious_flow_variables = c(product_variables$filter(model1$settings$infection_flow(), .wrt = model1_required_partitions)$dotted(),
                                product_variables$filter(model2$settings$infection_flow(), .wrt = model2_required_partitions)$dotted())
  return(list(required_partitions, null_partitions, state_variables, infectious_state_variables,
              infected_state_variables, infectious_flow_variables))
}

# ModelProduct = function(model1, model2){
#   variables = VariablesProduct(model1, model2)
#   flows = FlowsProduct(model1, model2)
#   settings = SettingsProduct(model1, model2, variables)
#   
#   #TODO: turn the above into a model object
#   #TODO: return model object
# }