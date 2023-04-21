variables_product = function(model1, model2){
  output_vars = union_vars(
    cartesian(model1$variables$state(), model2$variables$state()),
    cartesian(model1$variables$state(), model2$variables$flow()),
    cartesian(model1$variables$flow(), model2$variables$state()),
    model1$variables$other(),
    model2$variables$other()
  )
  return(output_vars)
}

flows_product = function(model1, model2){
  m1_flows = model1$flows()
  m1_required_partitions = paste0(model1$def$settings()$required_partitions, collapse = ".")
  
  m2_flows = model2$flows()
  m2_required_partitions = paste0(model2$def$settings()$required_partitions, collapse = ".")
  
  m1_flows$from_to_partition = m2_required_partitions
  m1_flows$flow_partition = m2_required_partitions
  
  m2_flows$from_to_partition = m1_required_partitions
  m2_flows$flow_partition = m1_required_partitions
  
  return(rbind(m1_flows, m2_flows))
}
