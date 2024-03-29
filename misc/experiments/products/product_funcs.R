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
  # for model i:
  #   - from_partition: from_partition_i
  #   - to_partition: to_partition_i
  #   - flow_partition: flow_partition_i
  #   - from_to_partition: from_to_partition_i dot required_partition_j

  dot = function(...) paste0(..., collapse = ".")
  cdot = function(x, y) {
    ifelse(
      is_empty(x),
      y,
      paste(x, y, sep = ".")
    )
  }
  is_empty = function(x) nchar(x) == 0L

  m1_flows = model1$flows()
  m1_required_partitions = dot(model1$def$settings()$required_partitions)
  #m1_flows$from_to_partition = ifelse(
  #  is_empty(m1_flows$from_to_partition),
  #  m1_required_partitions,
  #  m1_flows$from_to_partition
  #)
  print(m1_required_partitions)

  m2_flows = model2$flows()
  m2_required_partitions = dot(model2$def$settings()$required_partitions)
  #m2_flows$from_to_partition = ifelse(
  #  is_empty(m2_flows$from_to_partition),
  #  m2_required_partitions,
  #  m2_flows$from_to_partition
  #)
  print(m2_required_partitions)

  m1_flows$from_to_partition = cdot(m1_flows$from_to_partition, m2_required_partitions)
  m2_flows$from_to_partition = cdot(m2_flows$from_to_partition, m1_required_partitions)
  m1_flows$from_flow_partition = cdot(m1_flows$from_flow_partition, m2_required_partitions)
  m2_flows$from_flow_partition = cdot(m2_flows$from_flow_partition, m1_required_partitions)

  return(rbind(m1_flows, m2_flows))
}
