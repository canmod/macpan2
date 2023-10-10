Settings = function(model) {
  self = Base()
  self$model = model
  self$.settings = model$def$settings
  self$name = function() to_name(self$.settings()$required_partition)
  self$names = function() to_names(self$.settings()$required_partition)
  self$null = function() self$.settings()$null_partition
  self$var_partitions = function() self$.settings()$var_partitions
  self$variable = function(type) {
    type_nm = sprintf("%s_variables", type)
    var_nms = self$.settings()[[type_nm]]
    as.character(var_nms)  ## treat NULL values as length-zero character vectors
  }
  self$state = function() self$variable("state")
  self$flow = function() self$variable("flow")
  self$infectious_state = function() self$variable("infectious_state")
  self$infected_state = function() self$variable("infected_state")
  self$infection_flow = function() self$variable("infection_flow")
  return_object(self, "Settings")
}
