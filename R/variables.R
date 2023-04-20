
Variables = function(model) {
  self = Base()
  self$model = model
  self$all = function() Partition(self$model$def$variables())
  self$.type = function(type) {
    type_nm = sprintf("%s_variables", type)
    s = self$model$def$settings()
    var_nms = s[[type_nm]]
    if (length(var_nms) == 0L) {
      warning(
        "\nThere are no ",
        gsub("_", " ", type),
        " variables",
        "\nin this model."
      )
      return(NULL)
    }
    self$all()$filter(var_nms, .wrt = s$required_partitions)
  }
  self$flow = function() self$.type("flow")
  self$state = function() self$.type("state")
  self$infectious_state = function() self$.type("infectious_state")
  self$infected_state = function() self$.type("infected_state")
  self$infection_flow = function() self$.type("infection_flow")
  self$other = function() {
    # TODO: A better way to handle the NULL case would make it possible
    # to return a null Partition object. This is currently not possible
    # for 'technical' reasons.
    all_types = c("flow", "state", "infectious_state", "infected_state", "infection_flow")
    if (length(self$model$labels$other()) == 0L) {
      warning(
        "\nThere are no other variables in this model",
        "\nexcept for state and flow variables."
      )
      return(NULL)
    }
    s = self$model$def$settings()
    self$all()$filter_out(
      c(s$flow_variables, s$state_variables),
      .wrt = s$required_partitions
    )
  }
  return_object(self, "Variables")
}

VariableLabels = function(variables) {
  self = Base()
  self$variables = variables
  self$.type = function(type) {
    v = self$variables[[type]]()
    if (is.null(v)) return(v)
    v$labels()
  }
  self$all = function() self$variables$all()$labels()
  self$flow = function() self$variables$flow()$labels()
  self$state = function() self$variables$state()$labels()
  self$infectious_state = function() self$variables$infectious_state()$labels()
  self$infected_state = function() self$variables$infected_state()$labels()
  self$infection_flow = function() self$variables$infection_flow()$labels()
  self$other = function() setdiff(self$all(), c(self$state(), self$flow()))
  return_object(self, "VariableLabels")
}
