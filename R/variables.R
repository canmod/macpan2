
Variables = function(model) {
  self = Base()
  self$model = model
  self$all = function() Partition(self$model$def$variables())
  self$.type = function(type) {
    labels_this_type = self$model$settings$variable(type)
    var_part = self$model$settings$var_partitions()
    wrt = self$model$settings$name()
    if (is.null(var_part)) {
      vars = self$all()$filter_ordered(labels_this_type, .wrt = wrt)
    } else {
      vars = self$all()$filter(labels_this_type, .wrt = var_part)
    }
    vars
    # if (length(var_nms) == 0L) {
    #   warning(
    #     "\nThere are no ",
    #     gsub("_", " ", type),
    #     " variables",
    #     "\nin this model."
    #   )
    #   return(NULL)
    # }
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
  initialize_cache(self, "all")
  return_object(self, "Variables")
}

NullLabels = function() {
  self = Base()
  self$variables = NULL ## TODO: should be NullVariables, which doesn't yet exist
  self$all = function() character(0L)
  self$flow = function() character(0L)
  self$state = function() character(0L)
  self$infectious_state = function() character(0L)
  self$infected_state = function() character(0L)
  self$infection_flow = function() character(0L)
  self$other = function() character(0L)
  initialize_cache(self, "all", "flow", "state", "infectious_state", "infection_flow", "other")
  return_object(self, "NullLabels")
}

VariableLabels = function(variables) {
  self = Base()
  self$variables = variables
  self$.type = function(type) {
    v = self$variables[[type]]()
    if (is.null(v)) return(v)
    v$labels()
  }
  self$rp = function() self$variables$model$settings$names()
  self$all = function() self$variables$all()$select(self$rp())$labels()
  self$flow = function() self$variables$flow()$select(self$rp())$labels()
  self$state = function() self$variables$state()$select(self$rp())$labels()
  self$infectious_state = function() self$variables$infectious_state()$select(self$rp())$labels()
  self$infected_state = function() self$variables$infected_state()$select(self$rp())$labels()
  self$infection_flow = function() self$variables$infection_flow()$select(self$rp())$labels()
  self$other = function() setdiff(self$all(), c(self$state(), self$flow()))
  initialize_cache(self, "all", "flow", "state", "infectious_state", "infection_flow", "other", "rp")
  return_object(self, "VariableLabels")
}

VariableIndices = function(labels) {
  self = Base()
  self$flow = FlowTypeIndices(labels)
  self$transmission = TransmissionIndices(labels)
  return_object(self, "VariableIndices")
}

IndexUtilities = function(labels) {
  self = Base()
  self$labels = labels
  self$variables = labels$variables
  self$model = labels$variables$model
  self$match = function(...) match(...) - 1L  # -1 for zero-based C++ indexing
  self$.state = function() self$labels$state()
  self$.flow = function() self$labels$flow()
  self$.make_flow_method = function(flow_component, flow_type, vector_name) {
    force(flow_type); force(flow_component); force(vector_name)
    function() {
      f = self$model$flows_expanded()
      flow_labels = f[[flow_component]][f$type == flow_type]
      self$match(flow_labels, self$labels[[vector_name]]())
    }
  }
  self$flow_types = c(
    "per_capita", "per_capita_inflow", "per_capita_outflow",
    "absolute", "absolute_inflow", "absolute_outflow"
  )
  return_object(self, "IndexUtilities")
}

FlowIndices = function(labels, type) {
  self = IndexUtilities(labels)
  self$type = type
  self$from = self$.make_flow_method("from", type, "state")
  self$to = self$.make_flow_method("to", type, "state")
  self$flow = self$.make_flow_method("flow", type, "flow")
  initialize_cache(self, "from", "to", "flow")
  return_object(self, "FlowIndices")
}

FlowTypeIndices = function(labels) {
  self = IndexUtilities(labels)
  for (type in self$flow_types) self[[type]] = FlowIndices(labels, type)
  self$invalidate = function() {
    for (type in self$flow_types) self[[type]]$cache$invalidate()
  }
  return_object(self, "FlowTypeIndices")
}

TransmissionIndices = function(labels) {
  self = IndexUtilities(labels)
  self$infectious_state = function() {
    self$match(
      self$labels$infectious_state(),
      self$.state()
    )
  }
  self$infection_flow = function() {
    self$match(
      self$labels$infection_flow(),
      self$.flow()
    )
  }
  return_object(self, "TransmissionIndices")
}

