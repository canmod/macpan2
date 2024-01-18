## for back-compat
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
