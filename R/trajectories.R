TMBSimVar = function(simulator, variable_name) {
  self$simulator = simulator
  self$variable_name = variable_name
  self$derivation
  return_object(self, "TMBSimVar")
}
