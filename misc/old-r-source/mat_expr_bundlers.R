TimeVaryingParameter = function(simulator) {
  self = Base()
  self$simulator = simulator
  return_object(self, "TimeVaryingParameter")
}

ParameterTransformation = function(simulator) {
  self = Base()
  self$simulator = simulator
  self$create = function(transformation) {}
}


