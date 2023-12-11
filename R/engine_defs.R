Engine = function() {
  self = Base()
  self$null_simulator = function() stop("abstract method")
  return_object(self, "Engine")
}

TMB = function() {
  self = Engine()
  self$null_simulator = function() TMBSimulator(TMBModel())
  return_object(self, "TMB")
}
