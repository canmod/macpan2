TMBSimulatorGetters = function(simulator) {
  self = Base()
  self$simulator = simulator
  self$initial = function(matrix_name) {
    self$simulator$tmb_model$init_mats$get(matrix_name)
  }
  self$simulator$tmb_model$expr_list$during
  return_object(self, "TMBSimulatorGetters")
}
