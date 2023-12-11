#' Compartmental Modelling Language
#'
#'
Clause = function() {
  self = Base()
  self$render = function(engine) {}
  return_object(self, "Calculation")
}

Transition = function(from, to, rate) {
  self = Calculation()
  self$from = from
  self$to = to
  self$rate = rate
  return_object(self, "Transition")
}
