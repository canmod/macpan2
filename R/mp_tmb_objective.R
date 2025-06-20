#' Value of the Objective Function of a Model
#' 
#' @param model A model with an objective function, probably one produced using
#' \code{\link{mp_tmb_calibrator}}.
#' @inheritParams mp_trajectory_par
#' @export
mp_tmb_objective = function(model
    , parameter_updates = list()
    , baseline = c("recommended", "default", "optimized")
  ) {
  UseMethod("mp_tmb_objective")
}

#' @export
mp_tmb_objective.TMBSimulator = function(model
    , parameter_updates = list()
    , baseline = c("recommended", "default", "optimized")
  ) {
  baseline = match.arg(baseline)
  value_column_name = value_column_simulator_util(baseline)
  vector = objective_vec_util(model, parameter_updates, value_column_name)
  model$objective(vector)
}

#' @export
mp_tmb_objective.TMBCalibrator = function(model
    , parameter_updates = list()
    , baseline = c("recommended", "default", "optimized")
  ) {
  mp_tmb_objective(model$simulator, parameter_updates, baseline)
}

# take a simulator and return the parameter vector that can be
# passed to TMB fn, gr, he
objective_vec_util = function(simulator, parameter_updates, value_column_name) {
  vector = updated_param_vector(parameter_updates
    , simulator$current$params_frame()
    , matrix = "mat", value = value_column_name
  )
  return(vector)
}


TransPrototype = function(formula, trans, input_var = "x", output_var = "y") {
  self = MethodPrototype(formula, c(input_var, output_var), character())
  self$trans = trans
  oor::return_object(self, "TransPrototype")
}

ff = function(model, formula) {
  protos = list(
      MethodPrototype(~x, "x", character())
    , MethodPrototype(~x[i], "x", "i")
    , MethodPrototype(~x[i,j], "x", c("i", "j"))
  )
  for (proto in protos) {
    if (proto$consistent(formula)) {
      mats = proto$mat_args(formula)
      if (!all(mats %in% names(model$default))) {
        stop("Matrices with parameters must be have defaults.")
      }
      if (length(mats) != 1L)
      ints = proto$int_vec_args(formula)
      model
    }
  }
}
# mm = MethodPrototype(~x[i], "x", "i")
# mm$consistent(~state[I])
# mm$mat_args(~state[I])
# mm$int_vec_args(~state[I])

#xx = TransPrototype(y ~ logit(x), Logit)
# Logit("testing")$trans_two_sided_formula()

TransTypes = function() {
  self = MethodTypeUtils()
  self$method_ordering = c("meth_from_rows", "meth_to_rows", "meth_rows_to_rows", "meth_mat_mult_to_rows", "meth_tv_mat_mult_to_rows", "meth_group_sums", "meth_tv_mat", "meth_rows_times_rows")
  self$trans_log = TransPrototype(y ~ log(x), c("x", "y"), character())
  self$trans_logit = TransPrototype(y ~ logit(x), c("x", "y"), character())
  return_object(self, "MethodTypes")
}

assert_string = function(x) {
  if (length(x) != 1L) {
    stop("Cannot yet create parameter sub-vectors from more than one matrix")
  }
  x
}
