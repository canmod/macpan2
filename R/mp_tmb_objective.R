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

if (FALSE) {

## take a description of a parameter and return a 
## (possibly empty) list containing expressions
## to put at the beginning of the before phase.
param_desc_to_inverse = function(desc, model) UseMethod("param_desc_to_inverse")

param_desc_to_inverse.character = function(desc, model) list()

param_desc_to_inverse.formula = function(desc, model) list(desc)

param_desc_to_inverse.list = function(desc, model) c(lapply(desc, param_desc_to_inverse))

param_desc_to_inverse.Transform = function(desc, model) list(desc$inverse_two_sided_formula())


## take a description of a parameter and return
## a string giving the name of the parameter
param_desc_to_name = function(desc, model) UseMethod("param_desc_to_name")

param_desc_to_name.character = function(desc, model) {
  existing_names = model$default |> names()
  if (!desc %in% existing_names) {
    stop(
      "Parameter is not already in the model. ",
      "Plese pass a formula to param_description ",
      "instead to create it."
    )
  }
  assert_string(desc)
}

param_desc_to_name.formula = function(desc) trans_lhs_var(desc)

param_desc_to_name.list = function(desc) {
  if (!all(vapply(desc, is_two_sided, logical(1L)))) {
    stop(
      "List-valued parameter descriptions ",
      "must contain only two-sided formulas."
    )
  }
  param_desc_to_name(desc[[length(desc)]])  ## lhs of the last expression is the parameter name
}

param_desc_to_name.Transform = function(desc, model) desc$variable




## take a description of a parameter and return
## a transformation object
param_desc_to_trans_obj = function(desc) UseMethod("param_desc_to_inverse")

param_desc_to_trans_obj.character = function(desc) {
  assert_string(desc)
  two_sided(desc, desc) |> list()
}

param_desc_to_trans_obj.formula = function(desc) find_trans(desc)

param_desc_to_trans_obj.list = function(desc) {
  param_desc_to_name(desc) |> param_desc_to_trans_obj()
}


#' @param param_description One of three things: (1) a character string
#' giving the name of the matrix to be used as a statistical parameter,
#' (2) a transformation object (e.g. \code{\link{Log}}), or (3) a list
#' of formulas, the last of which has the parameter name on the 
#' left-hand-side.
#' 
#' @section Types of Parameter Descriptions:
#' 
#' * Character String: All elements of a single matrix are used as parameters.
#' * Transformation Object: All elements of a single matrix are transformed,
#' and these transformed elements are used as parameters. This is implemented
#' by applying the inverse transformation to the matrix as the first step
#' in the `before` phase of the simulation, which is evaluated `before` the
#' simulation loop begins. 
#' * List of Formulas: A list of formulas which, taken together, apply the
#' inverse transformation of any set of matrices or matrix elements. This
#' inverse will 
#' 
#' @section Transformation Object:
#'
#' @section List of Formulas:
#' @noRd
TMBObjectiveParam = function(model, param_description) {
  self = Base()
  self$model = model
  self$param_description = param_description
  self$trans_obj = function() param_desc_to_trans_obj(self$param_description)
  self$trans_name = function() param_desc_to_name(self$param_description)
  self$trans_expr_list = function() self$trans_obj()
  
  return_object(self, "TMBObjectiveParam")
}
}