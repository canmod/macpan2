


TransPrototype = function(formula, trans, input_var = "x", output_var = "y") {
  self = macpan2:::MethodPrototype(formula, c(input_var, output_var), character())
  self$trans = trans
  oor:::return_object(self, "TransPrototype")
}

ff = function(model, formula) {
  protos = list(
      macpan2:::MethodPrototype(~x, "x", character())
    , macpan2:::MethodPrototype(~x[i], "x", "i")
    , macpan2:::MethodPrototype(~x[i,j], "x", c("i", "j"))
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
# mm = macpan2:::MethodPrototype(~x[i], "x", "i")
# mm$consistent(~state[I])
# mm$mat_args(~state[I])
# mm$int_vec_args(~state[I])

#xx = TransPrototype(y ~ logit(x), Logit)
# macpan2:::Logit("testing")$trans_two_sided_formula()

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
  existing_names = sir$default |> names()
  if (!desc %in% existing_names) {
    stop(
      "Parameter is not already in the model. ",
      "Plese pass a formula to param_description ",
      "instead to create it."
    )
  }
  assert_string(desc)
}
param_desc_to_name.formula = function(desc) macpan2:::trans_lhs_char(desc)
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
  macpan2:::two_sided(desc, desc) |> list()
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
#' @section Types of Parameter Descriptions
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
#' @section Transformation Object
#'
#' @section List of Formulas 
#' 
TMBObjectiveParam = function(model, param_description) {
  self = Base()
  self$model = model
  self$param_description = param_description
  self$trans_obj = function() param_desc_to_trans_obj(self$param_description)
  self$trans_name = function() param_desc_to_name(self$param_description)
  self$trans_expr_list = function() self$trans_obj()
  
  return_object(self, "TMBObjectiveParam")
}
