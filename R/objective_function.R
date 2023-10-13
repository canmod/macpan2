#' Objective Function
#'
#' Define the objective function of a compartmental model in TMB.
#'
#' @param obj_fn_expr One sided \code{\link{formula}} giving the objective
#' function of a TMB model. The right hand side expression must contain only
#' the names of matrices in the model, functions defined in \code{macpan2.cpp},
#' and numerical literals (e.g. \code{3.14}).
#'
#' @return Object of class \code{ObjectiveFunction} with the following methods.
#'
#' ## Methods
#'
#' * `data_arg(..., .existing_literals)` -- Return the following components of the data structure
#' to pass to C++.
#'     * `o_table_x` -- Objective function parse table column giving an index for looking up either
#'     function, matrix, or literal.
#'     * `o_table_n` -- Objective function parse table column giving the number of arguments in
#'     functions.
#'     * `o_table_i` -- Objective function parse table column giving indices for looking up the
#'     rows in the parse table corresponding with the first argument of the
#'     function.
#'     * `literals` -- Numeric vector of literals that can were used in the
#'     expressions of the model.
#'
#' ## Method Arguments
#'
#' * `...`: Character vector containing the names of the matrices in the model.
#' * `.existing_literals`: Numeric vector giving the literals used in the
#' model expressions produced before the objective function.
#'
#' @export
ObjectiveFunction = function(obj_fn_expr) {

  ## Inherit Private Methods
  self = ExprListUtils()

  ## Args
  self$obj_fn_expr = obj_fn_expr

  ## Standard Methods
  self$formula_list = function() list(self$obj_fn_expr)
  self$.literals = function() {
    self$.parsed_expr_list(rhs, self$expr_list$.literals())$valid_literals
  }
  self$.o_table = function() {
    l = self$.parsed_expr_list(rhs, self$expr_list$.literals())
    o_table = l$parse_table[c("x", "n", "i")] |> as.list()
    self$.set_name_prefix(o_table, "o_table_")
  }
  self$data_arg = function() {
    l = self$.o_table()
    l$literals = self$.literals()
    l
  }

  ## Composition
  self$init_mats = MatsList()
  self$expr_list = ExprList()
  self$engine_methods = EngineMethods()

  return_object(self, "ObjectiveFunction")
}
