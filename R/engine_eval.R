#' Engine Evaluation
#'
#' Evaluate an expression in the TMB-based C++ simulation and objective
#' function engine.
#'
#' This function is useful for trying out functions that can be used to
#' define \code{macpan2} models.
#'
#' @param e Expression as a two-sided formula.
#' @param ... Named objects that can be coerced into numeric matrices.
#' @param .matrix_to_return Optional name of one of the matrices given
#' in \code{...} to be returned. If this argument is missing, the
#' matrix that will be returned is the matrix on the left-hand-side of the
#' expression.
#'
#' @return Matrix being produced on the left-hand-side or matrix given in
#' \code{.matrix_to_return} if it is provided.
#' @export
#' @examples
#' engine_eval(x ~ exp(y), y = pi) # ~ 23.14069
#'
#' # It is not currently possible to assign values to a subset of
#' # a matrix in a natural way (e.g. you cannot do things like x[1] = exp(y)),
#' # but you can achieve this functionality using the assign function.
#' engine_eval(dummy ~ assign(x, 1, 0, exp(y))
#'   , dummy = empty_matrix
#'   , x = rep(0, 2)
#'   , y = pi
#'   , .matrix_to_return = "x"
#' )
engine_eval = function(e, ..., .matrix_to_return) {
  dot_mats = list(...)

  if (missing(.matrix_to_return)) {
    output_nm = as.character(e[[2L]])
    output_mat = setNames(list(empty_matrix), output_nm)
    init_mats = c(output_mat, dot_mats, .mats_to_return = output_nm)
  } else {
    output_nm = valid$char1$assert(.matrix_to_return)
    output_mat = setNames(dot_mats[output_nm], output_nm)
    init_mats = c(dot_mats, .mats_to_return = output_nm)
  }

  m = TMBModel(
    init_mats = do.call(MatsList, init_mats),
    expr_list = ExprList(before = list(e)),
    params = OptParamsList(),
    random = OptParamsList(),
    time_steps = Time(0),
    obj_fn = ObjectiveFunction(~0)
  )

  TMBSimulator(m)$matrix(NA, matrix_name = output_nm, time_step = 1L)
}

