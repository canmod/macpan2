#' Engine Evaluation
#'
#' Evaluate an expression in the TMB-based C++ simulation and objective
#' function engine. This function is useful for trying out the
#' \code{\link{engine_functions}} that can be used to define \pkg{macpan2}
#' models.
#'
#' @param e Expression as a one-sided formula, the right-hand-side of which is
#' treated as the expression to be evaluated.
#' @param ... Named objects that can be coerced into numeric matrices.
#' @param .matrix_to_return Optional name of one of the matrices given
#' in \code{...} to be returned. If this argument is missing, the
#' matrix that will be returned is the matrix returned by the expression on
#' the right-hand-side of the formula.
#'
#' @return Matrix being produced on the right-hand-side or matrix given in
#' \code{.matrix_to_return} if it is provided.
#' @export
#' @examples
#' engine_eval(~ exp(y), y = pi) # ~ 23.14069
#'
#' # It is not currently possible to assign values to a subset of
#' # a matrix in a natural way (e.g. you cannot do things like x[1] = exp(y)),
#' # but you can achieve this functionality using the assign function.
#' engine_eval(~ assign(x, 1, 0, exp(y))
#'   , x = rep(0, 2)
#'   , y = pi
#'   , .matrix_to_return = "x"
#' )
engine_eval = function(e, ..., .matrix_to_return) {
  dot_mats = list(...)

  ## force two-sided formula for compliance with TMBSimulator
  ## that is used below
  if (length(e) == 2L) {
    left_hand_side = paste0(c("output", names(dot_mats)), collapse = "_")
    e = as.formula(paste0(c(left_hand_side, as.character(e)), collapse = " "))
  } else {
    stop(
      "\nThe expression must be given as the",
      "\nright-hand-side of a one-sided formula."
    )
  }

  if (missing(.matrix_to_return)) .matrix_to_return = left_hand_side

  init_mats = c(
    setNames(list(empty_matrix), left_hand_side),
    dot_mats,
    .mats_to_return = .matrix_to_return
  )

  m = TMBModel(
    init_mats = do.call(MatsList, init_mats),
    expr_list = ExprList(before = list(e)),
    params = OptParamsList(),
    random = OptParamsList(),
    time_steps = Time(0),
    obj_fn = ObjectiveFunction(~0)
  )

  TMBSimulator(m)$matrix(NA, matrix_name = .matrix_to_return, time_step = 1L)
}
