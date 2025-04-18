#' Engine Evaluation
#'
#' Evaluate an expression in the TMB-based C++ simulation and objective
#' function engine. This function is useful for trying out the
#' \code{\link{engine_functions}} that can be used to define \pkg{macpan2}
#' models.
#'
#' @param .expr Expression as a one-sided formula, the right-hand-side of which 
#' is treated as the expression to be evaluated.
#' @param ... Named objects that can be coerced into numeric matrices.
#' @param .matrix_to_return Optional name of one of the matrices given
#' in \code{...} to be returned. If this argument is missing, the
#' matrix that will be returned is the matrix returned by the expression on
#' the right-hand-side of the formula.
#' @param .tmb_cpp Name of a \code{C++} program defining the engine. Typically
#' you just want to use the default, which is \code{macpan2}, unless you
#' are extending the
#' [engine](https://canmod.github.io/macpan2/articles/cpp_side.html)
#' yourself.
#' @param .structure_labels Deprecated.
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
#' @importFrom stats as.formula setNames
engine_eval = function(.expr, ..., .matrix_to_return, .tmb_cpp = getOption("macpan2_dll"), .structure_labels = NullLabels()) {
  dot_mats = list(...)
  if (is.numeric(.expr) & any(vapply(dot_mats, inherits, logical(1L), "formula"))) {
    stop("Looks like you used the name `.expr` for one of the numeric objects, which is not allowed when using `engine_eval`. Please try `simple_sims` for similar functionality with better naming rules.")
  }
  e = .expr

  ## force two-sided formula for compliance with TMBSimulator
  ## that is used below
  if (length(e) == 2L) {
    right_hand_side = rhs_char(e)
    left_hand_side = paste0(c("output", names(dot_mats)), collapse = "_")
    e = two_sided(left_hand_side, right_hand_side)
  } else {
    msg(
      "The expression must be given as the",
      "right-hand-side of a one-sided formula."
    ) |> stop()
  }

  if (missing(.matrix_to_return)) .matrix_to_return = left_hand_side

  init_mats = c(
    setNames(list(empty_matrix), left_hand_side),
    dot_mats,
    .mats_to_return = .matrix_to_return
  )

  if (!inherits(.structure_labels, "NullLabels")) {
    component_list = .structure_labels$component_list()
    # list(
    #   state = .structure_labels$state(),
    #   flow = .structure_labels$flow()
    # )
    e = to_special_vecs(e, component_list, c(left_hand_side, names(dot_mats)))
  }

  m = TMBModel(
    init_mats = do.call(MatsList, init_mats),
    expr_list = ExprList(before = list(e)),
    params = OptParamsList(),
    random = OptParamsList(),
    time_steps = Time(0),
    obj_fn = ObjectiveFunction(~0)
  )

  TMBSimulator(m, tmb_cpp = .tmb_cpp)$matrix(NA
    , matrix_name = .matrix_to_return

    # because the number of time steps, T = 0,
    # we have T + 1 = 1 here because we pick up the answers
    # after the simulation loop
    , time_step = 1L

    # even though we evaluate "before", we pick up the
    # answers after because we do not save results
    , .phases = "after"
  )
}


