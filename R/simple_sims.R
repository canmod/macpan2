#' Simple Iterated Simulation
#'
#' @param iteration_exprs List of expressions to pass to the
#' [engine](https://canmod.github.io/macpan2/articles/cpp_side). The expressions
#' are only allowed to use valid \code{\link{engine_functions}}. Each expression
#' is evaluated in order, once for each iteration. The number of iterations is
#' controlled by the \code{time_steps} argument.
#' @param time_steps Number of time steps to iterate.
#' @param int_vecs Named list of integer vectors.
#' @param mats Named list of matrices.
#'
#' @return A data frame with the simulation results.
#'
#' @export
simple_sims = function(iteration_exprs, time_steps, int_vecs = list(), mats = list()) {
  
  mat_names = names(mats)

  TMBModel(
    init_mats = do.call(MatsList, c(mats, list(.mats_to_return = mat_names, .mats_to_save = mat_names))),
    expr_list = ExprList(during = iteration_exprs),
    engine_methods = EngineMethods(int_vecs = do.call(IntVecs, int_vecs)),
    time_steps = Time(time_steps)
  )$simulator()$report()
}
