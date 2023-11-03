#' Simple Iterated Simulation
#'
#' @param iteration_exprs List of expressions to pass to the
#' [engine](https://canmod.github.io/macpan2/articles/cpp_side). The expressions
#' are only allowed to use valid \code{\link{engine_functions}}.
#' @param time_steps Number of time steps to iterate.
#' @param ... Named list \code{\link{numeric}} objects that can be coerced to
#' \code{\link{numeric}} \code{\link{matrix}} objects. These matrices can be
#' referred to by name in the expressions in `iteration_exprs`.
#'
#' @return A data frame with the simulation results returned by the `$report()`
#' method in \code{\link{TMBSimulator}}.
#'
#' @export
simple_sims = function(iteration_exprs, time_steps, int_vecs = list(), mats = list()) {
  mat_names = names(mats)

  TMBModel(
    init_mats = do.call(MatsList, c(mats, list(.mats_to_return = mat_names, .mats_to_save = mat_names))),
    expr_list = ExprList(during = iteration_exprs),
    engine_methods = EngineMethods(int_vecs = do.call(IntVecs, int_vecs)),
    time_steps = Time(time_steps)
  )$simulator()$report(.phases = c("before", "during"))
}
