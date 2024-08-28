## construct objects to pass to the par argument of mp_tmb_calibrator

mp_par = function(param, random) {
  arg = list()
  
  structure(arg, class = "ParArg")
}

## construct objects to pass to the tv argument of mp_tmb_calibrator

#' @param parameters List of time-variation specifications for parameters.
mp_tv = function(parameters) {
  arg = list()
  arg$parameters = parameters
  structure(arg, class = "TVArg")
}

#' Fit a Time-Varying Parameter with Radial Basis Functions
#' 
#' Pass the output of this function to the `tv` argument of 
#' \code{\link{mp_tmb_calibrator}} to model time variation of 
#' a parameter with flexible radial basis functions.
#' 
#' @param tv String giving the name of the parameter.
#' @param dimension Number of bases.
#' @param initial_weights Optional vector with `dimensions` elements. These
#' are the parameters that are fitted and determine how `tv` varies with
#' time.
#' @param seed Optional random seed to use to generate the `initial_weights`
#' if they are not provided.
#' @param sparse_tol Tolerance below which radial basis function outputs
#' are set exactly to zero. Small values are more accurate but slower. Lack
#' of accuracy can be visually apparent as jumps in graphs of the time-varying
#' parameter.
#' 
#' @export
mp_rbf = function(tv, dimension, initial_weights, seed = 1L, sparse_tol = 1e-2) {
  if (missing(initial_weights)) {
    set.seed(seed)
    initial_weights = rnorm(dimension, sd = 0.01)
  }
  if (length(initial_weights) != dimension) {
    stop("The `initial_weights` vector must be of length `dimension`.")
  }
  arg = list()
  arg$tv = tv
  arg$dimension = dimension
  arg$initial_weights = initial_weights
  arg$sparse_tol = sparse_tol
  structure(arg, class = "RBFArg")
}




## construct objects to pass to the traj argument of mp_tmb_calibrator

#' Trajectory Specification
#' 
#' Specify a set of trajectories to fit. The output of this function
#' is intended to be passed to the `traj` argument of
#' \code{\link{mp_tmb_calibrator}}.
#' 
#' @param likelihood List of likelihood components. The names of the list
#' identify the trajectory associated with each likelihood component.
#' @param condensation List of condensation methods. The names of the list
#' identify the trajectories produced by each condensation method.
#' @export
mp_traj = function(
      likelihood = list()
    , condensation = list()
  ) {
  arg = list()
  arg$likelihood = likelihood
  arg$condensation = condensation
  structure(arg, class = "TrajArg")
}
