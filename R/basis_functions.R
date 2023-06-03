#' Radial Basis Functions
#'
#' @param time_steps number of time steps in the model
#' @param dimension number of gaussians in the basis
#' @param scale width of the gaussians
#'
#' @export
rbf = function(time_steps, dimension, scale = time_steps / dimension) {
  s = scale
  make_gaussian = \(m)\(x)exp(-((x-m)^2)/(2*s^2))
  locations = seq(from = 0, to = time_steps + 0, length = dimension)
  gaussians = lapply(locations, make_gaussian)
  do.call(cbind, lapply(gaussians, do.call, list(0:time_steps)))
}
