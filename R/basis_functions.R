make_gaussian = \(s)\(m)\(x)exp(-((x-m)^2)/(2*s^2))
make_locations = \(t)\(d)seq(from=0,to=t-1,length=d)

# g = (ss
#   lapply(make_gaussian)
#   mapply(FUN = \(f, x) lapply(x, f), mm, SIMPLIFY = FALSE)
#   unlist(recursive = FALSE)
#   lapply(do.call, args = list(tt))
# )

rbf_base = function(times, locations, scales) {
  gaussians = lapply(locations, make_gaussian(scales))
  gaussian_outputs = lapply(gaussians, do.call, list(times))
  do.call(cbind, gaussian_outputs)
}


#' Radial Basis Functions
#'
#' Compute a set of radial basis functions (`dimension` of them).
#'
#' @param time_steps number of time steps in the model
#' @param dimension number of gaussians in the basis
#' @param scale width of the gaussians
#'
#' @examples
#' matplot(rbf(100, 5), type = "l")
#'
#' @export
rbf = function(time_steps, dimension, scale = time_steps / dimension) {
  locations = make_locations(time_steps)(dimension)
  times = seq_len(time_steps) - 1L
  rbf_base(times, locations, scale)
}


## experimental
rbf_heterogeneous = function(time_steps, locations, scales) {
  times = seq_len(time_steps) - 1L
  rbf_base(times, locations, scales)
}
