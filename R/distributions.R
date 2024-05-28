
## TODO: currently this is only used in likelihood functions.
## how to re-purpose this for priors? the only thing that makes it a 
## no-brainer is just that priors need a location parameter, and then
## what if these are hyperparameters with their own priors in a
## hierarchical model?

# TODO: mp_clamped_poisson, mp_sum_of_squares

#' @export
mp_poisson = function() {
  self = Base()
  self$distr_params = \() list()
  self$expr_char = \(x, location) sprintf("-sum(dpois(%s, %s))", x, location)
  return_object(self, "Poisson")
}

#' @export
mp_neg_bin = function(disp) {
  self = Base()
  self$disp = disp
  self$log_disp = \() log(self$disp)
  self$distr_params = \() list(log_disp = self$log_disp())
  self$expr_char = \(x, location, log_disp) {
    sprintf("-sum(dnbinom(%s, clamp(%s), exp(%s)))", x, location, log_disp)
  }
  return_object(self, "NegBin")
}

#' @export
mp_normal = function(sd) {
  self = Base()
  self$sd = sd
  self$log_sd = \() log(self$sd)
  self$distr_params = \() list(log_sd = self$log_sd())
  self$expr_char = \(x, location, log_sd) {
    sprintf("-sum(dnorm(%s, %s, exp(%s)))", x, location, log_sd)
  }
  return_object(self, "Normal")
}

#' @export
mp_log_normal = function(sd) {
  self = mp_normal(sd)
  self$expr_char = \(x, location, log_sd) {
    sprintf("-sum(dnorm(log(%s), log(%s), exp(%s)))", x, location, log_sd)
  }
  return_object(self, "NogNormal")
}
