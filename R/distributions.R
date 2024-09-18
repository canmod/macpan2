
## TODO: currently this is only used in likelihood functions.
## how to re-purpose this for priors? the only thing that makes it a 
## no-brainer is just that priors need a location parameter, and then
## what if these are hyperparameters with their own priors in a
## hierarchical model?

## this experimental class is an attempt to solve the above problem.
## we have the concept of a spec with defaults, much like the concept
## of a model spec. the defaults can then be used when required 
## (e.g., use the default location for a prior) but doesn't have to
## when the distribution spec is used for a likelihood. the thing i
## don't like about this is that the user might expect the default
## to be more than a default, and assume that the default value gets
## used no matter what. perhaps it will be better to have a class
## that can take all of the parameters (e.g. location, scale/dispersion
## parameters) in one of three forms: 
##   1. numeric
##   2. character (interpreted as an expression)
##   3. class with functions that are chosen for use
##      depending on context (e.g., as a likelihood 
##      component, as a prior)
## the default will be #3
DistrSpec = function(...) {
  args = capture_args(...)
  self = Base()
  self$default = args
  self$distr_params = \() list()
  self$prior = \(variable) character()
  self$likelihood = \(variable) character()
  return_object(self, "DistrSpec")
}
capture_args = function(f) {
  print(ls())
  force(as.list(environment()))
}
capture_args(f = 1)
xx = DistrSpec(f = 1)
xx$default
mp_normal(location = 0, sd = mp_log_normal())

#' Uniform Distribution (Improper)
#' @export
mp_uniform = function() {
  self = DistrSpec() # not sure if this should be Base()
  # improper uniform have no distribution parameters
  self$distr_params = \() list()
  # sum of negative log-likelihoods 
  # equal to log(1) = 0 so setting to empty character
  self$expr_char = \(x) character()
  return_object(self, "Uniform")
}

#' Poisson Distribution
#' @export
mp_poisson = function() {
  self = DistrSpec()
  self$distr_params = \() list()
  self$expr_char = \(x, location) sprintf("-sum(dpois(%s, %s))", x, location)
  return_object(self, "Poisson")
}

#' Negative Binomial Distribution
#' @param disp Dispersion parameter.
#' @export
mp_neg_bin = function(disp) {
  ## location??
  self = Base()
  self$disp = disp
  self$log_disp = \() log(self$disp)
  self$distr_params = \() list(log_disp = self$log_disp())
  self$expr_char = \(x, location, log_disp) {
    sprintf("-sum(dnbinom(%s, clamp(%s), exp(%s)))", x, location, log_disp)
  }
  return_object(self, "NegBin")
}


#' Normal Distributon
#' @param location Location parameter. Only necessary if used as a prior
#' distribution. If it is used as a likelihood component the location
#' parameter will be taken as the simulated variable being fitted to data,
#' and so this `location` parameter should be left `NULL`.
#' @param sd Standard deviation parameter.
#' @export
mp_normal = function(location = NULL, sd = 1) {
  self = Base()
  self$sd = sd
  self$log_sd = \() log(self$sd)
  self$location = location
  self$distr_params = \() list(log_sd = self$log_sd())
  self$expr_char = \(x, location, log_sd) {
    sprintf("-sum(dnorm(%s, %s, exp(%s)))", x, location, log_sd)
  }
  self$expr_char_prior = function(x) {
    if (is.null(location)) stop("please specify a ")
    self$expr_char(x, self$location, self$log_sd())
  }
  return_object(self, "Normal")
}

mp_normal_test = function(x, location, log_sd) {
  self = Base()
  self$expr_char = \(x, location, log_sd) {
    sprintf("-sum(dnorm(%s, %s, exp(%s)))", x, location, log_sd)
  }
  return_object(self, "NormalTest")
}

#' Log-Normal Distribution
#' @param sd Standard deviation parameter.
#' @export
mp_log_normal = function(sd) {
  self = mp_normal(sd)
  self$expr_char = \(x, location, log_sd) {
    sprintf("-sum(dnorm(log(%s), log(%s), exp(%s)))", x, location, log_sd)
  }
  return_object(self, "LogNormal")
}

# TODO: mp_clamped_poisson, mp_sum_of_squares
