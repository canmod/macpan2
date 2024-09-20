
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
## 
## distribution objects need to be used for (maybe too many?) things. 
## here are the use cases and the associated information that 
## the calibration machinery needs to get from the distribution objects
##  - trajectory matching 
##    - distr_params used as initial values in an optimization of a distribution parameter
##    - definition of this distribution parameter in the spec used for calibration
##  - 
##  - used to declare that an existing variable will be used as the parameter
##  - used to set the numerical value of the parameter, without optimization
##  

#' Distributional Parameter
#' 
#' Abstract class for a distributional parameter
#' 
#' @param name Generic name of the distributional parameter (e.g., sd, 
#' location, disp)
#' 
#' @noRd
DistrParam = function(name) {
  self = Base()
  self$name = name 
  
  self$variable_name = character(1L)     ## set this when it becomes known
  self$instance_name = character(1L)     ## set this when it becomes known
  self$global_name = character(1L)       ## set this when it becomes known
  self$model_spec = mp_tmb_model_spec()  ## set this when it becomes known
  
  self$update_names = function(name) {
    self$variable_name = name ## e.g., "beta"
    self$instance_name = sprintf("%s_%s", self$name, name) ## e.g., "sd_beta"
    self$global_name = self$instance_name
    self
  }
  self$update_instance_name = function(name) {
    self$instance_name = name
    self
  }
  self$update_global_name = function(name) {
    self$global_name = name
    self
  }
  self$update_model_spec = function(spec) {
    self$model_spec = spec
    self
  }
  
  #' Optimization Context
  #' 
  #' Some decisions cannot be made without knowing more about the optimization 
  #' problem in which this distributional parameter is being used. This
  #' knowledge is provided through the arguments in this method.
  #' 
  #' @param suggested_instance_name A suggestion for the name of this
  #' instance of the distributional parameter with name self$name. An
  #' example of an instance name is `distr_params_sd_I`, which is more
  #' informative than just the name `sd`. In particular, this instance
  #' name says that the variable is a distributional parameter, that it
  #' is a standard deviation parameter, and that it relates to the 
  #' trajectory, `I`. The reason that this is just a suggestion, is that
  #' it may not be necessary at all to provide a name for an instance
  #' of a distributional parameter. For example, if the user provides a 
  #' literal value for the standard deviation such as `1` and asks that this
  #' value not be refined through a fitting process, then it is not necessary
  #' to store this value anywhere and just pass the literal `1` on to the
  #' engine. So in this case, the suggestion for the instance name will be
  #' silently ignored, but this is fine because this is purely an internal
  #' concept and the user should not see any evidence of this complexity.
  #' @param spec A `mp_tmb_model_spec` object to be possibly used to look
  #' for default values of a distributional parameter that already exists.
  #' If the user supplies the parameter through the definition of this
  #' distributional parameter object
  #' @param existing_global_names A character vector of names that already
  #' exist in the `spec` object being used for optimization. These are used
  #' to avoid namespace conflicts (e.g., if the `suggested_instance_name`
  #' happens to be identical to another variable that is already in the
  #' `spec`, this suggestion is modified to avoid conflicts).
  # self$optimization_context = function(
  #       suggested_instance_name = character()
  #     , spec = macpan2::mp_tmb_model_spec()
  #     , existing_global_names = character()
  #   ) {
  #   self$spec = spec
  #   self$update_instance_name(suggested_instance_name)
  #   self
  # }
  # self$optimization_context()
  
  ## list of values to be added as default matrices to a 
  ## mp_tmb_model_spec through a `default` argument
  self$default = function() list(numeric()) |> setNames(self$global_name)
  
  ## character vector giving the how to represent the parameter in
  ## an expression (e.g., literal value, matrix name)
  self$expr_ref = function() character()
  
  self$distr_params_frame = function() macpan2:::empty_frame(c("mat", "row", "col", "default"))
  self$distr_random_frame = function() macpan2:::empty_frame(c("mat", "row", "col", "default"))
  
  return_object(self, "DistrParam")
}
DistrParamNum = function(name, value) {
  self = DistrParam(name)
  self$.value = as.numeric(value)
  if (any(is.na(self$.value))) {
    stop("This distributional parameter must be specified as a number")
  }
  return_object(self, "DistrParamNum")
}
DistrParamNumNoFit = function(name, value) {
  self = DistrParamNum(name, value)
  self$expr_ref = function() self$.value
  self$update_names = function(name) {
    self$variable_name = name
    self
  }
  self$update_instance_name = function(name) self$instance_name = character()
  self$update_global_name = function(name) self$global_name = character()
  return_object(self, "DistrParamNumNoFit")
}
DistrParamNumFit = function(name, value) {
  self = DistrParamNum(name, value)
  self$default = function() {
    list(self$.value) |> setNames(self$global_name)
  }
  self$expr_ref = function() self$global_name
  self$distr_params_frame = function() {
    if (length(self$global_name) != 1L | length(self$default()) != 1L) {
      return(macpan2:::empty_frame(c("mat", "row", "col", "default")))
    }
    mat = self$global_name
    data.frame(
        mat = mat
      , row = 0L, col = 0L
      , default = self$default()[[mat]]
    )
  }
  return_object(self, "DistrParamNumFit")
}
DistrParamChar = function(name, instance_name) {
  self = DistrParam(name)
  self$instance_name = as.character(instance_name)
  self$global_name = instance_name
  self$update_instance_name = function(name) self
  self$update_global_name = function(name) self
  self$update_names = function(name) {
    self$variable_name = name
    self
  }
  self$expr_ref = function() self$global_name
  return_object(self, "DistrParamChar")
}
DistrParamCharFit = function(name, instance_name) {
  self = DistrParamChar(name, instance_name)
  self$default = function() {
    spec_def = self$model_spec$default
    nm = self$global_name
    if (!nm %in% names(spec_def)) {
      stop("spec does not contain the required variable containing this distribution parameter")
    }
    list(spec_def[[nm]]) |> setNames(nm)
  }
  self$distr_params_frame = function() {
    mat = self$global_name
    data.frame(
        mat = mat
      , row = 0L, col = 0L
      , default = self$default()[[mat]]
    )
  }
  return_object(self, "DistrParamCharNoFit")
}
DistrParamCharNoFit = function(name, instance_name) {
  self = DistrParamChar(name, instance_name)
  self$default = function() {
    nm = self$instance_name
    list(self$model_spec$default[[nm]]) |> setNames(nm)
  }
  return_object(self, "DistrParamCharNoFit")
}


# 
# mp_tmb_calibrator(spec, traj = list(reports = mp_neg_bin(disp = DistrParamNoFitNum("disp", 1))))
# ## obj_fn term : dnbinom(reports, sim_reports, 1)
# 
# mp_tmb_calibrator(spec, traj = list(reports = mp_neg_bin(disp = no_fit("disp", "disp_par"))))
# ## obj_fn term : dnbinom(reports, sim_reports, disp_par)
# 
# mp_tmb_calibrator(spec, traj = list(reports = mp_neg_bin(disp = 1)))
# ## name for new_var
# ## update spec with new_var
# ## obj_fn term : dnbinom(reports, sim_reports, new_var)
# ## add new_var to opt_params with new_var = 1
# 
# mp_tmb_calibrator(spec, traj = list(reports = mp_neg_bin(disp = "disp_par")))
# ## obj_fn term : dnbinom(reports, sim_reports, disp_par)
# ## add disp_par to opt_params
# 
# mp_tmb_calibrator(spec, par = list(beta = mp_log_normal(location = 0.25, sd = 1)))
# ## prior term : dnorm(log(beta), log(0.25), 1)
# 
# mp_tmb_calibrator(spec, par = list(beta = mp_log_normal(location = "beta_loc", sd = "beta_sd")))
# ## prior term : dnorm(log(beta), log(beta_loc), beta_sd)
# 
# 
# ll = list(reports = mp_neg_bin(disp = 1), W = mp_normal(sd = 1))
# names(ll)
# oor::method_apply(ll, "distr_params")
# 

#' Distribution Specification
#' 
#' Class representing the specification of a distribution, either a prior or
#' likelihood component.
DistrSpec = function() {
  self = Base()
  
  self$update_variable_name = function(name = "variable") {
    self$variable_name = name
    for (o in self$distr_param_objs) o$update_names(name)
    self
  }
  self$update_model_spec = function(spec) {
    for (o in self$distr_param_objs) o$update_model_spec(spec)
    self
  }
  
  self$instance_names = function() {
    dpo = self$distr_param_objs
    lapply(dpo, getElement, "instance_name") |> setNames(names(dpo))
  }
  
  #' List of DistrParam objects defining the distribution. All distribution
  #' must have a `location` parameter as the first parameter when the
  #' distribution is instantiated. However, this `location` parameter 
  #' might be removed by the `remove_location_parameter` method, which
  #' should be called later if it is determined that the distribution does
  #' not require a `location` parameter.
  self$distr_param_objs = list()
  
  self$distr_params = function() {
    dpo = self$distr_param_objs
    dp = list()
    for (o in dpo) dp = append(dp, o$default())
    dp
  }
  
  #' character string giving an expression for the component of a log prior
  #' density associated with this distribution. this may not be used if
  #' it is intended to be used as a likelihood component.
  self$prior = \() character()
  
  #' character string giving an expression for the component of a log likelihood
  #' function associated with this distribution. this may not be used if
  #' it is intended to be used as a prior distribution component.
  self$likelihood = \() character()
  
  #' data frame characterizing what distributional parameters associated
  #' with this distribution should be optimized as fixed effects.
  self$distr_params_frame = function() {
    (self$distr_param_objs
      |> oor::method_apply("distr_params_frame")
      |> macpan2:::bind_rows()
    )
  }
  
  #' data frame characterizing what distributional parameters associated
  #' with this distribution should be optimized as random effects.
  self$distr_random_frame = function() {
    (self$distr_param_objs
      |> oor::method_apply("distr_random_frame")
      |> macpan2:::bind_rows()
    )
  }
  
  #' method to remove the location parameter, and therefore make it impossible
  #' to use this distribution as the component of a prior distribution.
  #' this method is useful when the distribution is to be used as a likelihood
  #' component that will take a simulated trajectory as the location parameter.
  self$remove_location_parameter = function() {
    self$distr_param_objs$location = NULL
    ## TODO: better more informative error message
    self$prior = function() stop("this distribution has had its location parameter removed and therefore cannot be used to compute a prior -- only a likelihood")
  }
  
  return_object(self, "DistrSpec")
}

TESTDISTR = function(location, sd) {
  self = DistrSpec()
  self$distr_param_objs = macpan2:::nlist(location, sd)

  self$prior = \(par) {
    sprintf("-sum(dnorm(%s, %s, %s))"
      , par
      , self$distr_param_objs$location$expr_ref()
      , self$distr_param_objs$sd$expr_ref()
    )
  }
  self$likelihood = \(obs, sim) {
    sprintf("-sum(dnorm(%s, %s, %s))"
      , obs
      , sim
      , self$distr_param_objs$sd$expr_ref()
    )
  }
  return_object(self, "Normal")
}


#' Poisson Distribution
#' @export
mp_poisson = function(location) {
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
