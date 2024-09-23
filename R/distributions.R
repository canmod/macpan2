
## design brainstorming notes
## 
## this experimental class is an attempt to solve the above problem.
## we have the concept of a spec with defaults, much like the concept
## of a model spec. the defaults can then be used when required 
## (e.g., use the default location for a prior) but doesn't have to
## when the distribution spec is used for a likelihood. the thing i
## don't like about this is that the user might expect the default
## to be more than a default, and assume that the default value gets
## used no matter what.
## 
## distribution objects need to be used for (maybe too many?) things. 
## here are the use cases and the associated information that 
## the calibration machinery needs to get from the distribution objects
##  - trajectory matching 
##    - distr_params used as initial values in an optimization of a distribution parameter
##    - definition of this distribution parameter in the spec used for calibration
##  - used to declare that an existing variable will be used as the parameter
##  - used to set the numerical value of the parameter, without optimization


#' Distributional Parameter
#' 
#' Abstract class for a distributional parameter
#' 
#' @param name Generic name of the distributional parameter (e.g., sd, 
#' location, disp)
#' 
#' @section Optimization Context
#' 
#' Some decisions cannot be made without knowing more about the optimization 
#' problem in which this distributional parameter is being used. This
#' knowledge is provided through the arguments in this method.
#' 
#' @noRd
DistrParam = function(generic_name) {
  self = Base()
  
  # Part A: Required Fields
  
  # Generic name of the distributional parameter (e.g., sd)
  self$generic_name = generic_name
  
  # Name of the variable being modelled by the distribution containing this
  # distributional parmaeter (e.g., beta has this prior)
  self$variable_name = character(1L)
  
  # Name of the instance of this distributional parameter (e.g., sd_beta)
  # This name is is more informative than either the generic name (e.g., `sd`)
  # or the variable name (e.g., `beta`)
  self$instance_name = character(1L)
  
  # Instance name that has potentially been modified so that it can be
  # added to a model spec without clashing with exiseting names
  # (e.g., distr_params_sd_beta_1)
  self$global_name = character(1L)
  
  # Model specification for which this distribution parameter is beig applied
  self$model_spec = mp_tmb_model_spec()
  
  
  # Part B: Boilerplate -- no need to override these in extensions of this class
  
  # section 1: update fields when more context is available
  
  self$update_global_name = function(name) {
    self$global_name = name
    self
  }
  self$update_names = function(name) {
    self$variable_name = name ## e.g., "beta"
    self$instance_name = sprintf("%s_%s", self$generic_name, self$variable_name)
    self$global_name = self$instance_name
    self
  }
  self$update_model_spec = function(spec) {
    self$model_spec = spec
    self
  }
  
  
  # Part C: Abstract -- implement these methods in extensions of this class
  # to produce new types of distributional parameters
  
  ## section 2: distributional parameters that need to be added as 
  ## _new_ defaults to model spec to be updated by calibration machinery
  
  ## section 2a: list of values to be added as default matrices to a 
  ## mp_tmb_model_spec through a `default` argument. this list is either
  ## length 0 (if this distribuytional parameter does not have to be added
  ## to the model spec) or length 1 (if it does). any other length is
  ## invalid
  self$default = function() list()
  
  ## section 1b: list parallel to self$default() containing the objects
  ## that represent each distributional parameter
  self$default_objs = function() list()
  
  
  ## section 3: how should each distributional parameter be represented in
  ## the expression for the distribution density function (and maybe soon
  ## random number generators)
  
  ## character vector giving the how to represent the parameter in
  ## an expression (e.g., literal value, matrix name)
  self$expr_ref = function() character()
  
  
  ## section 4: what distributional parameters should be fitted by the
  ## calibration machinery?
  
  self$distr_params_frame = function() macpan2:::empty_frame(c("mat", "row", "col", "default"))
  self$distr_random_frame = function() macpan2:::empty_frame(c("mat", "row", "col", "default"))
  
  return_object(self, "DistrParam")
}

#' Distribution Specification
#' 
#' Class representing the specification of a distribution, either a prior or
#' likelihood component.
#' 
#' Extend this class to develop new distributions.
DistrSpec = function() {
  
  # Part A: Boilerplate -- no need to override these in extensions of this class
  
  self = Base()
  
  ## section 1: get context from the distribution list, which includes
  ## the model spec
  
  # accept the variable name from the DistrList and propagate it up to
  # the DistrParam objects
  self$update_variable_name = function(name = "variable") {
    self$variable_name = name
    for (o in self$distr_param_objs) o$update_names(name)
    self
  }
  self$update_model_spec = function(spec) {
    for (o in self$distr_param_objs) o$update_model_spec(spec)
    self
  }
  # remove the location parameter, and therefore make it impossible
  # to use this distribution as the component of a prior distribution.
  # this method is useful when the distribution is to be used as a likelihood
  # component that will take a simulated trajectory as the location parameter.
  self$remove_location_parameter = function() {
    self$distr_param_objs$location = NULL
    ## TODO: better more informative error message
    self$prior = function(par) stop("this distribution has had its location parameter removed and therefore cannot be used to compute a prior -- only a likelihood")
  }
  
  
  ## section 2: distributional parameters that need to be added as 
  ## _new_ defaults to model spec to be updated by calibration machinery
  
  self$default = function() {
    dpo = self$distr_param_objs
    dp = list()
    for (o in dpo) dp = append(dp, o$default())
    dp
  }
  
  self$default_objs = function() {
    dpo = self$distr_param_objs
    dp = list()
    for (o in dpo) dp = append(dp, o$default_objs())
    dp
  }
  
  
  ## section 5: what distributional parameters should be fitted by the
  ## calibration machinery?
  
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
  
  
  
  # Part B: Abstract -- implement these methods when extending this class
  # to produce new distributions.
  
  ## section 4: generate strings for expressions that will be used by
  ## the engine as expressions. this, along with the distr_param_objs,
  ## is what needs to be defined to make a new distribution.
  
  #' character string giving an expression for the component of a log prior
  #' density associated with this distribution. this may not be used if
  #' it is intended to be used as a likelihood component.
  self$prior = \(par) character()
  
  #' character string giving an expression for the component of a log likelihood
  #' function associated with this distribution. this may not be used if
  #' it is intended to be used as a prior distribution component.
  self$likelihood = \(obs, sim) character()
  
  #' for the future (e.g. beta ~ rnorm(0, 1))
  self$noisy_parameter = \() character()
  self$noisy_trajectory = \(sim) character()
  
  #' These functions require one field that must be in every distribution 
  #' that inherits from DistrSpec. This field is called `distr_param_objs`,
  #' which is a list of DistrParam objects defining the distribution. All 
  #' distributions must have a `location` parameter as the first parameter when 
  #' the distribution is instantiated. However, this `location` parameter 
  #' might be removed by the `remove_location_parameter` method, which
  #' should be called later if it is determined that the distribution does
  #' not require a `location` parameter (e.g., it is a likelihood component
  #' that takes the simulated tracjectory as the location).
  self$distr_param_objs = list()
  
  return_object(self, "DistrSpec")
}

#' Distribution List
#' 
#' List of distribution specifications, combined with a model specification.
#' These objects contain all of the information required by calibrators.
#' It is probably not necessary to extend this class, as it is mostly
#' (1) an aggregator of DistrSpec objects and (2) propagates information
#' from the model spec and calibration machinery back to the DistrSpec and
#' DistrParam objects.
#' 
#' @param distr_list Named list of DistrSpec objects that the user (or 
#' interface) passes to mp_tmb_calibrator.
#' @param model_spec Model spec object to be calibrated to data that the 
#' user passed to mp_tmb_calibrator.
#' 
#' @noRd
DistrList = function(distr_list = list(), model_spec = mp_tmb_model_spec()) {
  
  # get context from the interface
  
  for (nm in names(distr_list)) distr_list[[nm]]$update_variable_name(nm)
  for (nm in names(distr_list)) distr_list[[nm]]$update_model_spec(model_spec)
  
  # initialize the object
  
  self = Base()
  self$distr_list = distr_list
  self$model_spec = model_spec
  
  
  # section 1: get more context from the calibration problem
  
  self$remove_location_parameters = function() {
    for (o in self$distr_list) o$remove_location_parameter()
  }
  
  #' @param obj An object with an `obj$distr_params()` method returning a flat list
  #' of named numerical matrices, and an `obj$global_names()` method returning
  #' the list of names that need to be avoided in namespace clashes
  self$update_global_names = function(obj) {
    nms = names(globalize(obj, "distr_params"))
    default_objs = method_apply(self$distr_list, "default_objs") |> Reduce(f = "c")
    ## assume length(nms) == length(default_objs)
    for (i in seq_along(nms)) default_objs[[i]]$update_global_name(nms[i])
  }
  
  
  # section 2: distributional parameters that need to be added as 
  ## _new_ defaults to model spec to be updated by calibration machinery
  
  self$default = function() {
    method_apply(self$distr_list, "default") |> Reduce(f = "c")
  }
  
  
  # section 3: what distributional parameters should be fitted by the
  ## calibration machinery?

  self$distr_params_frame = function() {
    (self$distr_list
      |> method_apply("distr_params_frame")
      |> bind_rows()
      |> rename_synonyms(mat = "matrix", default = "value")
    )
  }
  
  return_object(self, "DistrList")
}

DistrParamNum = function(generic_name, value) {
  self = DistrParam(generic_name)
  self$.value = as.numeric(value)
  if (any(is.na(self$.value))) {
    stop("This distributional parameter must be specified as a number")
  }
  return_object(self, "DistrParamNum")
}
DistrParamNumNoFit = function(name, value) {
  self = DistrParamNum(name, value)
  self$expr_ref = function() as.character(self$.value)
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
    list(self$.value) |> setNames(self$instance_name)
  }
  self$default_objs = function() list(self) |> setNames(self$instance_name)
  self$expr_ref = function() self$global_name
  self$distr_params_frame = function() {
    if (length(self$global_name) != 1L | length(self$default()) != 1L) {
      return(macpan2:::empty_frame(c("mat", "row", "col", "default")))
    }
    global_mat_name = self$global_name
    local_mat_name = self$instance_name
    data.frame(
        mat = global_mat_name
      , row = 0L, col = 0L
      , default = self$default()[[local_mat_name]]
    )
  }
  return_object(self, "DistrParamNumFit")
}

DistrParamChar = function(name, instance_name) {
  self = DistrParam(name)
  self$instance_name = as.character(instance_name)
  self$global_name = instance_name
  self$update_instance_name = function(name) self  ## these should probably be the abstract version
  self$update_global_name = function(name) self
  self$update_names = function(name) {
    self$variable_name = name
    self
  }
  self$expr_ref = function() self$global_name
  self$check_in_spec = function() {
    if (!self$global_name %in% names(self$model_spec$default)) {
      stop(self$global_name, " is not in the model spec")
    }
  }
  return_object(self, "DistrParamChar")
}
DistrParamCharFit = function(name, instance_name) {
  self = DistrParamChar(name, instance_name)
  self$distr_params_frame = function() {
    self$check_in_spec()
    mat = self$global_name
    data.frame(
        mat = mat
      , row = 0L, col = 0L
      , default = self$model_spec$default[[mat]]
    )
  }
  return_object(self, "DistrParamCharNoFit")
}
DistrParamCharNoFit = function(name, instance_name) {
  self = DistrParamChar(name, instance_name)
  self$default = function() {
    self$check_in_spec()
    list()
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
