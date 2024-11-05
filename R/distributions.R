
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
#' @section Optimization Context:
#' 
#' Some decisions cannot be made without knowing more about the optimization 
#' problem in which this distributional parameter is being used. This
#' knowledge is provided through the arguments in this method.
#' 
#' @noRd
DistrParam = function(generic_name, trans = DistrParamTransDefault()) {
  self = Base()
  self$trans = trans
  
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
    self$variable_name = name ## e.g., "beta", or maybe "log_beta"
    self$instance_name = self$trans$nm(sprintf("%s_%s", self$generic_name, self$variable_name))
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
  ## length 0 (if this distributional parameter does not have to be added
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
  
  self$distr_params_frame = function() empty_frame(c("mat", "row", "col", "default"))
  self$distr_random_frame = function() empty_frame(c("mat", "row", "col", "default"))
  
  return_object(self, "DistrParam")
}


#' Distribution Specification
#' 
#' Class representing the specification of a distribution, either a prior or
#' likelihood component. Extend this class to develop new distributions.
#' 
#' @param distr_param_objs List of DistrParam objects defining the distribution. All 
#' distributions must have a `location` parameter as the first parameter when 
#' the distribution is instantiated. However, this `location` parameter 
#' might be removed by the `remove_location_parameter` method, which
#' should be called later if it is determined that the distribution does
#' not require a `location` parameter (e.g., it is a likelihood component
#' that takes the simulated trajectory as the location).
#' @param default_trans list of default parameter transformation objects
#' of class `DistrParamTrans` for each `distr_param_objs`.
#' 
#' @noRd
DistrSpec = function(distr_param_objs = list(), default_trans = list()) {
  for (nm in names(distr_param_objs)) distr_param_objs[[nm]] = to_distr_param(distr_param_objs[[nm]])
  for (nm in names(distr_param_objs)) distr_param_objs[[nm]]$generic_name = nm
  for (nm in names(default_trans)) {
    distr_param_objs[[nm]]$trans = distr_param_objs[[nm]]$trans$resolve_default(default_trans[[nm]])
  }
  
  # Part A: Boilerplate -- no need to override these in extensions of this class
  
  self = Base()
  self$distr_param_objs = distr_param_objs
  self$default_trans = default_trans
  
  ## section 0: check distributional parameter objects
  
  # This may not be needed in the future, but currently distributions
  # can only accept single distributional parameters. We hope to be able
  # to incorporate vector distributional parameters to allow a vector 
  # variable to have common a distribution and different distributional
  # parameters for each vector component.
  self$check_args = function(distr_param_objs){
    nms = names(distr_param_objs)
    for (nm in nms) {
      if (length(distr_param_objs[[nm]]$expr_ref()) > 1) {
        stop("Expression given for the distributional parameter, ", nm, ", for the variable, " , distr_param_objs[[nm]]$variable_name, ", has more than one element.")
      }
    }
  }
  
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
    self$prior = function(par) stop("this distribution has had its location parameter removed and therefore cannot be used to compute a prior -- only a likelihood.")
  }
  self$has_location = function() {
    loc = self$distr_param_objs$location
    return(!inherits(loc, "DistrParamNull"))
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
  
  # data frame characterizing what distributional parameters associated
  # with this distribution should be optimized as fixed effects.
  self$distr_params_frame = function() {
    (self$distr_param_objs
      |> oor::method_apply("distr_params_frame")
      |> bind_rows()
    )
  }
  
  # data frame characterizing what distributional parameters associated
  # with this distribution should be optimized as random effects.
  self$distr_random_frame = function() {
    (self$distr_param_objs
      |> oor::method_apply("distr_random_frame")
      |> bind_rows()
    )
  }
  
  
  # Part B: Abstract -- implement these methods when extending this class
  # to produce new distributions.
  
  ## section 4: generate strings for expressions that will be used by
  ## the engine as expressions. this, along with the distr_param_objs,
  ## is what needs to be defined to make a new distribution.
  
  # character string giving an expression for the component of a log prior
  # density associated with this distribution. this may not be used if
  # it is intended to be used as a likelihood component.
  self$prior = \(par) character()
  
  # character string giving an expression for the component of a log likelihood
  # function associated with this distribution. this may not be used if
  # it is intended to be used as a prior distribution component.
  self$likelihood = \(obs, sim) character()
  
  # for the future (e.g. beta ~ rnorm(0, 1))
  self$noisy_parameter = \() character()
  self$noisy_trajectory = \(sim) character()
  
  
  # Part C: Assumption Checking
  
  # section 5: check if the variable meets the assumptions of the distribution,
  # at least at the start of the simulation (i.e., in the defaults or data).
  # here the variable argument is the numeric value that the variable will
  # take at the start of the simulation. note that if the spec modifies this
  # variable before the objective function is evaluated, this check might not
  # be correct. so these checks should always just throw warnings and not
  # errors. the user experience enhancement will be to provide context for
  # why the objective function or gradient returns NaNs sometimes.
  self$check_variable = function(variable) NULL
  
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
  
  ## This class is all boilerplate and none of the methods should be thought
  ## of as abstract, meaning that it is not meant to be extended by adding a
  ## child class that adds new functionality.
  
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
  self$error_if_not_all_have_location = function() {
    x = method_apply(self$distr_list, "has_location") |> setNames(names(self$distr_list))
    all_have = x |> unlist(use.names = TRUE) |> all()
    if (!all_have) {
      bads = names(x)[!unlist(x)]
      msg = sprintf(
          "\nThe following distributions do not have location parameters:\n%s"
        , paste(bads, collapse = "   \n")
      )
      stop(msg)
    }
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
  
  # section 4: check variable assumptions
  
  self$check_variables = function(data_list) {
    vnms = names(self$distr_list) ## all variable names
    for (nm in vnms) {
      self$distr_list[[nm]]$check_variable(data_list[[nm]]$value)
    }
    NULL
  }
  
  return_object(self, "DistrList")
}

mp_distr_param_null = function(generic_name) DistrParamNull(generic_name)
DistrParamNull = function(generic_name) {
  self = DistrParam(generic_name, DistrParamTrans())
  # self$err = function() {
  #   if (self$variable_name == "") {
  #     msg = sprintf("A %s parameter for some distribution is not provided"
  #       , self$generic_name
  #     )
  #   } else {
  #     msg = sprintf("A %s parameter for a distribution on %s is not provided"
  #       , self$generic_name
  #       , self$variable_name
  #     )
  #   }
  #   msg
  # }
  # self$default = function() stop(self$err())
  # self$default_objs = function() stop(self$err())
  # self$expr_ref = function() stop(self$err())
  return_object(self, "DistrParamNull")
}
DistrParamNum = function(generic_name, value, trans = DistrParamTrans()) {
  self = DistrParam(generic_name, trans)
  self$.value = as.numeric(value)
  if (any(is.na(self$.value))) {
    stop("This distributional parameter must be specified as a number")
  }
  return_object(self, "DistrParamNum")
}
DistrParamNumNoFit = function(name, value, trans = DistrParamTrans()) {
  self = DistrParamNum(name, value, trans)
  self$expr_ref = function() self$trans$ref(as.character(self$.value))
  self$update_names = function(name) {
    self$variable_name = name
    self
  }
  self$update_instance_name = function(name) self$instance_name = character()
  self$update_global_name = function(name) self$global_name = character()
  return_object(self, "DistrParamNumNoFit")
}
DistrParamNumFit = function(name, value, trans = DistrParamTrans()) {
  self = DistrParamNum(name, value, trans)
  self$default = function() {
    list(self$trans$val(self$.value)) |> setNames(self$instance_name)
  }
  self$default_objs = function() list(self) |> setNames(self$instance_name)
  self$expr_ref = function() self$trans$ref(self$global_name)
  self$distr_params_frame = function() {
    if (length(self$global_name) != 1L | length(self$default()) != 1L) {
      return(empty_frame(c("mat", "row", "col", "default")))
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

DistrParamChar = function(name, instance_name, trans = DistrParamTrans()) {
  self = DistrParam(name, trans)
  self$instance_name = as.character(instance_name)
  self$global_name = instance_name
  self$update_instance_name = function(name) self  ## these should probably be the abstract version
  self$update_global_name = function(name) self
  self$update_names = function(name) {
    self$variable_name = name
    self
  }
  self$expr_ref = function() self$trans$ref(self$global_name)
  self$check_in_spec = function() {
    if (!self$global_name %in% names(self$model_spec$default)) {
      stop(self$global_name, " is not in the model spec")
    }
  }
  return_object(self, "DistrParamChar")
}
DistrParamCharFit = function(name, instance_name, trans = DistrParamTrans()) {
  self = DistrParamChar(name, instance_name, trans)
  # Need to update default value with trans(default val)
  # self$default = function() {
  #   list(self$trans$ref(self$check_in_spec()))  |> setNames(self$instance_name)
  # }
  self$distr_params_frame = function() {
    self$check_in_spec()
    mat = self$global_name
    data.frame(
        mat = mat
      , row = 0L, col = 0L
      , default = self$model_spec$default[[mat]]
    )
  }
  return_object(self, "DistrParamCharFit")
}
DistrParamCharNoFit = function(name, instance_name, trans = DistrParamTrans()) {
  self = DistrParamChar(name, instance_name, trans)
  self$default = function() {
    self$check_in_spec()
    list()
  }
  return_object(self, "DistrParamCharNoFit")
}


DistrParamTrans = function() {
  self = Base()
  self$ref = function(x) x
  self$nm  = function(x) x
  self$val = function(x) x
  self$resolve_default = function(default) self
  return_object(self, "DistrParamTrans")
}

DistrParamTransDefault = function() {
  self = DistrParamTrans()
  self$resolve_default = function(default) default
  return_object(self, "DistrParamTransDefault")
}

DistrParamIdentity = function() {
  self = DistrParamTrans()
  return_object(self, "DistrParamIdentity")
}

DistrParamLog = function() {
  self = DistrParamTrans()
  self$ref = function(x) sprintf("exp(%s)", x)
  self$nm  = function(x) sprintf("log_%s", x)
  self$val = function(x) log(x)
  return_object(self, "DistrParamLog")
}

#' @importFrom stats qlogis
DistrParamLogit = function() {
  self = DistrParamTrans()
  self$ref = function(x) sprintf("(1 / (1 + exp(-%s)))", x)
  self$nm  = function(x) sprintf("logit_%s", x)
  self$val = function(x) qlogis(x)
  return_object(self, "DistrParamLogit")
}

DistrParamSqrt = function() {
  self = DistrParamTrans()
  self$ref = function(x) sprintf("(%s^2)", x)
  self$nm  = function(x) sprintf("sqrt_%s", x)
  self$val = function(x) sqrt(x)
  return_object(self, "DistrParamSqrt")
}

#' Distributional Parameter Transformation
#'
#' @name transform_distr_param
NULL

#' @description * `mp_identity` - Identity transformation 
#' @format NULL
#' @rdname transform_distr_param
#' @export
mp_identity = DistrParamIdentity()

#' @description * `mp_log` - Log transformation 
#' @format NULL
#' @rdname transform_distr_param
#' @export
mp_log = DistrParamLog()

#' @description * `mp_logit` - Logit transformation 
#' @format NULL
#' @rdname transform_distr_param
#' @export
mp_logit = DistrParamLogit()

#' @description * `mp_sqrt` - Square-root transformation 
#' @format NULL
#' @rdname transform_distr_param
#' @export
mp_sqrt = DistrParamSqrt()


TESTDISTR = function(location, sd) {
  self = DistrSpec()
  self$distr_param_objs = nlist(location, sd)

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






# mp_{DIST_NAME} = function(
#       {DISTR_PARAM_1} = mp_{FIT|NOFIT|}{_TRANS|}({NUMERIC|CHARACTER})
#     , {DISTR_PARAM_2} = mp_{FIT|NOFIT|}{_TRANS|}({NUMERIC|CHARACTER})
#     , ...
# )


#' Distributions
#' 
#' Distributions which can be used to specify prior or likelihood components in 
#' model calibration.
#' 
#' @param location Location parameter.
#' Specifying the `location` parameter is only necessary when the distribution
#' is used as a prior distribution. If it is used as a likelihood component the 
#' location parameter will be taken as the simulated variable being fitted to 
#' data, and so this `location` parameter should be left to the default.
#' @param sd Standard deviation parameter.
#' @param disp Dispersion parameter. 
#' @param default_trans Named list of default transformations for each 
#' distributional parameter. See `?transform_distr_param` for a list of 
#' available transformations.
#' 
#' @details All distributional parameter arguments can be specified either as 
#' a numeric value, a character string giving the parameter name, or a 
#' distributional parameter object (See ?fit_distr_params).
#' @name distribution
NULL

#' @description * Uniform Distribution (Improper), only appropriate for prior 
#' components - `mp_uniform`
#' @name distribution
#' @export
mp_uniform = function(default_trans = list()) { 
  self = DistrSpec(
    distr_param_objs = nlist()
    , default_trans = default_trans
  )
  self$prior = \(par) {
    "-0"
  }
  self$likelihood = \(obs, sim) { 
    stop("You cannot specify uniform likelihoods")
    
  }
  return_object(self, "DistrSpecUniform")
}


#' @description * Normal Distribution - `mp_normal`
#' @name distribution
#' @export
mp_normal = function(location = mp_distr_param_null("location")
     , sd
     , default_trans = list(location = mp_identity, sd = mp_log)
  ) {
  self = DistrSpec(
      distr_param_objs = nlist(location, sd)
    , default_trans = default_trans
  )
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
  return_object(self, "DistrSpecNormal")
}

#' @description * Log-Normal Distribution - `mp_log_normal`
#' @name distribution
#' @export
mp_log_normal = function(location = mp_distr_param_null("location")
                       , sd
                       , default_trans = list(location = mp_identity, sd = mp_identity)) {
  self = DistrSpec(
      distr_param_objs = nlist(location, sd)
      # identity transformations because distributional parameters are already
      # specified on the log scale?
    , default_trans = default_trans
  )

  self$prior = \(par) {
    sprintf("-sum(dnorm(log(%s), %s, %s))"
            , par
            , self$distr_param_objs$location$expr_ref()
            , self$distr_param_objs$sd$expr_ref()
    )
  }
  self$likelihood = \(obs, sim) {
    sprintf("-sum(dnorm(log(%s), log(%s), %s))"
            , obs
            , sim
            , self$distr_param_objs$sd$expr_ref()
    )
  }
  self$check_variable = function(variable) {
    any_bad = lapply(variable, all.equal, 0) |> vapply(isTRUE, logical(1L)) |> any()
    if (any_bad) {
      warning(sprintf("Log-normal distribution chosen for %s that contains zeros at the beginning of the simulation", self$variable_name))
    }
  }
  return_object(self, "DistrSpecLogNormal")
}


#' @description * Logit-Normal Distribution - `mp_logit_normal`
#' @name distribution
#' @export
mp_logit_normal = function(location = mp_distr_param_null("location")
     , sd
     , default_trans = list(location = mp_identity, sd = mp_identity)
  ) {
  self = DistrSpec(
      distr_param_objs = nlist(location, sd)
      # identity transformations because distributional parameters are already
      # specified on the log scale?
    , default_trans = default_trans
  )

  self$prior = \(par) {
    sprintf("-sum(dnorm(log(%s) - log(1 - %s), %s, %s))"
            , par, par
            , self$distr_param_objs$location$expr_ref()
            , self$distr_param_objs$sd$expr_ref()
    )
  }
  self$likelihood = \(obs, sim) {
    sprintf("-sum(dnorm(log(%s) - log(1 - %s), log(%s) - log(1 - %s), %s))"
            , obs, obs
            , sim, sim
            , self$distr_param_objs$sd$expr_ref()
    )
  }
  self$check_variable = function(variable) {
    any_bad_0 = lapply(variable, all.equal, 0) |> vapply(isTRUE, logical(1L)) |> any()
    any_bad_1 = lapply(variable, all.equal, 1) |> vapply(isTRUE, logical(1L)) |> any()
    if (any_bad_0) {
      warning(sprintf("Logit-normal distribution chosen for %s that contains zeros at the beginning of the simulation", self$variable_name))
    }
    if (any_bad_1) {
      warning(sprintf("Logit-normal distribution chosen for %s that contains ones at the beginning of the simulation", self$variable_name))
    }
  }
  return_object(self, "DistrSpecLogNormal")
}



#' @description * Poisson Distribution - `mp_poisson`
#' @name distribution
#' @export
mp_poisson = function(location = mp_distr_param_null("location")
                    , default_trans = list(location = mp_identity)) { 
  self = DistrSpec(
      distr_param_objs = nlist(location)# should this be named lambda
    , default_trans = default_trans
  )
  self$prior = \(par) {
    sprintf("-sum(dpois(%s, %s))"
            , par
            , self$distr_param_objs$location$expr_ref()
    )
  }
  self$likelihood = \(obs, sim) { # par doesn't get used here
    sprintf("-sum(dpois(%s, %s))"
            , obs
            , sim
    )
  }
  return_object(self, "DistrSpecPoisson")
}
#' @description * Negative Binomial Distribution - `mp_neg_bin` 
#' @name distribution
#' @export
mp_neg_bin = function(location = mp_distr_param_null("location")
                    , disp
                    , default_trans = list(location = mp_identity, disp = mp_log)) {
  self = DistrSpec(
      distr_param_objs = nlist(location, disp)
    , default_trans = default_trans
  )
  self$prior = \(par) {
    sprintf("-sum(dnbinom(%s, clamp(%s), %s))"
            , par
            , self$distr_param_objs$location$expr_ref()
            , self$distr_param_objs$disp$expr_ref()
    )
  }
  self$likelihood = \(obs, sim) {
    sprintf("-sum(dnbinom(%s, clamp(%s), %s))"
            , obs
            , sim
            , self$distr_param_objs$disp$expr_ref()
    )
  }
  return_object(self, "DistrSpecNegBin")
}

#' Fitting Distributional Parameters
#' 
#' Distributional parameters can be added to the list of parameters that are fit
#' during calibration. By default, distributional parameters in priors and
#' likelihoods are not fit. Use `mp_nofit` to exclude distributional parameters
#' from being fit and `mp_fit` to fit them.
#' 
#' @param x numeric starting value of the distributional parameter to fit, or 
#' character name of an existing variable in the model with a default starting
#' value to use.
#' @param trans transformation to apply to the distributional parameter. 
#' By default, distributional parameters inherit a default transformation from
#' the associated distribution. For example, the standard deviation parameter
#' `sd` in the \code{\link{mp_normal}} distributions has a default log
#' transformation specified using \code{\link{mp_log}}.
#' 
#' @return A distributional parameter object.
#' @examples
#' 
#' # First we call the SIR model spec, and generate some data for calibration.
#' spec = mp_tmb_library("starter_models", "sir", package = "macpan2")
#' data = mp_simulator(spec, 50, "infection") |> mp_trajectory()
#' 
#' # Suppose we want to specify a Normal prior on the transmission parameter 
#' # beta, and we are interested in estimating the prior standard deviation.
#' # Here we use `mp_fit` to estimate the standard deviation, `sd`, and we 
#' # provide a numeric starting value for `sd` in the optimization. 
#' cal = mp_tmb_calibrator(
#'     spec
#'   , data
#'   , traj = "infection"
#'   , par = list(beta = mp_normal(location = 0.35, sd = mp_fit(0.1)))
#'   , default = list(beta = 0.25)
#' )
#' 
#' # When viewing the calibration objective function we can see the additional
#' # prior density term added for beta. The standard deviation parameter has
#' # been automatically named 'distr_params_log_sd_beta'.
#' cal$simulator$tmb_model$obj_fn$obj_fn_expr
#' 
#' # Next we optimize and view the fitted parameters. We can see the 
#' # distributional parameter in the coefficient table with a default value 
#' # equal to the numeric value we provided to `mp_fit` above.
#' mp_optimize(cal)
#' mp_tmb_coef(cal)
#' 
#' # If instead we want control over the name of the new fitted distributional
#' # parameter, we can add a new variable to our model specification with the 
#' # default value set to the desired optimization starting value.
#' updated_spec = spec |> mp_tmb_insert(default = list(sd_var = 0.1))
#' 
#' # In the calibrator, we use the name of this newly added variable, "sd_var",
#' # as input to `mp_fit`.
#' cal = mp_tmb_calibrator(
#'     updated_spec
#'   , data
#'   , traj = "infection"
#'   , par = list(beta = mp_normal(location = 0.35, sd = mp_fit("sd_var")))
#'   , default = list(beta = 0.25)
#' )
#' 
#' # We can see this distributional parameter get propogated to the objective 
#' # function and the fitted parameter table.
#' cal$simulator$tmb_model$obj_fn$obj_fn_expr
#' mp_optimize(cal)
#' mp_tmb_coef(cal)
#' @name fit_distr_params
#' @export
mp_fit = function(x, trans = DistrParamTransDefault()) UseMethod("mp_fit")

#' @export
mp_fit.numeric = function(x, trans = DistrParamTransDefault()) DistrParamNumFit("generic_name", x, trans)

#' @export
mp_fit.character = function(x, trans = DistrParamTransDefault()) DistrParamCharFit("generic_name", x, trans)

#' @rdname fit_distr_params
#' @export
mp_nofit = function(x, trans = DistrParamTransDefault()) UseMethod("mp_nofit")

#' @export
mp_nofit.numeric = function(x, trans = DistrParamTransDefault()) DistrParamNumNoFit("generic_name", x, trans)

#' @export
mp_nofit.character = function(x, trans = DistrParamTransDefault()) DistrParamCharNoFit("generic_name", x, trans)




to_distr_param = function(x) UseMethod("to_distr_param")

#' @export
to_distr_param.DistrParam = function(x) x

#' @export 
to_distr_param.numeric = function(x) mp_nofit(x)

#' @export 
to_distr_param.character = function(x) mp_nofit(x)






### below is for testing back-compatibility


#' Poisson Distribution
#' 
#' @param location Distributional parameter for the location of the 
#' distribution.
#' @noRd
mp_poisson2 = function(location) {
  self = DistrSpec()
  self$distr_params = \() list()
  self$expr_char = \(x, location) sprintf("-sum(dpois(%s, %s))", x, location)
  return_object(self, "Poisson")
}

#' Negative Binomial Distribution
#' 
#' @param disp Dispersion parameter.
#' @noRd
mp_neg_bin2 = function(disp) {
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


#' Normal Distribution
#' 
#' @param location Location parameter. Only necessary if used as a prior
#' distribution. If it is used as a likelihood component the location
#' parameter will be taken as the simulated variable being fitted to data,
#' and so this `location` parameter should be left `NULL`.
#' @param sd Standard deviation parameter.
#' @noRd
mp_normal2 = function(location = NULL, sd = 1) {
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
#' 
#' @param sd Standard deviation parameter.
#' @noRd
mp_log_normal2 = function(sd) {
  self = mp_normal(sd)
  self$expr_char = \(x, location, log_sd) {
    sprintf("-sum(dnorm(log(%s), log(%s), exp(%s)))", x, location, log_sd)
  }
  return_object(self, "LogNormal")
}

# TODO: mp_clamped_poisson, mp_sum_of_squares
