mp_calibrator = function(
      model
    , data
    , params
    , random
    , time_scale
    , default
) {
  
}



## Internal classes that handle optimization. Objects of 
## these classes are in the `optimize` and `optimization_history`
## fields of `TMBSimulator` objects.

TMBOptimizer = function(simulator) {
  self = Base()
  self$simulator = simulator
  arg_updater = function(opt_method_nm, opt_func, arg_nms_from_object) {
    args = formals(opt_func)
    formals(self[[opt_method_nm]]) = args[!(names(args) %in% arg_nms_from_object)]
  }
  wrap = function(opt_method_nm, opt_func, ...) {
    arg_mappings = list(...)
    force(arg_mappings)
    force(opt_func)
    force(opt_method_nm)

    self[[opt_method_nm]] = function() {

      ## TMB doesn't compute hessians for models with random effects
      random_effects_in_model = !is.null(self$simulator$tmb_model$random_arg())
      hessian_in_args = !is.null(arg_mappings[["he"]])
      if (hessian_in_args & random_effects_in_model) {
        arg_mappings = arg_mappings[!names(arg_mappings) %in% "he"]
      }

      ad_fun = self$simulator$ad_fun()
      ad_fun$fn()  ## work out certain starting value issues -- is this necessary?
      args = list()
      mc = match.call()
      if (length(mc) != 1L) {
        args = setNames(
          lapply(2L:length(mc), function(i) mc[[i]]),
          names(mc)[-1L]
        )
      }
      args_from_object = setNames(
        ad_fun[names(arg_mappings)],
        unname(arg_mappings)
      )
      opt_obj = do.call(opt_func, c(args_from_object, args))
      self$simulator$optimization_history$save(opt_obj)
      self$simulator$cache$sdreport$invalidate() ## now that we have optimized again, we need to invalidate the now out-of-date sdreport
      ad_fun$fn(opt_obj$par) ## probably this should be last.par.best
      opt_obj
    }
    arg_updater(opt_method_nm, opt_func, unname(arg_mappings))
  }
  wrap(
      "optim", stats::optim
    , par = "par", fn = "fn", gr = "gr"
  )
  wrap(
      "nlminb", stats::nlminb
    , par = "start", fn = "objective", gr = "gradient", he = "hessian"
  )

  return_object(self, "TMBOptimizer")
}

TMBCurrentParams = function(simulator) { ## TMBSimulator
  self = Base()
  self$simulator = simulator

  self$n_params = function() {
    self$simulator$tmb_model$params$data_frame() |> nrow()
  }
  self$n_random = function() {
    self$simulator$tmb_model$random$data_frame() |> nrow()
  }
  self$params_vector = function() {
    if (self$n_params() == 0L) return(numeric())
    self$simulator$ad_fun()$env$parList()$params
  }
  self$random_vector = function() {
    if (self$n_random() == 0L) return(numeric())
    self$simulator$objective(self$params_vector())
    self$simulator$ad_fun()$env$parList()$random
  }
  self$params_frame = function() {
    self$simulator$tmb_model$params$data_frame(current = self$params_vector())
  }
  self$random_frame = function() {
    self$simulator$tmb_model$random$data_frame(current = self$random_vector())
  }
  return_object(self, "OptimizedParams")
}

TMBOptimizationHistory = function(simulator) {
  self = Base()
  self$simulator = simulator
  self$.history = list()
  self$get = function() self$.history
  self$save = function(opt_obj) self$.history = append(self$.history, list(opt_obj))
  return_object(self, "TMBOptimizationHistory")
}
