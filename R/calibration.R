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

  self$params_vector = function() self$simulator$ad_fun()$env$parList()$params
  self$random_vector = function() self$simulator$ad_fun()$env$parList()$random
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
