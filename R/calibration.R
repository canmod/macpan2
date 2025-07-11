## Internal classes that handle optimization. Objects of 
## these classes are in the `optimize` and `optimization_history`
## fields of `TMBSimulator` objects.

## To add new optimizers all that should be required is to add a 
## call to `wrap` in the TMBOptimizer constructor

TMBOptimizer = function(simulator) {
  self = Base()
  self$simulator = simulator
  arg_updater = function(opt_method_nm, opt_func, arg_nms_from_object) {
    args = formals(opt_func)
    formals(self[[opt_method_nm]]) = args[!(names(args) %in% arg_nms_from_object)]
  }
  wrap = function(opt_method_nm, opt_func, ...) {
    missing_opt_func = opt_func |> try(silent = TRUE) |> inherits("try-error")
    
    arg_mappings = list(...)
    force(arg_mappings)
    force(opt_method_nm)

    if (!missing_opt_func) {
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
        reduced_object = list(
            ## start from the last best parameter (should be an option?)
            par = get_last_best_par(ad_fun) # ad_fun$par is the alternative
          , fn = ad_fun$fn
          , gr = ad_fun$gr
          , he = ad_fun$he
        )
        args_from_object = setNames(
          reduced_object[names(arg_mappings)],
          unname(arg_mappings)
        )
        opt_obj = do.call(opt_func, c(args_from_object, args))
        self$simulator$optimization_history$save(opt_obj)
        
        ## now that we have optimized again, we need to 
        ## invalidate the now out-of-date sdreport
        self$simulator$cache$sdreport$invalidate() 
        
        self$simulator$objective(get_last_best_par(ad_fun))
        
        opt_obj
      }
      arg_updater(opt_method_nm, opt_func, unname(arg_mappings))
    }
  }
  wrap(
      "optim", stats::optim
    , par = "par", fn = "fn", gr = "gr"
  )
  wrap(
      "nlminb", stats::nlminb
    , par = "start", fn = "objective", gr = "gradient", he = "hessian"
  )
  wrap(
      "DEoptim", DEoptim::DEoptim
    , fn = "fn"
  )
  wrap(
      "optimize", stats::optimize
    , fn = "f"
  )
  wrap(
      "optimise", stats::optimize
    , fn = "f"
  )
  
  self$extract_best = list(
      nlminb = \(obj) obj$par
    , optim = \(obj) obj$par
    , optimize = \(obj) obj$minimum
    , optimise = \(obj) obj$minimum
    , DEoptim = \(obj) obj$optim$bestmem
  )
  self$reset_best = function(opt_obj, optimizer) {
    ## this is useful to make sure that the names
    ## of the parameter vector do not get mangled
    ## by the optimizers
    self$simulator$objective(self$extract_best[[optimizer]](opt_obj))
  }

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
    get_last_best_par(self$simulator$ad_fun())
    # $env$parList()$params
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
  
  ## update a matrix_list, which is a list of numeric matrices (or numeric 
  ## vectors, which will be treated as n-by-1 matrices) so that the current 
  ## fitted values of parameters are used to replace associated values in the 
  ## matrix_list
  self$update_matrix_list = function(matrix_list) {
    current_frame = rbind(self$params_frame(), self$random_frame())
    keepers = current_frame$mat %in% names(matrix_list)
    current_frame = current_frame[keepers, , drop = FALSE]
    if (nrow(current_frame) == 0L) return(matrix_list)
    for (i in seq_len(nrow(current_frame))) {
      mat = current_frame[i, "mat"]
      mat = mat[]
      
      row = current_frame[i, "row"]
      nr = nrow(matrix_list[[mat]])
      if (is.null(nr)) nr = length(matrix_list[[mat]])
      if (row > nr) {
        stop(
          sprintf(
              "Matrix, %s, only has %s rows, but attempting to update row %s"
            , mat, nr, row + 1L
          )
        )
      }
      
      col = current_frame[i, "col"]
      nc = ncol(matrix_list[[mat]])
      if (is.null(nc)) nc = 1L
      if (col > nc) {
        stop(
          sprintf(
              "Matrix, %s, only has %s columns, but attempting to update column %s"
            , mat, nc, col + 1L
          )
        )
      }
      
      val = current_frame[i, "current"]
      if (is.matrix(matrix_list[[mat]])) {
        matrix_list[[mat]][row + 1L, col + 1L] = val
      } else {
        matrix_list[[mat]][row + 1L] = val
      }
    } # endfor
    return(matrix_list)
  }
  
  return_object(self, "OptimizedParams")
}

TMBOptimizationHistory = function(simulator) {
  self = Base()
  self$simulator = simulator
  self$.history = list()
  self$get = function() self$.history
  self$save = function(opt_obj) self$.history = append(self$.history, list(opt_obj))
  self$latest = function() {
    if (!self$opt_attempted()) {
      stop(
          "This model has not been optimized and so it is not possible to extract "
        , "the latest output from an optimizer. To try running please call "
        , "mp_tmb_optimize."
      )
    }
    return(self$.history[[length(self$.history)]])
  }
  self$all = function() self$.history
  self$opt_attempted = function() length(self$.history) > 0L
  return_object(self, "TMBOptimizationHistory")
}

#' Optimization Attempted
#' 
#' Has an attempt been made to calibrate model parameters through optimization
#' of a likelihood function or posterior density, probably by using 
#' \code{\link{mp_optimize}}?
#' 
#' @param model A model that can be calibrated, probably produced using
#' \code{\link{mp_tmb_calibrator}}.
#' 
#' @return Either `TRUE` or `FALSE`.
#' @export
mp_opt_attempted = function(model) UseMethod("mp_opt_attempted")

#' @export
mp_opt_attempted.TMBSimulator = function(model) {
  model$optimization_history$opt_attempted()
}

#' @export
mp_opt_attempted.TMBCalibrator = function(model) {
  mp_opt_attempted(model$simulator)
}


#' Uncertainty Estimated
#' 
#' Does a model contain estimates of parameter uncertainty, probably
#' through a covariance matrix estimated using \code{\link{mp_optimize}}?
#' 
#' @inheritParams mp_opt_attempted
#' @return Either `TRUE` or `FALSE`.
#' @export
mp_uncertainty_estimated = function(model) {
  output = FALSE
  not_singular = Negate(singular)
  if (mp_opt_attempted(model)) {
    output = (model
      |> mp_tmb_fixef_cov() 
      |> not_singular(tol = getOption("macpan2_tol_singular_cov"))
    )
  }
  return(output)
}



# TMBCoef = function(simulator) {
#   self = Base()
#   self$simulator = simulator
# }
