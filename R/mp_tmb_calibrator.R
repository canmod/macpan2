#' Make TMB Calibrator
#' 
#' Construct an object that can get used to calibrate an object produced by 
#' \code{\link{mp_tmb_model_spec}} or \code{\link{mp_tmb_library}},
#' and possibly modified by \code{\link{mp_tmb_insert}} or 
#' \code{\link{mp_tmb_update}}.
#' 
#' @param spec An TMB model spec to fit to data. Such specs can be produced by 
#' \code{\link{mp_tmb_model_spec}} or \code{\link{mp_tmb_library}},
#' and possibly modified with \code{\link{mp_tmb_insert}} and 
#' \code{\link{mp_tmb_update}}.
#' @param data A data frame containing trajectories to fit to and possibly 
#' time-varying parameters. The data must be of the same format as that 
#' produced by \code{\link{mp_trajectory}}.
#' @param traj A character vector giving the names of trajectories to fit
#' to data, or a named list of likelihood distributions specified with
#' \code{?macpan2::distribution} for each trajectory.
#' @param tv A character vector giving the names of parameters to make
#' time-varying according to the values in \code{data}, or a radial basis 
#' function specified with \code{\link{mp_rbf}}.
#' @param par A character vector giving the names of parameters, either
#' time-varying or not, to fit using trajectory match.
#' @param outputs A character vector of outputs that will be generated
#' when \code{\link{mp_trajectory}}, \code{\link{mp_trajectory_sd}}, or 
#' \code{\link{mp_trajectory_ensemble}} are called on the optimized 
#' calibrator. By default it is just the trajectories listed in `traj`.
#' @param default A list of default values to use to update the defaults
#' in the `spec`. By default nothing is updated. Alternatively one could
#' use \code{\link{mp_tmb_update}} to update the spec outside of the 
#' function. Indeed such an approach is necessary if new expressions, 
#' in addition to default updates, need to be added to the spec 
#' (e.g. seasonally varying transmission).
#' @param time Specify the start and end time of the simulated trajectories,
#' and the time period associated with each time step. Currently the only
#' valid choice is `NULL`, which takes simulation bounds from the `data`.
#'
#' @examples
#' spec = mp_tmb_library("starter_models", "sir", package = "macpan2")
#' sim = mp_simulator(spec, 50, "infection")
#' data = mp_trajectory(sim)
#' cal = mp_tmb_calibrator(
#'     spec
#'   , data
#'   , traj = "infection"
#'   , par = "beta"
#'   , default = list(beta = 0.25)
#' )
#' mp_optimize(cal)
#' mp_tmb_coef(cal)  ## requires broom.mixed package
#' @export
mp_tmb_calibrator = function(spec, data
    , traj
    , par
    , tv = character()
    , outputs = traj
    , default = list()
    , time = NULL ## TODO: implement start-date offset. TODO: implement forecast extension
  ) {
  cal_args = nlist(traj, par, tv, outputs, default, time)
  if (inherits(spec, "TMBSimulator")) {
    stop(
        "This function creates simulators (capable of being calibrated to "
      , "data) from model specifications. But a simulator was passed instead "
      , "of a specification. Please pass in the specification that was used "
      , "to produce the simulator."
    )
  }
  if (!inherits(spec, "TMBModelSpec")) {
    stop(
        "This function creates simulators (capable of being calibrated to "
      , "data) from model specifications. But something other than a model "
      , "spec was passed instead. Please use mp_tmb_model_spec or "
      , "mp_tmb_library to produce a model spec that can be calibrated."
    )
  }
  
  ## gather defaults before they are updated
  force(outputs)
  
  ## copy the spec, reducing to apply state update methods
  ## (e.g. euler, rk4, euler_multinomial), and update defaults
  cal_spec = mp_expand(spec)
  check_default_updates(cal_spec, default)
  cal_spec = mp_tmb_update(cal_spec, default = default)
  
  struc = TMBCalDataStruc(data, time)
  traj = TMBTraj(traj, struc, cal_spec, cal_spec$all_formula_vars())
  tv = TMBTV(tv, struc, cal_spec, traj$global_names_vector())
  par = TMBPar(par, tv, traj, cal_spec, tv$global_names_vector())
  
  traj$check_assumptions(spec, struc)
  tv$check_assumptions(spec, struc)
  par$check_assumptions(spec, struc)
  
  
  ## add observed trajectories 
  ## (see globalize function for comments on what it does)
  cal_spec = mp_tmb_insert(cal_spec
    , phase = "after"
    , at = 1L
    , expressions = traj$sim_collect_exprs()
    , default = globalize(traj, "obs")
    , integers = globalize(traj, "obs_times")
    , must_not_save = names(globalize(traj, "obs"))
    , must_save = traj$outputs()
  )
  
  cal_spec = mp_tmb_insert(cal_spec
    , default = globalize(traj, "distr_params")
    #, must_not_save = names(globalize(traj, "distr_params"))
  )
  
  cal_spec = mp_tmb_insert(cal_spec
    , default = globalize(par, "distr_params")
  )
  
  ## TODO: handle likelihood trajectories
  
  ## add time-varying parameters
  if (tv$type() == "piecewise") {
    cal_spec = mp_tmb_insert(cal_spec
      , phase = "during"
      , at = 1L
      , expressions = tv$var_update_exprs()
      , default = globalize(tv, "time_var")
      , integers = c(
          globalize(tv, "change_points")
        , globalize(tv, "change_pointer")
      )
      , must_not_save = names(globalize(tv, "time_var"))
    )
  } else if (tv$type() == "smooth") {
    cal_spec = mp_tmb_insert(cal_spec
      , phase = "before"
      , at = Inf
      , expressions = tv$before_loop()
      , default = c(
          globalize(tv, "time_var")
        , globalize(tv, "values_var")
        , globalize(tv, "outputs_var")
        , globalize(tv, "prior_sd")
      )
      , integers = c(
          globalize(tv, "row_indexes")
        , globalize(tv, "col_indexes")
        , globalize(tv, "data_time_indexes")
      )
      , must_not_save = c(
          names(globalize(tv, "time_var"))
        , names(globalize(tv, "values_var"))
        , names(globalize(tv, "outputs_var"))
      )
    )
    cal_spec = mp_tmb_insert(cal_spec
      , phase = "during"
      , at = 1L
      , expressions = tv$var_update_exprs()
    )
  }
  
  ## add parameter transformations
  cal_spec = mp_tmb_insert(cal_spec
    , phase = "before"
    , at = 1L
    , expressions = par$inv_trans_exprs()
    , default = globalize(par, "trans_vars")
  )

  ## add condensation of trajectories
  cal_spec = mp_tmb_insert(cal_spec
    , phase = "during"
    , at = Inf
    , expressions = traj$cond_exprs()
    , default = globalize(traj, "cond_params")
  )
  
  if (!is.character(outputs)) outputs = traj$outputs()
  cal_sim = cal_spec$simulator_fresh(
      time_steps = struc$time_steps
    , outputs = outputs
    , initialize_ad_fun = FALSE
  )
  fn = sum_obj_terms(traj$obj_fn_expr_chars(), par$prior_expr_chars(), tv$prior_expr_chars())
  cal_sim$replace$obj_fn(fn)
  cal_sim$replace$params_frame(par$params_frame())
  cal_sim$replace$random_frame(par$random_frame())
  
  TMBCalibrator(spec, spec$copy(), cal_spec, cal_sim, cal_args)
}

TMBCalibrator = function(orig_spec, new_spec, cal_spec, simulator, cal_args = NULL) {
  self = Base()
  self$orig_spec = orig_spec  ## original spec for reference
  self$new_spec = new_spec  ## gets updated as optimization proceeds
  self$cal_spec = cal_spec  ## contaminated with stuff required for calibration
  self$simulator = simulator  ## model simulator object keeping track of optimization attempts
  self$cal_args = cal_args
  return_object(self, "TMBCalibrator")
}



#' @export
print.TMBCalibrator = function(x, ...) {
  spec_printer(x$cal_spec, include_defaults = FALSE)
  cat("---------------------\n")
  msg("Objective function:\n") |> cat()
  cat("---------------------\n")
  cat(deparse1(x$simulator$tmb_model$obj_fn$formula_list()[[1L]], width.cutoff = 500L))
  cat("\n")
  hist = x$simulator$optimization_history
  if (hist$opt_attempted()) {
    cat("\n---------------------\n")
    msg("Optimization:\n") |> cat()
    cat("---------------------\n")
    print(hist$latest())
  }
}

#' Optimize
#'
#' @param model A model object capable of being optimized. See below
#' for model types that are supported.
#' @param optimizer Name of an implemented optimizer. See below for options
#' for each type of \code{model}.
#' @param ... Arguments to pass to the `optimizer`.
#' 
#' @returns The output of the `optimizer`. The `model` object is modified
#' and saves the history of optimization outputs. These outputs can be 
#' obtained using \code{\link{mp_optimizer_output}}.
#' 
#' @export
mp_optimize = function(model, optimizer, ...) UseMethod("mp_optimize")


#' @export
mp_optimize.TMBSimulator = function(model
    , optimizer = c("nlminb", "optim")
    , ...
  ) {
  optimizer = match.arg(optimizer)
  optimizer_results = model$optimize[[optimizer]](...)
  return(optimizer_results)
} 

#' @describeIn mp_optimize Optimize a TMB calibrator.
#' @export
mp_optimize.TMBCalibrator = function(model, optimizer = c("nlminb", "optim"), ...) {
  optimizer_results = mp_optimize(model$simulator, optimizer, ...)
  
  old_defaults = mp_default(model$new_spec) |> frame_to_mat_list()
  new_defaults = (model$simulator
    |> mp_default()
    |> filter(matrix %in% names(old_defaults))
    |> frame_to_mat_list()
  )
  model$new_spec = mp_tmb_update(model$new_spec, default = new_defaults)
  return(optimizer_results)
}

TMBCalDataStruc = function(data, time) {
  self = Base()
  
  ## infer if the time field in the data
  ## is measured in time-steps
  infer_time_step = function(x) {
    y = is.numeric(x)
    if (y) return(TRUE)
    if (inherits(x, "character")) {
      if (all(grepl("^[0-9]+$", x))) {
        return(TRUE)
      }
    }
    FALSE
  }
  if (is.null(time)) {
    if (infer_time_step(data$time)) {
      data$time = as.integer(data$time)
      time = Steps(min(data$time), max(data$time))
    } else {
      ## TODO: I'm guessing this could fail cryptically
      time = Daily(min(data$time), max(data$time), checker = NoError)
    }
  }
  else {
    time = assert_cls(time, "CalTime", match.call(), "?mp_cal_time")
    time$update_data_bounds(data)
  }
  self$time_steps = time$bound_steps()[2L]
  data$time_ids = time$time_ids(data$time)
  self$data_time_ids = data$time_ids
  self$data_time_steps = length(data$time_ids)
  data = rename_synonyms(data
    , time = c(
        "time", "Time", "ID", "time_id", "id", "date", "Date"
      , "time_step", "timeStep", "TimeStep"
    )
    , matrix = c(
        "matrix", "Matrix", "mat", "Mat", "variable", "var", "Variable", "Var"
    )
    , row = c("row", "Row")
    , col = c("col", "Col", "column", "Column")
    , value = c("value", "Value", "val", "Val", "default", "Default")
  )
  ## TODO: Still splitting on matrices, which doesn't allow flexibility
  ## in what counts as an 'output'. In general, an output could be
  ## a matrix, row, or column.
  self$matrix_list = split(data, data$matrix)
  
  self$check_matrices = function(matrices) {
    bad_traj = !matrices %in% names(self$matrix_list)
    if (any(bad_traj)) {
      sprintf("%s (including %s) %s:\n     %s"
        , "Requested trajectories"
        , paste0(matrices[bad_traj], collapse = ", ")
        , "are not available in the data, which includes the following"
        , paste(names(self$matrix_list), collapse = ", ")
      ) |> stop()
    }
    NULL
  }
  
  self$init_list = function(matrices) {
    self$check_matrices(matrices)
    self$matrix_list[matrices]
  }
  return_object(self, "TMBCalDataStruc")
}



#' Optimizer Output
#'
#' Get the output from an optimizer used in model calibration.
#' 
#' When objects created by \code{\link{mp_tmb_calibrator}} are successfully 
#' passed to \code{\link{mp_optimize}}, they build up an 
#' optimization history. This history is recorded as a list of the output 
#' produced by the underlying optimizer (e.g. \code{\link{nlminb}}). This
#' `mp_optimizer_output` function returns the latest output by default
#' or the entire history list.
#' 
#' @param model An object that has been optimized.
#' @param what A string indicating whether to return the results of the
#' `"latest"` optimization attempt or a list with `"all"` of them.
#' 
#' @export
mp_optimizer_output = function(model, what = c("latest", "all")) {
  UseMethod("mp_optimizer_output")
}

#' @export
mp_optimizer_output.TMBCalibrator = function(model, what = c("latest", "all")) {
  hist = model$simulator$optimization_history
  switch(match.arg(what)
    , latest = hist$latest()
    , all = hist$all()
  )
}


NameHandlerAbstract = function() {
  self = Base()
  self$empty_params_frame = empty_frame(c("mat", "row", "col", "default"))
  self$local_names = function() {}
  self$global_names = function() {
    map_names(self$existing_global_names, self$local_names())
  }
  self$global_names_vector = function() {
    c(
      self$existing_global_names, 
      unlist(self$global_names(), recursive = TRUE, use.names = FALSE)
    )
  }
  
  ## check_assumptions has no return value. while running
  ## it is allowed to throw messages, warnings, errors,
  ## or none of the above. it takes a spec object for
  ## comparison, which is useful especially if it has an internal
  ## spec object that is different.
  self$check_assumptions = function(orig_spec, data_struc) NULL

  return_object(self, "NameHandlerAbstract")
}

TMBTrajAbstract = function() {
  self = NameHandlerAbstract()
  
  ## A list of matrices containing 
  ## observed trajectories with names of this 
  ## list given by the 
  ## output variable being matched.
  self$obs = function() list()
  
  ## A list of integers containing
  ## time steps at which observed trajectories are not missing.
  ## The names of this list match the output variable being
  ## matched.
  self$obs_times = function() list()
  
  ## A list describing matrices that parameterize (1) the shape 
  ## and scale of likelihood functions for the trajectories
  ## and (2) condensation steps (e.g. convolution, sum of boxes)
  ## The names of the outer list describe an output variable.
  ## Sometimes this output variable is in the original spec
  ## and other times it is a condensation of the output variables
  ## in the spec.
  self$distr_params = function() list()
  self$cond_params = function() list()
  self$cond_exprs = function() list()
  self$cond_temp = function() list()
  
  ## return a list character vectors giving
  ## expressions for computing trajectories
  ## via condensation steps
  self$obj_fn_traj_exprs = function() character()
  
  ## return a character vector of terms in
  ## the objective function. These will be concatenated
  ## without any separating operators (plus/minus etc 
  ## need to be handled in the expression).
  self$obj_fn_expr_chars = function() character()
  
  ## data frames describing the fixed and random effects corresponding
  ## to distributional parameters
  self$distr_params_frame = function() self$empty_params_frame
  self$distr_random_frame = function() self$empty_params_frame
  self$cond_params_frame = function() self$empty_params_frame
  self$cond_random_frame = function() self$empty_params_frame
  
  return_object(self, "TMBTrajAbstract")
}
TMBTVAbstract = function() {
  self = NameHandlerAbstract()
  
  self$before_loop = function() list()
  self$after_loop = function() list()
  
  ## List with the values of each 
  ## time varying parameter at the change points. The 
  ## names of the list are the time-varying matrices
  ## in the spec.
  self$time_var = function() list()
  
  ## List of the integers 
  ## giving the time-steps of the changepoints with
  ## the first time-step always being 0 (the initial)
  ## The names of the list are the time-varying 
  ## matrices in the spec.
  self$change_points = function() list()
  
  ## List of expressions that update parameters that
  ## are time-varying
  self$var_update_exprs = function() list()
  
  ## character vector of signed expressions that give components
  ## of the prior distribution on the negative log scale. these
  ## components will be combined with the components of the 
  ## likelihood and space-pasted together and converted into 
  ## the objective function expression. the 'signed' part means
  ## that - or + must appear before every term because these 
  ## expressions are going to be space-pasted.
  self$prior_expr_chars = function() character()
  
  ## data frames describing the fixed and random effects corresponding
  ## to time-varying parameters
  self$tv_params_frame = function(tv_pars) self$empty_params_frame
  self$tv_random_frame = function() self$empty_params_frame
  
  return_object(self, "TMBTVAbstract")
}
TMBParAbstract = function() {
  self = NameHandlerAbstract()
  
  ## These functions must be present in every
  ## child class and return these 'types'
  
  ## list of expressions to be placed at the beginning of the
  ## before phase, each of which computes the inverse transform
  ## of a parameter matrix to compute the untransformed matrix.
  self$inv_trans_exprs = function() list()
  
  self$trans_vars = function() list()
  self$hyperparams = function() list()
  
  ## character vector of signed expressions that give components
  ## of the prior distribution on the negative log scale. these
  ## components will be combined with the components of the 
  ## likelihood and space-pasted together and converted into 
  ## the objective function expression. the 'signed' part means
  ## that - or + must appear before every term because these 
  ## expressions are going to be space-pasted.
  self$prior_expr_chars = function() character()
  
  ## data frames describing the fixed and random effects
  self$params_frame = function() self$empty_params_frame
  self$random_frame = function() self$empty_params_frame
  
  ## list of distributional parameters corresponding to parameters
  ## specified with priors
  self$distr_params = function() list()
  self$distr_params_frame = function() self$empty_params_frame
  self$distr_random_frame = function() self$empty_params_frame
  
  return_object(self, "TMBParAbstract")
}

TMBTV = function(tv, struc, spec, existing_global_names = character()) {
  UseMethod("TMBTV")
}

TMBTV.character = function(
      tv
    , struc
    , spec
    , existing_global_names = character()
  ) {
  self = TMBTVAbstract()
  self$existing_global_names = existing_global_names
  self$spec = spec
  self$type = function() "piecewise"
  
  ## internal data structure:
  ## assumes tv is a character vector
  ## of names identifying matrices
  ## that give piece-wise time variation
  self$tv_par = tv
  self$tv_list = struc$matrix_list[tv]
  for (p in names(self$tv_list)) {
    self$tv_list[[p]] = rename_synonyms(self$tv_list[[p]]
      , mat = c("matrix", "Matrix", "mat", "Mat", "variable", "var", "Variable", "Var")
      , row = c("row", "Row")
      , col = c("col", "Col", "column", "Column")
      , default = c("value", "Value", "val", "Val", "default", "Default")
    )
    if (isTRUE(!any(self$tv_list[[p]]$time_ids == 0))) {
      self$tv_list[[p]] = add_row(self$tv_list[[p]]
        , mat = p
        , row = 0L
        , col = 0L
        , time_ids = 0L
        , default = self$spec$default[[p]]
      )
    }
  }
  
  self$time_var = function() lapply(self$tv_list, getElement, "default")
  self$change_points = function() lapply(self$tv_list, getElement, "time_ids")
  self$tv_params_frame = function(tv_par_mat_nms) {
    tv = self$time_var()
    if (length(tv) == 0L) {
      cols = c("mat", "row", "col", "default")
      return(empty_frame(cols))
    }
    l = list()
    time_var_mats = globalize(self, "time_var")
    tv_mat_nms = names(self$tv_list)
    for (i in seq_along(self$tv_list)) {
      if (tv_mat_nms[i] %in% tv_par_mat_nms) { ## only add tv mats that are pars
        l = append(l, list(data.frame(
            mat = names(time_var_mats)[[i]]
          , row = seq_along(self$time_var()[[i]]) - 1L
          , col = 0L
          , default = time_var_mats[[i]]
        )))
      }
    }
    bind_rows(l)
  }
  self$change_pointer = function() {
    ## Depended upon to return a list if length-one
    ## integer vectors with a single zero. Names of 
    ## the list are the time-varying matrices in the
    ## spec. 
    nms = names(self$change_points())
    (nms
      |> zero_vector()
      |> as.integer()
      |> as.list()
      |> setNames(nms)
    )
  }
  
  ## define local and external names ... to prepare
  ## for creating expressions, which require global,
  ## not local names
  self$local_names = function() {
    make_names_list(self
      , c("time_var", "change_points", "change_pointer")
    )
  }
    
  ## produce expressions
  self$var_update_exprs = function() {
    ## Depended upon to return a list of expressions returning
    ## the value of the time-varying parameter at each time step.
    ## The names of this list is the time-varying matrix.
    nms = self$global_names()
    lhs = names(self$tv_list) ## original spec parameter names
    rhs = sprintf("time_var(%s, %s, %s)"
      , nms$time_var
      , nms$change_points
      , nms$change_pointer
    )
    mapply(two_sided, lhs, rhs, SIMPLIFY = FALSE)
  }
  
  self$check_assumptions = function(orig_spec, data_struc) {
    bad_pars = !self$tv_par %in% unique(names(data_struc$matrix_list))
    if (any(bad_pars)) {
      sprintf("%s (including %s) %s:\n     %s\n%s %s"
        , "Requested piece-wise time-varying parameters"
        , paste0(self$tv_par[bad_pars], collapse = ", ")
        , "are not available in the data, which includes the following"
        , paste(unique(names(data_struc$matrix_list)), collapse = ", ")
        , "Piece-wise time-varying parameters must appear in the data"
        , "in order to identify what time-steps are change-points."
      ) |> stop()
    }
    NULL
  }
  
  return_object(self, "TMBTV")
}

TMBTV.TVArg = function(
      tv
    , struc
    , spec
    , existing_global_names = character()
) {
  self = TMBTVAbstract()
  self$existing_global_names = existing_global_names
  self$spec = spec
  self$type = function() "list"
  
  self$before_loop = function() list()
  self$after_loop = function() list()
  
  ## List with the values of each 
  ## time varying parameter at the change points. The 
  ## names of the list are the time-varying matrices
  ## in the spec.
  self$time_var = function() list()
  
  ## List of the integers 
  ## giving the time-steps of the changepoints with
  ## the first time-step always being 0 (the initial)
  ## The names of the list are the time-varying 
  ## matrices in the spec.
  self$change_points = function() list()
  
  ## List of expressions that update parameters that
  ## are time-varying
  self$var_update_exprs = function() list()
  
  ## data frames describing the fixed and random effects corresponding
  ## to time-varying parameters
  self$tv_params_frame = function(tv_pars) self$empty_params_frame
  self$tv_random_frame = function() self$empty_params_frame
  
  return_object(self, "TMBTV")
}

TMBTV.RBFArg = function(
      tv
    , struc
    , spec
    , existing_global_names = character()
  ) {
  self = TMBTVAbstract()
  self$existing_global_names = existing_global_names
  self$spec = spec
  self$type = function() "smooth"
  
  self$rbf_data = sparse_rbf_notation(
      struc$data_time_steps
    , tv$dimension
    , zero_based = TRUE
    , tol = tv$sparse_tol
  )
  self$initial_outputs = c(self$rbf_data$M %*% tv$initial_weights)
  self$initial_weights = tv$initial_weights
  self$dimension = tv$dimension
  self$data_time_ids = struc$data_time_ids
  self$par_name = tv$tv
  self$prior_sd_default = tv$prior_sd
  self$fit_prior_sd = tv$fit_prior_sd
  
  self$time_var = function() {
    setNames(list(self$initial_weights), self$par_name)
  }
  self$values_var = function() {
    setNames(list(self$rbf_data$values), self$par_name)
  }
  self$outputs_var = function() {
    setNames(list(self$initial_outputs), self$par_name)
  }
  self$row_indexes = function() {
    setNames(list(as.integer(self$rbf_data$row_index)), self$par_name)
  }
  self$col_indexes = function() {
    setNames(list(as.integer(self$rbf_data$col_index)), self$par_name)
  }
  self$data_time_indexes = function() {
    setNames(list(as.integer(c(0, self$data_time_ids))), self$par_name)
  }
  self$prior_sd = function() {
    setNames(list(self$prior_sd_default), self$par_name)
  }
  self$before_loop = function() {
    nms = self$global_names()
    s = sprintf("%s ~ group_sums(%s * %s[%s], %s, %s)" ## TODO: change 1 to an argument in mp_rbf
      , nms$outputs_var, nms$values_var, nms$time_var, nms$col_indexes, nms$row_indexes, nms$outputs_var
    )
    s2 = sprintf("%s ~ c(%s[0], %s)", nms$outputs_var, nms$outputs_var, nms$outputs_var)
    list(as.formula(s), as.formula(s2))
  }
  self$var_update_exprs = function() {
    nms = self$global_names()
    # s = sprintf("%s ~ exp(%s[time_step(1)])", self$par_name, nms$outputs_var)
    s = sprintf("%s ~ exp(time_var(%s, %s))", self$par_name, nms$outputs_var, nms$data_time_indexes)

    list(as.formula(s))
  }
  self$local_names = function() {
    make_names_list(self
      , c(
          "time_var", "values_var", "outputs_var"
        , "row_indexes", "col_indexes", "data_time_indexes"
        , "prior_sd"
      )
    )
  }
  
  ## character vector of signed expressions that give components
  ## of the prior distribution on the negative log scale. these
  ## components will be combined with the components of the 
  ## likelihood and space-pasted together and converted into 
  ## the objective function expression. the 'signed' part means
  ## that - or + must appear before every term because these 
  ## expressions are going to be space-pasted.
  self$prior_expr_chars = function() {
    nms = self$global_names()
    sprintf(
        "-sum(dnorm(%s, 0, %s))"
      , nms$values_var, nms$prior_sd
    )
  }
  
  ## data frames describing the fixed and random effects corresponding
  ## to time-varying parameters
  # self$tv_params_frame = function(tv_par_mat_nms) {
  #   self$empty_params_frame
  # }
  self$tv_params_frame = function(tv_par_mat_nms) {
    cols = c("mat", "row", "col", "default")
    nms = self$global_names()
    d = data.frame(
        mat = rep(nms$time_var, self$dimension)
      , row = seq_len(self$dimension) - 1L
      , col = 0L
      , default = self$initial_weights
    )
    if (self$fit_prior_sd) {
      d_prior = data.frame(
          mat = nms$prior
        , row = 0L
        , col = 0L
        , default = self$prior_sd_default
      )
      d = rbind(d, d_prior)
    }
    return(d)
  }
  
  return_object(self, "TMBTV")
}


#' @param height Number giving the default height for the convolution kernel.
#' @param mean Number giving the mean of the kernel.
#' @param cv Number giving the coefficient of variation of the kernel.
#' @noRd
mp_gamma_conv = function(height, mean, cv) {
  self = Base()
  self$height = height
  self$length = length
  self$mean = mean
  self$cv = cv
  
  self$shape = function() 1 / (self$cv^2)
  self$scale = function() self$mean * self$cv^2
  
  ## TODO: warn if the computed length is 'weird'
  self$length = function() {
    qgamma(0.95, self$shape(), scale = self$scale()) |> ceiling()
  }
  
  self$expr_char = function(
        traj, variable, height, mean, cv # strings to be supplied by calibrator
      , p = "p", delta = "delta", kernel = "kernel" # temporary names that can be overridden
    ) {
    length = self$length()
    c(
        sprintf("%s ~ pgamma(1:(%s+1), 1/(%s), %s * (%s^2))", p, length, cv, mean, cv)
      , sprintf("%s ~ %s[1:%s] - %s[0:(%s-1)]", delta, p, length, p, length)
      , sprintf("%s ~ %s * %s / sum(%s)", kernel, height, delta, delta)
      , sprintf("%s ~ convolution(%s, %s)", traj, variable, kernel)
    )
  }
  
  return_object(self, "GammaConv")
}


TMBTraj = function(traj
      , struc # = TMBCalDataStruc()
      , spec
      , existing_global_names = character()
) {
  UseMethod("TMBTraj")
}

TMBTraj.function = function(        
      traj # function that takes a struc, spec, and existing_global_names and returns a TMBTraj object
    , struc # = TMBCalDataStruc()
    , spec
    , existing_global_names = character()
  ) {
  traj(struc, spec, existing_global_names)
}

TMBTraj.character = function(
        traj = character()
      , struc # = TMBCalDataStruc()
      , spec
      , existing_global_names = character()
    ) {
  self = TMBTrajAbstract()
  self$spec = spec
  self$existing_global_names = existing_global_names
  
  ## internal data structure:
  self$list = struc$init_list(traj)
  
  ## Depended upon to create a character vector of output variables to fit to
  self$outputs = function() names(self$list)
  
  ## implemented methods
  self$obs = function() lapply(self$list, getElement, "value")
  self$obs_times = function() lapply(self$list, getElement, "time_ids")
  self$distr_params = function() {
    switch(
        getOption("macpan2_default_loss")[1L]
      , clamped_poisson = list()
      , neg_bin = setNames(as.list(rep(0, length(self$outputs()))), self$outputs())
      , poisson = list()
      , sum_of_squares = list()
    )
  }
  
  ## define local and external names ... to prepare
  ## for creating expressions, which require global,
  ## not local names
  self$local_names = function() {
    l = make_names_list(self, c("obs", "obs_times", "distr_params"))
    l$sim = sprintf("%s_%s", "sim", self$outputs())
    l
  }
  
  ## expressions
  self$sim_collect_exprs = function() {
    ## Depended upon to return a list of expressions to be 
    ## evaluated in the "after" phase to collect the 
    ## simulated trajectories into one matrix per observed
    ## trajectory. Each simulated trajectory must have 
    ## the same number of rows (and maybe columns??) as the
    ## corresponding observed trajectory.
    nms = self$global_names()
    lhs = nms$sim
    rhs = sprintf("rbind_time(%s, %s)"
      , self$outputs()
      , nms$obs_times
    )
    mapply(two_sided, lhs, rhs, SIMPLIFY = FALSE)
  }
  self$obj_fn_traj_exprs = function() character()
  self$obj_fn_expr_chars = function() {
    nms = self$global_names()
    switch(
        getOption("macpan2_default_loss")[1L]
      , clamped_poisson = sprintf("-sum(dpois(%s, clamp(%s)))", nms$obs, nms$sim)
      
        ## this will fail for multiple trajectories? need to move on
        ## to explicit trajectory specifications with likelihood
        ## and condensation specs
      , neg_bin = sprintf("-sum(dnbinom(%s, clamp(%s), exp(%s)))", nms$obs, nms$sim, nms$distr_params)
      
      , poisson = mp_poisson()$expr_char(nms$obs, nms$sim)
      , sum_of_squares = sprintf("-sum(dnorm(%s, %s, 1))", nms$obs, nms$sim)
    )
  }
  
  self$distr_params_frame = function() {
    nms = self$global_names()
    switch(
        getOption("macpan2_default_loss")[1L]
      , clamped_poisson = empty_frame(c("mat", "row", "col", "default"))
      , neg_bin = data.frame(
          mat = nms$distr_params
        , row = rep(0L, length(self$outputs()))
        , col = rep(0L, length(self$outputs()))
        , default = unlist(self$distr_params(), use.names = FALSE)
      )
      , poisson = empty_frame(c("mat", "row", "col", "default"))
      , sum_of_squares = empty_frame(c("mat", "row", "col", "default"))
    )
  }
  
  self$cond_params_frame = function() {
    nms = self$global_names()
    data.frame(
        mat = nms$cond_params
      , row = rep(0L, length(self$outputs()))
      , col = rep(0L, length(self$outputs()))
    )
    switch(
        getOption("macpan2_default_loss")[1L]
      , clamped_poisson = empty_frame(c("mat", "row", "col", "default"))
      , neg_bin = data.frame(
          mat = nms$distr_params
        , row = rep(0L, length(self$outputs()))
        , col = rep(0L, length(self$outputs()))
        , default = unlist(self$distr_params(), use.names = FALSE)
      )
      , poisson = empty_frame(c("mat", "row", "col", "default"))
      , sum_of_squares = empty_frame(c("mat", "row", "col", "default"))
    )
  }
  
  self$check_assumptions_basic = function(orig_spec, data_struc) {
    spec_mats = names(orig_spec$all_matrices())
    struc_mats = names(data_struc$matrix_list)
    bad_traj = !struc_mats %in% spec_mats
    if (any(bad_traj)) {
      sprintf("%s (including %s) %s:\n     %s"
        , "Requested trajectories"
        , paste0(struc_mats[bad_traj], collapse = ", ")
        , "are not available in the model spec, which includes the following"
        , paste(spec_mats, collapse = ", ")
      ) |> stop()
    }
    NULL
  }
  self$check_assumptions = self$check_assumptions_basic
  
  return_object(self, "TMBTraj")
}

TMBTraj.list = function(traj
    , struc
    , spec
    , existing_global_names = character()) {
  TMBTraj(
      mp_traj(likelihood = traj)
    , struc
    , spec
    , existing_global_names
  )
}

TMBTraj.TrajArg = function(traj
      , struc
      , spec
      , existing_global_names = character()) {
    
  self = TMBTraj(names(traj$likelihood), struc, spec, existing_global_names)
  
  self$distr_params = function() self$arg$likelihood$default()
  
  ## return a character vector of terms in
  ## the objective function. These will be concatenated
  ## without any separating operators (plus/minus etc 
  ## need to be handled in the expression).
  self$obj_fn_expr_chars = function() {
    nms = self$global_names()
    traj_nms = self$outputs()
    y = character()
    for (i in seq_along(traj_nms)) {
      nm = traj_nms[i]
      ll = self$arg$likelihood$distr_list[[nm]]  ## DistrSpec for trajectory i
      y = c(y, ll$likelihood(nms$obs[i], nms$sim[i]))
    }
    y
  }
  
  ## data frames describing the fixed and random effects corresponding
  ## to distributional parameters
  self$distr_params_frame = function() self$arg$likelihood$distr_params_frame()
  
  ## adapt distributional parameters to this trajectory object
  self$arg = traj ## what the user passed
  self$arg$likelihood = DistrList(self$arg$likelihood, spec)
  self$arg$likelihood$update_global_names(self)
  self$arg$likelihood$remove_location_parameters()
  
  self$check_assumptions = function(orig_spec, data_struc) {
    self$check_assumptions_basic(orig_spec, data_struc)
    self$arg$likelihood$check_variables(data_struc$matrix_list)
    for (t in names(self$arg$likelihood$distr_list)) {
      self$arg$likelihood$distr_list[[t]]$check_args(self$arg$likelihood$distr_list[[t]]$distr_param_objs)
    }
    NULL
  }
  
  return_object(self, "TMBTraj")
}

TMBPar = function(par
      , tv, traj, spec
      , existing_global_names = character()
    ) UseMethod("TMBPar")

TMBPar.ParArg = function(par
      , tv, traj, spec
      , existing_global_names = character()
    ) {
  self = TMBPar(names(par$param), tv, traj, spec, existing_global_names)
  
  self$distr_params = function() self$arg$param$default()
  
  self$prior_expr_chars = function() {
    # union to get both parameters and
    # time-varying pars we are estimating
    par_nms = union(self$par, self$tv_par)
    y = character()
    for (i in seq_along(par_nms)) {
      nm = par_nms[i]
      pp = self$arg$param$distr_list[[nm]]
      y = c(y, pp$prior(nm))
    }
    y
  }
  
  self$distr_params_frame = function() self$arg$param$distr_params_frame()
  
  ## adapt (prior) distributional parameters to this parameter object
  self$arg = par
  self$arg$param = DistrList(self$arg$param, spec)
  self$arg$param$update_global_names(self)
  self$arg$param$error_if_not_all_have_location()
  
  self$check_assumptions = function(orig_spec, data_struc) {
    self$check_assumptions_basic(orig_spec, data_struc)
    self$arg$param$check_variables(data_struc$matrix_list)
    for (p in self$par) {
      self$arg$param$distr_list[[p]]$check_args(self$arg$param$distr_list[[p]]$distr_param_objs)
    }
    NULL
  }
  return_object(self, "TMBPar")
}

TMBPar.list = function(par
      , tv, traj, spec
      , existing_global_names = character()
    ) {
  TMBPar(
    # ignore random (will be incorporated later)
      mp_par(param = par, random = NULL)
    , tv, traj, spec
    , existing_global_names
  )
}

TMBPar.character = function(par
      , tv, traj, spec
      , existing_global_names = character()
    ) {
  self = TMBParAbstract()
  self$existing_global_names = existing_global_names
  self$tv = tv
  self$traj = traj
  self$spec = spec
  
  ## internal data structure
  tv_names = self$tv$time_var() |> names()
  self$par = setdiff(par, tv_names)
  self$tv_par = intersect(par, tv_names)
  
  self$local_names = function() {
    make_names_list(self, c("trans_vars", "hyperparams", "distr_params"))
  }
  self$params_frame = function() {
    pf = (self$spec$default[self$par]
      |> melt_default_matrix_list(FALSE)
      |> rename_synonyms(mat = "matrix", default = "value")
    )
    bind_rows(pf
      , self$tv$tv_params_frame(self$tv_par)
      , self$traj$distr_params_frame()
      , self$distr_params_frame()
    )
  }
  self$random_frame = function() self$tv$tv_random_frame()
  
  self$check_assumptions_basic = function(orig_spec, data_struc) {
    pnms = union(self$par, self$tv_par)
    bad_pars = !pnms %in% names(orig_spec$default)
    if (any(bad_pars)) {
      spec_mats = names(orig_spec$all_matrices())
      sprintf("%s (including %s) %s:\n     %s%s%s"
        , "Requested parameters"
        , paste0(pnms[bad_pars], collapse = ", ")
        , "are either not available in the model spec, which includes the following"
        , paste(spec_mats, collapse = ", ")
        , "\nor cannot be fit because they are not default model parameters. See "
        , "mp_default(spec) for all default model parameters."
      ) |> stop()
    }
  }
  self$check_assumptions = self$check_assumptions_basic
  
  return_object(self, "TMBPar")
}

## the globalize function below handles possible naming conflicts
## in the engine when new variables are added by the system that
## could be the same as a name chosen by the user. the syntax
## of the globalize function works as follows. take the example
## of the `obs` method in the `traj` object. the expression
## traj$obs() would return the untreated automatically generated
## names for the variables that will contain the observed
## trajectories. however, globalize(traj, "obs") does the same
## unless there are name conflicts and in that case returns
## slightly modified names that do not conflict with and of the
## names chosen by the user.
## 
## @param obj An oor-based object with a global_names method that returns 
## a list of character vectors, each of which is associated with the 
## method `type`.
## @param type the name of an oor-based method in obj. this method must be
## a no-op and return an object with names that can be set with setNames.
## 
## @examples
## if sir is an object and pars is a no-op method returning a named object,
## then one may replace sir$pars() for globalize(sir, "pars") and get 
## an object with the same values as sir$pars() but with global names.
## 
globalize = function(obj, type) setNames(obj[[type]](), obj$global_names()[[type]])

## some object components with globalized names have a structured version
## called {type}_struc. This version distributes the elements of obj[[type]]()
## amongst the elements of a list returned by obj[[{type}_struc]](). we can 
## put these global names into the structured list so that we can get the 
## globalized names from the structure using local names, as easily as we
## can get the values from the structure using local names.
globalize_struc_names = function(obj, type) {
  relist(
      obj$global_names()[[type]]
    , obj[[sprintf("%s_struc", type)]]()
  )
}

## combines character vectors and turns them into a one-sided formula
sum_obj_terms = function(...) {
  terms = c(...)
  if (length(terms) == 0L) { ## if there are no terms, the objective function is just 0
    str = "0"
  } else {
    str = paste(terms, collapse = " ")
  }
  one_sided(str)
}


#' Get Underlying TMB Object
#' 
#' Get the result of `TMB::MakeADFun` underlying a TMB-based
#' model in `macpan2`.
#' 
#' @param model An object based on TMB.
#' @export
mp_tmb = function(model) UseMethod("mp_tmb")

#' @export
mp_tmb.TMBSimulator = function(model) {
  structure(model$ad_fun(), class = "TMB")
}

#' @export
mp_tmb.TMBCalibrator = function(model) mp_tmb(model$simulator)
