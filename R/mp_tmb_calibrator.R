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
#' to data.
#' @param tv A character vector giving the names of parameters to make
#' time-varying according to the values in \code{data}.
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
    , time = NULL
  ) {
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
  
  ## gather defaults before they are updated (FIXME: this will need to be 
  ## updated when non-character inputs are allowed)
  force(outputs)
  
  ## copy the spec and update defaults
  cal_spec = spec$copy()
  check_default_updates(cal_spec, default)
  cal_spec = mp_tmb_update(cal_spec, default = default)
  
  struc = TMBCalDataStruc(data, time)
  traj = TMBTraj(traj, struc, cal_spec$all_formula_vars())
  tv = TMBTV(tv, struc, cal_spec, traj$global_names_vector())
  par = TMBPar(par, tv, traj, cal_spec, tv$global_names_vector())
  
  ## FIXME: not object oriented. create generic check method
  pnms = union(par$par, par$tv_par)
  parameterized_defaults = spec$default[pnms]
  if (length(parameterized_defaults) > 0L) {
    non_scalars = vapply(parameterized_defaults, length, integer(1L)) != 1L
    if (any(non_scalars)) {
      stop(
        sprintf("The following parameterized model defaults are not scalars:\n%s\nThe development interface can be used to fit such models and in the future we plan on making user interfaces that can handle vector-valued defaults.", paste0(pnms[non_scalars], collapse = ", ")
        )
      )
    }
  }
  
  ## add observed trajectories 
  ## (see globalize function for comments on what it does)
  cal_spec = mp_tmb_insert(cal_spec
    , phase = "after"
    , at = 1L
    , expressions = traj$sim_collect_exprs()
    , default = globalize(traj, "obs")
    , integers = globalize(traj, "obs_times")
    , must_not_save = names(globalize(traj, "obs"))
    , must_save = traj$traj
  )
  
  ## TODO: handle likelihood trajectories
  
  ## add time-varying parameters
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
  
  ## add parameter transformations
  cal_spec = mp_tmb_insert(cal_spec
    , phase = "before"
    , at = 1L
    , expressions = par$inv_trans_exprs()
    , default = globalize(par, "trans_vars")
  )
  
  cal_sim = cal_spec$simulator_fresh(
      time_steps = struc$time_steps
    , outputs = outputs
    , initialize_ad_fun = FALSE
  )
  fn = sum_obj_terms(traj$obj_fn_expr_chars(), par$prior_expr_chars())
  cal_sim$replace$obj_fn(fn)
  cal_sim$replace$params_frame(par$params_frame())
  cal_sim$replace$random_frame(par$random_frame())
  
  TMBCalibrator(spec, spec$copy(), cal_spec, cal_sim)
}

TMBCalibrator = function(orig_spec, new_spec, cal_spec, simulator) {
  self = Base()
  self$orig_spec = orig_spec  ## original spec for reference
  self$new_spec = new_spec  ## gets updated as optimization proceeds
  self$cal_spec = cal_spec  ## contaminated with stuff required for calibration
  self$simulator = simulator  ## model simulator object keeping track of optimization attempts
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
    cat("---------------------\n")
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
  optimizer_results = model$optimize[[optimizer]]()
  return(optimizer_results)
} 

#' @describeIn mp_optimize Optimize a TMB calibrator.
#' @export
mp_optimize.TMBCalibrator = function(model, optimizer, ...) {
  optimizer_results = mp_optimize(model$simulator)
  
  old_defaults = mp_default(model$new_spec) |> frame_to_mat_list()
  new_defaults = (model$simulator
    |> mp_default()
    |> filter(matrix %in% names(old_defaults))
    |> frame_to_mat_list()
  )
  model$new_spec = mp_tmb_update(model$new_spec, default = new_defaults)
  return(optimizer_results)
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


TMBCalDataStruc = function(data, time) {
  self = Base()
  
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
      time = Steps(max(data$time))
    } else {
      ## TODO: I'm guessing this could fail cryptically
      time = Daily(min(data$time), max(data$time))
    }
  }
  self$time_steps = time$bound_steps()[2L]
  data$time_ids = time$time_ids(data$time)
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
  self$matrix_list = split(data, data$matrix)
  return_object(self, "TMBCalDataStruc")
}

TMBTV = function(
      tv = character()
    , struc
    , spec
    , existing_global_names = character()
  ) {
  self = Base()
  self$existing_global_names = existing_global_names
  self$spec = spec
  
  ## internal data structure:
  ## assumes tv is a character vector
  ## of names identifying matrices
  ## that give piece-wise time variation
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
  
  ## get lists of things that will become
  ## things like defaults, integers, ...
  self$time_var = function() {
    ## Depended upon to return a list with the values of each 
    ## time varying parameter at the change points. The 
    ## names of the list are the time-varying matrices
    ## in the spec.
    lapply(self$tv_list, getElement, "default")
  }
  self$change_points = function() {
    ## Depended upon to return a list of the integers 
    ## giving the time-steps of the changepoints with
    ## the first time-step always being 0 (the initial)
    ## The names of the list are the time-varying 
    ## matrices in the spec.
    lapply(self$tv_list, getElement, "time_ids")
  }
  self$time_var_melt = function(tv_par_mat_nms) {
    ## Depended upon to return a data frame with the
    ## following columns:
    ##   1. mat -- 
    ##   2. row
    ##   3. col
    ##   4. default
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
  self$global_names = function() {
    map_names(self$existing_global_names, self$local_names())
  }
  self$global_names_vector = function() {
    c(
      self$existing_global_names, 
      unlist(self$global_names(), recursive = TRUE, use.names = FALSE)
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
  
  return_object(self, "TMBTV")
}
TMBTraj = function(
        traj = character()
      , struc
      , existing_global_names = character()
    ) {
  self = Base()
  self$existing_global_names = existing_global_names
  
  ## internal data structure:
  ## assumes traj is character vector
  ## identifying matrices
  self$traj_list = struc$matrix_list[traj]
  self$traj = traj ## Depended upon to contain a character vector of output variables to fit to
  
  
  ## public methods ----
  ## matrices representing observed times
  self$obs = function() {
    ## Depended upon to return a list of matrices containing 
    ## observed trajectories with names given by the 
    ## output variable being matched.
    lapply(self$traj_list, getElement, "value")
  }
  ## integer vectors giving the times associated 
  ## with each row in the matrices in self$obs
  self$obs_times = function() {
    ## Depended upon to return a list of integers containing
    ## time steps at which observed trajectories are not missing.
    ## The names of this list match the output variable being
    ## matched.
    lapply(self$traj_list, getElement, "time")
  }
  self$distr_params = function() list()
  self$distr_params_melt = function() {
    cols = c("mat", "row", "col", "default") ## TODO: correct?
    empty_frame(cols)
  }
  
  ## define local and external names ... to prepare
  ## for creating expressions, which require global,
  ## not local names
  self$local_names = function() {
    l = make_names_list(self, c("obs", "obs_times", "distr_params"))
    l$sim = sprintf("%s_%s", "sim", self$traj)
    l
  }
  self$global_names = function() {
    map_names(self$existing_global_names, self$local_names())
  }
  self$global_names_vector = function() {
    c(
      self$existing_global_names, 
      unlist(self$global_names(), recursive = TRUE, use.names = FALSE)
    )
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
      , self$traj
      , nms$obs_times
    )
    mapply(two_sided, lhs, rhs, SIMPLIFY = FALSE)
  }
  self$obj_fn_traj_exprs = function() list()
  self$obj_fn_expr_chars = function() {
    ## Depended upon to return a character vector of terms in
    ## the objective function. These will be concatenated
    ## without any separating operators (plus/minus etc 
    ## need to be handled in the expression).
    nms = self$global_names()
    switch(
        getOption("macpan2_default_loss")[1L]
      , clamped_poisson = sprintf("-sum(dpois(%s, clamp(%s)))", nms$obs, nms$sim)
      , poisson = sprintf("-sum(dpois(%s, %s))", nms$obs, nms$sim)
      , sum_of_squares = sprintf("-sum(dnorm(%s, %s, 1))", nms$obs, nms$sim)
    )
    
  }
  
  return_object(self, "TMBTraj")
}
TMBPar = function(
        par = character()
      , tv = NULL, traj = NULL, spec = NULL
      , existing_global_names = character()
    ) {
  self = Base()
  self$existing_global_names = existing_global_names
  self$tv = tv
  self$traj = traj
  self$spec = spec
  
  ## internal data structure
  tv_names = self$tv$time_var() |> names()
  self$par = setdiff(par, tv_names)
  self$tv_par = intersect(par, tv_names)
  
  
  self$trans_vars = function() list()
  self$hyperparams = function() list()
  self$hyperparams_melt = function() {
    cols = c("mat", "row", "col", "default")
    empty_frame(cols)
  }
  
  self$local_names = function() {
    make_names_list(self, c("trans_vars", "hyperparams"))
  }
  self$global_names = function() {
    map_names(self$existing_global_names, self$local_names())
  }
  self$global_names_vector = function() {
    c(
      self$existing_global_names, 
      unlist(self$global_names(), recursive = TRUE, use.names = FALSE)
    )
  }
  
  self$inv_trans_exprs = function() list()
  self$prior_expr_chars = function() character(0L)
  
  ## produce fixed and random effect parameter frames
  ## associated with time-varying parameters
  self$params_frame = function() {
    ## Depended upon to produce a data frame with the following columns:
    ##   1. 
    pf = (self$spec$default[self$par]
      |> melt_default_matrix_list(FALSE)
      |> rename_synonyms(mat = "matrix", default = "value")
    )
    bind_rows(pf
      , self$tv$time_var_melt(self$tv_par)
      , self$hyperparams_melt()
      , self$traj$distr_params_melt()
    )
  }
  self$random_frame = function() {
    cols = c("mat", "row", "col", "default")
    empty_frame(cols)
  }
  
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
globalize = function(obj, type) setNames(obj[[type]](), obj$global_names()[[type]])

## combines character vectors and turns them into a one-sided formula
sum_obj_terms = function(...) {
  c(...) |> paste(collapse = " ") |> one_sided()
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