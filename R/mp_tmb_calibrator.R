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
#' produced by \code{\link{mp_tmb_trajectory}}.
#' @param traj A character vector giving the names of trajectories to fit
#' to data.
#' @param tv A character vector giving the names of parameters to make
#' time-varying according to the values in \code{data}.
#' @param par A character vector giving the names of parameters, either
#' time-varying or not, to fit using trajectory match.
#' @param outputs A character vector of outputs to pass to 
#' \code{\link{mp_simulator}}.
#' @param time Specify the start and end time of the simulated trajectories,
#' and the time period associated with each time step. Currently the only
#' valid choice is `NULL`, which takes simulation bounds from the `data`.
#'
#' @examples
#' spec = mp_tmb_library("starter_models", "sir", package = "macpan2")
#' sim = mp_simulator(spec, 50, "infection")
#' data = mp_trajectory(sim)
#' cal = mp_tmb_calibrator(
#'     spec |> mp_tmb_update(default = list(beta = 0.25))
#'   , data
#'   , traj = "infection"
#'   , par = "beta"
#' )
#' mp_optimize(cal, "nlminb")
#' mp_fixed_effects(cal)
#' @export
mp_tmb_calibrator = function(spec, data
    , traj = character()
    , par = character()
    , tv = character()
    , outputs = character()
    , time = NULL
  ) {
  struc = TMBCalDataStruc(data, time)
  traj = TMBTraj(traj, struc, spec$all_formula_vars())
  tv = TMBTV(tv, struc, traj$global_names_vector())
  par = TMBPar(par, tv, traj, spec, tv$global_names_vector())
  
  ## copy the spec
  cal_spec = mp_tmb_model_spec(
      spec$before, spec$during, spec$after
    , spec$default, spec$integers
    , spec$must_save, spec$must_not_save, spec$sim_exprs
  )
  
  ## add observed trajectories
  mp_tmb_insert(cal_spec
    , phase = "after"
    , at = 1L
    , expressions = traj$sim_collect_exprs()
    , default = globalize(traj, "obs")
    , integers = globalize(traj, "obs_times")
    , must_not_save = globalize(traj, "obs")
    , must_save = traj$traj
  )
  
  ## TODO: handle likelihood trajectories
  
  ## add time-varying parameters
  mp_tmb_insert(cal_spec
    , phase = "during"
    , at = 1L
    , expressions = tv$var_update_exprs()
    , default = globalize(tv, "time_var")
    , integers = c(
        globalize(tv, "change_points")
      , globalize(tv, "change_pointer")
    )
    , must_not_save = globalize(tv, "time_var")
  )
  
  ## add parameter transformations
  mp_tmb_insert(cal_spec
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
  cal_sim
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
#' and saves the history of optimization outputs (TODO: develop a way for 
#' the user to get a hold of this stuff).
#' 
#' @export
mp_optimize = function(model, optimizer, ...) UseMethod("mp_optimize")

#' @describeIn mp_optimize Optimize a TMB simulator. TODO: less technical ...
#' @export
mp_optimize.TMBSimulator = function(model
    , optimizer = c("nlminb", "optim")
    , ...
  ) {
  optimizer = match.arg(optimizer)
  model$optimize[[optimizer]]()
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
      time = macpan2:::Steply(max(data$time))
    } else {
      ## TODO: I'm guessing this could fail cryptically
      time = macpan2:::Daily(min(data$time), max(data$time))
    }
  }
  self$time_steps = time$bound_steps()[2L]
  data$time_ids = time$time_ids(data$time)
  self$matrix_list = split(data, data$matrix)
  return_object(self, "TMBCalDataStruc")
}
TMBTV = function(tv = character(), struc, existing_global_names = character()) {
  self = Base()
  self$existing_global_names = existing_global_names
  
  ## internal data structure:
  ## assumes tv is a character vector
  ## of names identifying matrices
  ## that give piece-wise time variation
  self$tv_list = struc$matrix_list[tv]
  synonyms_for_default = c("default", "Default", "value", "Value", "val", "Val")
  for (p in names(self$tv_list)) {
    if (isTRUE(!any(self$tv_list$time_ids == 0L))) {
      df = self$tv_list[[p]]
      zero_time = list(
          matrix = p
        , row = 0
        , col = 0
        , time_ids = 0L
      ) |> as.data.frame()
      #, value = spec$default[[p]]
      df = macpan2:::bind_rows(zero_time, df)
      value_col = synonyms_for_default[synonyms_for_default %in% names(df)][1]
      df[[value_col]][1] = spec$default[[p]]
      df[[value_col]] = as.numeric(df[[value_col]])
      self$tv_list[[p]] = df
    }
  }
  
  ## get lists of things that will become
  ## things like defaults, integers, ...
  self$time_var = function() {
    lapply(self$tv_list, getElement, "value")
  }
  self$change_points = function() {
    lapply(self$tv_list, getElement, "time_ids")
  }
  self$time_var_melt = function() {
    tv = self$time_var()
    if (length(tv) == 0L) {
      cols = c("matrix", "row", "col", "value") ## TODO: correct?
      return(macpan2:::empty_frame(cols))
    }
    cp = self$change_points()
    (
         mapply(setNames, tv, cp, SIMPLIFY = FALSE) 
      |> setNames(names(tv))
      |> macpan2:::melt_default_matrix_list()
    )
  }
  self$change_pointer = function() {
    nms = names(self$change_points())
    (nms
      |> macpan2:::zero_vector()
      |> as.integer()
      |> as.list()
      |> setNames(nms)
    )
  }
  
  ## define local and external names ... to prepare
  ## for creating expressions, which require global,
  ## not local names
  self$local_names = function() {
    macpan2:::make_names_list(self
      , c("time_var", "change_points", "change_pointer")
    )
  }
  self$global_names = function() {
    macpan2:::map_names(self$existing_global_names, self$local_names())
  }
  self$global_names_vector = function() {
    c(
      self$existing_global_names, 
      unlist(self$global_names(), recursive = TRUE, use.names = FALSE)
    )
  }
  
  ## produce expressions
  self$var_update_exprs = function() {
    nms = self$global_names()
    lhs = names(self$tv_list) ## original spec parameter names
    rhs = sprintf("time_var(%s, %s, %s)"
      , nms$time_var
      , nms$change_points
      , nms$change_pointer
    )
    mapply(macpan2:::two_sided, lhs, rhs, SIMPLIFY = FALSE)
  }
  
  return_object(self, "TMBTV")
}
TMBTraj = function(traj = character(), struc, existing_global_names = character()) {
  self = Base()
  self$existing_global_names = existing_global_names
  
  ## internal data structure:
  ## assumes traj is character vector
  ## identifying matrices
  self$traj_list = struc$matrix_list[traj]
  self$traj = traj
  
  
  ## public methods ----
  ## matrices representing observed times
  self$obs = function() {
    lapply(self$traj_list, getElement, "value")
  }
  ## integer vectors giving the times associated 
  ## with each row in the matrices in self$obs
  self$obs_times = function() {
    lapply(self$traj_list, getElement, "time")
  }
  self$distr_params = function() list()
  self$distr_params_melt = function() {
    cols = c("matrix", "row", "col", "value") ## TODO: correct?
    macpan2:::empty_frame(cols)
  }
  
  ## define local and external names ... to prepare
  ## for creating expressions, which require global,
  ## not local names
  self$local_names = function() {
    l = macpan2:::make_names_list(self, c("obs", "obs_times", "distr_params"))
    l$sim = sprintf("%s_%s", "sim", self$traj)
    l
  }
  self$global_names = function() {
    macpan2:::map_names(self$existing_global_names, self$local_names())
  }
  self$global_names_vector = function() {
    c(
      self$existing_global_names, 
      unlist(self$global_names(), recursive = TRUE, use.names = FALSE)
    )
  }
  
  ## expressions
  self$sim_collect_exprs = function() {
    nms = self$global_names()
    lhs = nms$sim
    rhs = sprintf("rbind_time(%s, %s)"
      , self$traj
      , nms$obs_times
    )
    mapply(macpan2:::two_sided, lhs, rhs, SIMPLIFY = FALSE)
  }
  self$obj_fn_traj_exprs = function() list()
  self$obj_fn_expr_chars = function() {
    nms = self$global_names()
    sprintf("-sum(dpois(%s, %s))", nms$obs, nms$sim)
  }
  
  return_object(self, "TMBTraj")
}
TMBPar = function(par = character(), tv = NULL, traj = NULL, spec = NULL, existing_global_names = character()) {
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
    cols = c("matrix", "row", "col", "value") ## TODO: correct?
    macpan2:::empty_frame(cols)
  }
  
  self$local_names = function() {
    macpan2:::make_names_list(self, c("trans_vars", "hyperparams"))
  }
  self$global_names = function() {
    macpan2:::map_names(self$existing_global_names, self$local_names())
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
    pf = macpan2:::melt_default_matrix_list(self$spec$default[self$par], FALSE)
    macpan2:::bind_rows(pf
      , self$tv$time_var_melt()
      , self$hyperparams_melt()
      , self$traj$distr_params_melt()
    )
  }
  self$random_frame = function() {
    cols = c("matrix", "row", "col", "value") ## TODO: correct?
    macpan2:::empty_frame(cols)
  }
  
  return_object(self, "TMBPar")
}

globalize = function(obj, type) setNames(obj[[type]](), obj$global_names()[[type]])
sum_obj_terms = function(...) {
  char_terms = c(...) |> paste(collapse = " ")
  macpan2:::one_sided(char_terms)
}

