#' Create a Simulator
#' 
#' Construct a simulator from a model specification object.
#' 
#' @param model A model specification object.
#' @param time_steps How many time steps should be simulated when simulations
#' are requested?
#' @param outputs Character vector of names of model quantities that will be
#' outputted when simulations are requested.
#' @param default Named list of numerical objects that will update the default
#' values defined in the model specification object. Any number of objects
#' can be updated or not.
#' @param inits An optional list of initial values for the state variables.
#' These initial values can be added to the `default` list with identical 
#' results, but adding them to `inits` is better practice because it makes it 
#' clear that they are initial values that will change as the state updates.
#' 
#' @concept create-model-simulator
#' @export
mp_simulator = function(model
    , time_steps
    , outputs
    , default = list()
    , inits = list()
  ) {
  UseMethod("mp_simulator")
}

#' @export
mp_simulator.default = function(model
    , time_steps
    , outputs
    , default = list()
    , inits = list()
  ) {
  stop("You can only create a simulator from a model specification or a calibrator. But a ", class(model)[1L], " was passed instead.")
}

#' @export
mp_simulator.TMBModelSpec = function(model
    , time_steps
    , outputs
    , default = list()
    , inits = list()
  ) {
  default = c(default, inits)
  model$simulator_fresh(time_steps, outputs, default)
}

#' @export
mp_simulator.TMBSimulator = function(model
    , time_steps
    , outputs
    , default = list()
    , inits = list()
  ) {
  stop("under construction")
  if (!missing(time_steps)) {
    model$simulator$replace$time_steps(time_steps)
  }
  ## TODO: 
  ## set the params vector as the last best params vector
  ## update the outputs
}

#' @export
mp_simulator.TMBCalibrator = function(model
    , time_steps
    , outputs
    , default = list()
    , inits = list()
  ) {
  default = c(default, inits)
  mp_simulator(model$simulator, time_steps, outputs, default)
}

#' @export
mp_simulator.TMBParameterizedModelSpec = function(model
  , time_steps, outputs, default = list(), inits = list()
) {
  ## FIXME: doesn't seem to be used anywhere
  simulator = mp_simulator(model$spec, time_steps, outputs, default)
}


#' TMB Model
#'
#' Define a compartmental model in TMB. This model uses the spec
#' \url{https://canmod.net/misc/cpp_side}.
#'
#' @param init_mats An object of class \code{\link{MatsList}}.
#' @param expr_list An object of class \code{\link{ExprList}}.
#' @param params An object of class \code{\link{OptParamsList}}.
#' @param random An object of class \code{\link{OptParamsList}}.
#' @param obj_fn An object of class \code{\link{ObjectiveFunction}}.
#' @param time_steps An object of class \code{\link{Time}}.
#' @param do_pred_sdreport A logical flag (\code{FALSE}/\code{TRUE}, or any value evaluating to 1 for \code{TRUE}) indicating whether predicted values should be accessible via \code{TMB::sdreport()}
#' @param engine_methods An object of class \code{\link{EngineMethods}}.
#' @param log_file An object of class \code{\link{LogFile}}.
#'
#' @return Object of class \code{TMBModel} with the following methods.
#'
#' ## Methods
#'
#' * `$data_arg()` -- Return all of the components of the data structure
#' to pass to C++.
#' * `$param_arg()` -- Return all of the components of the parameter list
#' to pass to C++.
#' * `$simulator()` -- Return an object of class \code{\link{TMBSimulator}},
#' which can be used to simulate data from the model.
#'
#' @examples
#' sir = TMBModel(
#'   init_mats = MatsList(
#'     state = c(1 - 1e-2, 1e-2, 0),
#'     beta = 0.3,
#'     gamma = 0.2,
#'     N = 1,
#'     foi = 0,
#'     ratemat = matrix(0, 3, 3),
#'     flowmat = matrix(0, 3, 3),
#'     .mats_to_save = c("state", "N", "foi"),
#'     .mats_to_return = c("state", "N", "foi")
#'   ),
#'   expr_list = ExprList(
#'     before = list(
#'       N ~ sum(state)
#'     ),
#'     during = list(
#'       foi ~ beta * state[1, 0] / N,
#'       ratemat ~ matrix(c(
#'         0,   0,     0,
#'         foi, 0,     0,
#'         0,   gamma, 0), 3, 3),
#'       flowmat ~ ratemat * state,
#'       state ~ state - row_sums(flowmat) + t(col_sums(flowmat))
#'     )
#'   ),
#'   params = OptParamsList(0.3
#'     , par_id = 0L
#'     , mat = "beta"
#'     , row_id = 0L
#'     , col_id = 0L
#'   ),
#'   random = OptParamsList(),
#'   obj_fn = ObjectiveFunction(~ foi + 1),
#'   time_steps = Time(time_steps = 30L)
#' )
#' sir$data_arg()
#' sir$param_arg()
#' sir$simulator()$report()
#'
#' @useDynLib macpan2
#' @importFrom TMB MakeADFun
#' @noRd
TMBModel = function(
      init_mats = MatsList()
    , expr_list = ExprList()
    , params = OptParamsList(0)
    , random = OptParamsList()
    , obj_fn = ObjectiveFunction(~0)
    , time_steps = Time(0L)
    , engine_methods = EngineMethods()
    , log_file = LogFile()
    , do_pred_sdreport = TRUE
  ) {
  ## Inheritance
  self = Base()

  ## Args
  self$expr_list = expr_list
  self$init_mats = init_mats
  self$params = params
  self$random = random
  self$obj_fn = obj_fn
  self$time_steps = time_steps
  self$do_pred_sdreport = do_pred_sdreport
  self$engine_methods = engine_methods
  self$log_file = log_file

  ## Standard Methods
  self$data_arg = function() {
    c(
      self$init_mats$data_arg(),
      self$expr_list$data_arg(),
      self$params$data_arg(),
      self$random$data_arg("r"),
      self$obj_fn$data_arg(),
      self$time_steps$data_arg(),
      self$engine_methods$meth_list$data_arg(),
      self$engine_methods$int_vecs$data_arg(),
      self$log_file$data_arg(),
      list(values_adreport = as.integer(self$do_pred_sdreport))
    )
  }
  self$param_arg = function() {
    p = list(
      params = self$params$vector(),
      random = self$random$vector()
    )
    
    ## FIXME: Need a dummy parameter if the model has not
    ## yet been parameterized. Is there a more TMB-ish
    ## way to do this?
    if (length(p$params) == 0L) p$params = 0
    p
  }
  self$random_arg = function() {
    if (length(self$random$vector()) == 0L) return(NULL)
    return("random")
  }
  self$make_ad_fun_arg = function(
        tmb_cpp = getOption("macpan2_dll")
      , verbose = getOption("macpan2_verbose")
    ) {
    tmb_type_option = getOption("macpan2_tmb_type")
    params = self$param_arg()
    if (identical(tmb_type_option, "Fun")) params$params = numeric()
    args = list(
        data = self$data_arg()
      , parameters = params
      , random = self$random_arg()
      , DLL = tmb_cpp
      , silent = !verbose
    )
    if (!is.null(tmb_type_option)) args$type = tmb_type_option
    return(args)
  }
  self$ad_fun = function(
        tmb_cpp = getOption("macpan2_dll")
      , verbose = getOption("macpan2_verbose")
    ) {
    do.call(TMB::MakeADFun, self$make_ad_fun_arg(tmb_cpp, verbose))
  }

  self$simulator = function(
        tmb_cpp = getOption("macpan2_dll")
      , initialize_ad_fun = TRUE
      , outputs = NULL
    ) {
    TMBSimulator(self
      , tmb_cpp = tmb_cpp
      , initialize_ad_fun = initialize_ad_fun
      , outputs = outputs
    )
  }


  ## Composition
  self$add = TMBAdder(self)
  self$insert = TMBInserter(self)
  self$print = TMBPrinter(self)
  self$replace = TMBReplacer(self)


  ## Dependency management
  self$dependencies = Dependencies(self
      , init_mats = "MatsList"
      , expr_list = "ExprList"
      , obj_fn = "ObjectiveFunction"
      , engine_methods = "EngineMethods"
      , params = "OptParamsList"
      , random = "OptParamsList"
      , time_steps = "Time"
  )
  self$refresh = Refresher(self$dependencies)
  self$refresh$init_mats(self$init_mats)
  self$refresh$expr_list(self$expr_list)
  self$refresh$engine_methods(self$engine_methods)
  self$refresh$obj_fn(self$obj_fn)

  return_object(
    valid$tmb_model$assert(self),
    "TMBModel"
  )
}


#' Default Values
#' 
#' @param model A model object from which to extract default values. If
#' `model` is a calibrator object (see \code{\link{mp_tmb_calibrator}})
#' that has been optimized (using \code{\link{mp_optimize}}), then the values
#' returned by `mp_default` and `mp_default_list` are updated to reflect this
#' calibration/optimization process.
#' @param include_all Include all default variables, even those that are not
#' used in the `before`, `during`, or `after` phase of the simulations.
#' When `include_all` is `FALSE`, examples of excluded variables are
#' those used by an objective function only or those intended to be used in an 
#' extended model specification produced using functions like 
#' \code{\link{mp_tmb_insert}} and \code{\link{mp_tmb_update}}.
#' 
#' @returns A long-format data frame with default values for matrices required
#' as input to model objects. The columns of this output are `matrix`, `row`,
#' `col`, and `value`. Scalar matrices do not have any entries in the `row` or
#' `col` columns.
#' @export
mp_default = function(model, include_all = FALSE) UseMethod("mp_default")

#' @describeIn mp_default List of the default variables as matrices.
#' @export
mp_default_list = function(model, include_all = FALSE) UseMethod("mp_default_list")

#' @export
mp_default.TMBModelSpec = function(model, include_all = FALSE) {
  melt_default_matrix_list(mp_default_list(model, include_all))
}

#' @export
mp_default_list.TMBModelSpec = function(model, include_all = FALSE) {
  default = model$default
  if (!include_all) {
    default_mats = model$all_default_mats()
    default = default[default_mats]
  }
  return(default)
}

#' @export
mp_default.TMBSimulator = function(model, include_all = FALSE) {
  melt_default_matrix_list(mp_default_list(model, include_all))
}

#' @export
mp_default_list.TMBSimulator = function(model, include_all = FALSE) {
  init_mats = model$tmb_model$init_mats
  expr_list = model$tmb_model$expr_list
  int_vecs = model$tmb_model$engine_methods$int_vecs
  update = model$current$update_matrix_list
  default = init_mats$all_matrices()
  if (!include_all) {
    all_default_mats = setdiff(
        expr_list$all_default_vars()
      , int_vecs$const_names()
    )
    default = default[all_default_mats]
  }
  default = update(default)
  return(default)
}

#' @export
mp_default.TMBCalibrator = function(model, include_all = FALSE) {
  mp_default(model$simulator, include_all)
}

#' @export
mp_default_list.TMBCalibrator = function(model, include_all = FALSE) {
  mp_default_list(model$simulator, include_all)
}


#' Initial Values of Variables Immediately Before the Simulation Loop
#' 
#' Return a data frame containing the values of variables at the end of the
#' `before` phase, right before the simulation loop begins (i.e. right before
#' the `during` phase).
#' 
#' @param model A model specification object or model simulator object from 
#' which to extract initial values.
#' @returns A long-format data frame with initial values for matrices. 
#' The columns of this output are `matrix`, `time`, `row`, `col`, and `value`. 
#' Scalar matrices do not have any entries in the `row` or `col` columns. The 
#' `before` phase corresponds to a `time` value of 0.
#' @export
mp_initial = function(model) UseMethod("mp_initial")

#' @describeIn mp_initial List of the initial variables as matrices.
#' @export
mp_initial_list = function(model) UseMethod("mp_initial_list")

#' @export
mp_initial.TMBModelSpec = function(model) {
  spec_initial_util(model, simplify_ids = TRUE)
}

#' @export
mp_initial_list.TMBModelSpec = function(model) {
  (model
    |> spec_initial_util(simplify_ids = FALSE) ## the casting does the simplification
    |> cast_default_matrix_list()
  )
}

#' @export
mp_initial.TMBSimulator = function(model) {
  sim_initial_util(model, simplify_ids = TRUE)
}

#' @export
mp_initial_list.TMBSimulator = function(model) {
  (model
    |> sim_initial_util(simplify_ids = FALSE) 
    |> cast_default_matrix_list()
  )
}

#' @export
mp_initial.TMBCalibrator = function(model) mp_initial(model$simulator)

#' @export
mp_initial_list.TMBCalibrator = function(model) mp_initial_list(model$simulator)

spec_initial_util = function(model, simplify_ids = TRUE) {
  # warning("under construction")
  ## Should this just be whatever the report returns in the before phase?
  ## And in this way be analogous to mp_final?  I think so.
  all_derived_mats_in_before_step = setdiff(
      ExprList(model$update_method$before())$all_derived_vars()
    , names(model$all_integers())
  )
  outputs = c(
      model$all_default_vars()
    , all_derived_mats_in_before_step
  ) |> unique()
  r = mp_simulator(model, time_steps = 0L, outputs)$report(.phases = "before")
  r$time = NULL
  if (simplify_ids) r = simplify_row_col_ids(r)
  return(r)
}
sim_initial_util = function(model, simplify_ids = TRUE) {
  init_mats = model$tmb_model$init_mats
  int_vecs = model$tmb_model$engine_methods$int_vecs
  expr_list = model$tmb_model$expr_list
  
  update = model$current$update_matrix_list
  
  before_expr_list = ExprList(expr_list$before)
  
  all_ints = names(int_vecs$list)
  defaults = setdiff(expr_list$all_default_vars(), all_ints)
  
  outputs = (defaults
    |> c(before_expr_list$all_derived_vars())
    |> unique()
    |> setdiff(all_ints)
  )
  
  spec = mp_tmb_model_spec(
      before = expr_list$before
    , default = init_mats$all_matrices()[defaults] |> update()
    , integers = int_vecs$list
  )
  
  r = mp_simulator(spec, time_steps = 0L, outputs)$report(.phases = "before")
  r$time = NULL
  if (simplify_ids) r = simplify_row_col_ids(r)
  return(r)
}


#' Final Values
#' 
#' Return the values of variables after the simulation loop has finished
#' and the `final` set of expressions have been evaluated.
#' 
#' @param model Object that can be used to simulate.
#' @export
mp_final = function(model) UseMethod("mp_final")

#' @describeIn mp_final Final values formatted as a list of matrices.
#' @export
mp_final_list = function(model) UseMethod("mp_final_list")


#' @export
mp_final.TMBSimulator = function(model) model$report(.phases = "after")

#' @export
mp_final.TMBCalibrator = function(model) mp_final(model$simulator)

#' @export
mp_final_list.TMBSimulator = function(model) {
  mp_final(model) |> cast_default_matrix_list()
}

#' Simulate Dynamical Model Trajectories
#' 
#' Return simulations of the trajectory of the output
#' variables of a dynamical model simulator. To see this functionality 
#' in context, please see `vignette("quickstart")`.
#' 
#' @param model A dynamical model simulator produced by
#' \code{\link{mp_simulator}}.
#' @param include_initial Should the initial values of the simulation be 
#' included in the output? If `TRUE` this will include outputs for `time == 0`
#' associated with the initial values. See \code{\link{mp_initial}} for another 
#' approach to getting the initial values.
#' @param include_final Should the final values of the simulation, after the
#' post-simulation processing steps in the `after` stage of a model, be 
#' included in the output? If `TRUE` this will include outputs for 
#' `time == time_steps + 1`, associated with the values of the variables
#' after the full trajectory has been post-processed in the `after` stage.
#' See \code{\link{mp_final}} for another approach to getting the final values.
#' 
#' @returns A data frame with one row for each simulated value and the following
#' columns.
#' \describe{
#'   \item{matrix}{
#'      Name of the variable in the model. All variables are matrix-valued
#'      in `macpan2` (scalars are technically 1-by-1 matrices), which explains
#'      the name of this field. In hindsight I would have called it `variable`.
#'   }
#'   \item{time}{
#'      Time index of the simulated value, with `time = 0` indicating initial 
#'      values.
#'   }
#'   \item{row}{
#'      The 0-based index of the row of the matrix, or the name of the row
#'      of the matrix if row names (or names for column vectors) are supplied 
#'      for the default value of the matrix.
#'   }
#'   \item{col}{
#'      The 0-based index of the column of the matrix, or the name of the column
#'      of the matrix if column names are supplied for the default value of the 
#'      matrix. It is also possible that this column is blank if everything
#'      is either a scalar or column vector (a common case).
#'    }
#'   \item{value}{(`mp_trajectory` and `mp_trajectory_sd`) Simulation values.}
#'   \item{sd}{
#'      (for `mp_trajectory_sd` only) 
#'      The standard deviations of the simulated values accounting for parameter
#'      estimation uncertainty.
#'   }
#'   \item{conf.low}{
#'      (for `mp_trajectory_sd` only)
#'      The lower bounds of the confidence interval for the simulated values.
#'   }
#'   \item{conf.high}{
#'      (for `mp_trajectory_sd` only)
#'      The upper bounds of the confidence interval for the simulated values.
#'    }
#'   \item{n%}{
#'      (for `mp_trajectory_[ensemble|sim]`)
#'      The n-th quantiles of the simulation values over repeated simulations.
#'   }
#' }
#' 
#' @examples
#' spec = mp_tmb_library("starter_models"
#'   , "si"
#'   , package = "macpan2"
#' )
#' simulator = mp_simulator(spec
#'   , time_steps = 10L
#'   , outputs = c("infection", "I")
#' )
#' trajectory = mp_trajectory(simulator)
#' print(trajectory)
#' 
#' @export
mp_trajectory = function(model, include_initial = FALSE) {
  UseMethod("mp_trajectory")
}

resolve_phases = function(include_initial) {
  phases = "during"
  if (include_initial) phases = c("before", "during")
  return(phases)
}

#' @export
mp_trajectory.TMBSimulator = function(model, include_initial = FALSE) {
  phases = resolve_phases(include_initial)
  model$report(.phases = phases) |> reset_rownames()
}

#' @export
mp_trajectory.TMBCalibrator = function(model, include_initial = FALSE) {
  traj = mp_trajectory(model$simulator, include_initial = include_initial)
  traj$time = model$time_steps_obj$internal_to_external(traj$time)
  return(traj)
} 


#' @param parameter_updates Named list of a subset of model variables with
#' the values to use when simulating the trajectory using the 
#' `mp_trajectory_par` function. In the future we plan
#' to allow this variable to be a data frame with one row for each scalar value
#' (which would be useful if only certain elements of a vector
#' or matrix are parameters) and a string giving the name of a file containing
#' parameter information. But for now, only a list is allowed.
#' @param baseline Models can contain several alternative sets of 
#' parameters, and this `baseline` argument is used to choose which of these
#' should be updated using the `parameter_updates` passed to 
#' `mp_trajectory_par`. The current options are `"recommended"`, `"optimized"`,
#' and `"default"`. The `"recommended"` option will be used if neither of the
#' other two options are selected. If `model` is capable of being optimized
#' (e.g., it was created using \code{\link{mp_tmb_calibrator}}) then 
#' `"recommended"` is equivalent to `"optimized"`, which use the best set of 
#' parameters found by \code{\link{mp_optimize}}. If \code{\link{mp_optimize}} 
#' has not yet been called on `model` then a warning will be issued. If
#' `model` is not capable of being optimized then `"recommended"` is
#' equivalent to `"default"`, which uses the original set of parameters 
#' available when `model` was created.
#' @describeIn mp_trajectory Produce a trajectory, after updating the `baseline`
#' set of parameters with values in `parameter_updates`.
#' @export
mp_trajectory_par = function(model, parameter_updates = list()
    , include_initial = FALSE, include_final = FALSE
    , baseline = c("recommended", "default", "optimized")
  ) {
  UseMethod("mp_trajectory_par")
}

# take a simulator and return the trajectory data frame
trajectory_par_util = function(simulator
    , parameter_updates, value_column_name
    , include_initial = FALSE, include_final = FALSE
  ) {
  phases = trajectory_phases_util(include_initial, include_final)
  vector = trajectory_vec_util(simulator, parameter_updates, value_column_name)
  simulator$simulate(vector, .phases = phases)
}

trajectory_rep_util = function(n, simulator
    , parameter_updates, value_column_name
    , include_initial = FALSE, include_final = FALSE
  ) {
  phases = trajectory_phases_util(include_initial, include_final)
  vector = trajectory_vec_util(simulator, parameter_updates, value_column_name)
  replicates = replicate(n
    , simulator$simulate(vector, .phases = phases)
    , simplify = FALSE
  )
  return(replicates)
}

# take a simulator and return the parameter vector that can be
# passed to TMB report and simulate
trajectory_vec_util = function(simulator, parameter_updates, value_column_name) {
  sc = simulator$current
  frame = bind_rows(sc$params_frame(), sc$random_frame())
  vector = updated_param_vector(parameter_updates
    , frame
    , matrix = "mat", value = value_column_name
  )
  return(vector)
}

trajectory_phases_util = function(include_initial = FALSE, include_final = FALSE) {
  phases = "during"
  if (include_initial) phases = c("before", phases)
  if (include_final) phases = c(phases, "after")
  return(phases)
}

value_column_simulator_util = function(baseline) {
  value_column_name = switch(baseline
    , recommended = "default"
    , default = "default"
    , optimized = "current"
  )
  if (value_column_name == "current") {
    mp_wrap(
        "The model object being simulated from is intended to be optimized. "
      , "Please use mp_tmb_calibrator to produce an object that can be"
      , "calibrated/optimized."
    ) |> warning()
  }
  return(value_column_name)
}

value_column_calibrator_util = function(baseline, simulator) {
  value_column_name = switch(baseline
    , recommended = "current"
    , default = "default"
    , optimized = "current"
  )
  # opt_attempted = simulator$optimization_history$opt_attempted()
  # if ((value_column_name == "current") & !opt_attempted) {
  #   mp_wrap(
  #       "The model object has not been optimized, and so the default"
  #     , "(non-optimized) parameter set will be used as the baseline."
  #     , "Please either explicitly choose"
  #     , "to use the default set of parameters as the baseline, or optimize"
  #     , "the model object using mp_optimize(model, ...)."
  #   ) |> warning()
  # }
  return(value_column_name)
}

#' @export
mp_trajectory_par.TMBSimulator = function(model, parameter_updates = list()
    , include_initial = FALSE, include_final = FALSE
    , baseline = c("recommended", "default", "optimized")
  ) {
  baseline = match.arg(baseline)
  value_column_name = value_column_simulator_util(baseline)
  trajectory_par_util(model
    , parameter_updates, value_column_name
    , include_initial, include_final
  )
}


#' @export
mp_trajectory_par.TMBCalibrator = function(model, parameter_updates = list()
    , include_initial = FALSE, include_final = FALSE
    , baseline = c("recommended", "default", "optimized")
  ) {
  baseline = match.arg(baseline)
  simulator = model$simulator
  value_column_name = value_column_calibrator_util(baseline, simulator)
  traj = trajectory_par_util(simulator
    , parameter_updates, value_column_name
    , include_initial, include_final
  )
  traj$time = model$time_steps_obj$internal_to_external(traj$time)
  return(traj)
}

#' @param conf.int Should confidence intervals be produced?
#' @param conf.level If `conf.int` is `TRUE`, what confidence level should be
#' used?  For example, the default of `0.95` corresponds to 95% confidence
#' intervals.
#' @param back_transform A boolean to indicate if trajectories, standard
#' deviations, and confidence intervals should be back transformed to 
#' the original scale. Variable names are also stripped of their
#' transformation identifier. Currently, this back transformation only 
#' applies to log transformed coefficients that have been named with "log_" 
#' prefix or logit transformed coefficients that have been named with "logit_" 
#' prefix. Back transformation also applies to time varying parameters and 
#' distributional parameters that get automatic prefixes when used. 
#' `back_transform` defaults to `TRUE`.
#' @describeIn mp_trajectory Simulate a trajectory that includes uncertainty
#' information provided by the `sdreport` function in `TMB` with default
#' settings.
#' @export
mp_trajectory_sd = function(model
    , conf.int = FALSE
    , conf.level = 0.95
    , include_initial = FALSE
    , back_transform = TRUE
  ) {
  UseMethod("mp_trajectory_sd")
}

#' @param n Number of samples used in `mp_trajectory_ensemble`.
#' @param probs What quantiles should be returned by `mp_trajectory_ensemble`.
#' @describeIn mp_trajectory Simulate a trajectory that includes uncertainty
#' information provided by repeatedly sampling from a normal approximation to the 
#' distribution of the fitted parameters, and generating one trajectory for
#' each of these samples. The quantiles of the empirical distribution of these
#' trajectories can be used to produce a confidence interval for the 
#' fitted trajectory.
#' @export
mp_trajectory_ensemble = function(model, n, probs = c(0.025, 0.975)) {
  UseMethod("mp_trajectory_ensemble")
}
  
#' @importFrom stats qnorm
#' @export
mp_trajectory_sd.TMBSimulator = function(model
    , conf.int = FALSE
    , conf.level = 0.95
    , include_initial = FALSE
    , back_transform = TRUE
  ) {
  phases = resolve_phases(include_initial)
  alpha = (1 - conf.level) / 2
  best_pars = get_last_best_par(model$ad_fun())
  r = model$report_with_sd(best_pars, .phases = phases)
  if (conf.int) {
    r$conf.low = r$value + r$sd * qnorm(alpha)
    r$conf.high = r$value + r$sd * qnorm(1 - alpha)
  }
  if (back_transform) {
    vars = intersect(c("value", "conf.low", "conf.high"), names(r))
    r = backtrans(r, vars, "matrix", "sd", "value")
  }
  r
} 

#' @export
mp_trajectory_sd.TMBCalibrator = function(model
    , conf.int = FALSE
    , conf.level = 0.95
    , include_initial = FALSE
    , back_transform = TRUE
  ) {
  traj = mp_trajectory_sd(model$simulator, conf.int, conf.level, include_initial, back_transform)
  traj$time = model$time_steps_obj$internal_to_external(traj$time)
  return(traj)
}

#' @export
mp_trajectory_ensemble.TMBSimulator = function(model, n, probs = c(0.025, 0.975)) {
  best_pars = get_last_best_par(model$ad_fun())
  traj = model$report_ensemble(best_pars, .n = n, .probs = probs)
  return(traj)
}

#' @export
mp_trajectory_ensemble.TMBCalibrator = function(model, n, probs = c(0.025, 0.975)) {
  traj = mp_trajectory_ensemble(model$simulator, n, probs)
  traj$time = model$time_steps_obj$internal_to_external(traj$time)
  return(traj)
}


## still not really sure how to construct a single
## ad_fun that works both with process error simulation and deterministic 
## trajectory matching



##' @param n Number of random trajectories to simulate.
##' @param probs Numeric vector of probabilities corresponding to quantiles for 
##' summarizing the results over the random realizations.
##' 
##' @describeIn mp_trajectory Generate quantiles over `n` realizations of 
##' the trajectory. Instead of a `value` column in the output data frame, there
##' is one column for each of the quantiles defined in `probs`.
##' @export
mp_trajectory_sim = function(model, n, probs = c(0.025, 0.25, 0.5, 0.75, 0.975)) {
  UseMethod("mp_trajectory_sim")
}

##' @describeIn mp_trajectory Generate a list of `n` simulation results.
##' @export
mp_trajectory_replicate = function(model, n
    , parameter_updates = list()
    , include_initial = FALSE, include_final = FALSE
    , baseline = c("recommended", "default", "optimized")
  ) {
  if (!mp_generates_randomness(model)) {
    warning("Model does not include functions that generate randomness, and so replicate trajectories are not informative.")
  }
  UseMethod("mp_trajectory_replicate")
}

#' @export
mp_trajectory_replicate.TMBSimulator = function(model, n
    , parameter_updates = list()
    , include_initial = FALSE, include_final = FALSE
    , baseline = c("recommended", "default", "optimized")
  ) {
  baseline = match.arg(baseline)
  value_column_name = value_column_simulator_util(baseline)
  trajectory_rep_util(n, model
    , parameter_updates, value_column_name
    , include_initial, include_final
  )
}

#' @export
mp_trajectory_replicate.TMBCalibrator = function(model, n
    , parameter_updates = list()
    , include_initial = FALSE, include_final = FALSE
    , baseline = c("recommended", "default", "optimized")
  ) {
  baseline = match.arg(baseline)
  simulator = model$simulator
  value_column_name = value_column_calibrator_util(baseline, simulator)
  trajectory_rep_util(n, simulator
    , parameter_updates, value_column_name
    , include_initial, include_final
  )
}

##' @export
mp_trajectory_sim.TMBSimulator = function(model, n, probs = c(0.025, 0.25, 0.5, 0.75, 0.975)) {
  r = model$simulate()
  r = r[, names(r) != "value", drop = FALSE]
  rr = (n
    |> replicate(model$simulate_values()) 
    |> apply(1, quantile, probs, na.rm = TRUE)
    |> t()
  )
  names(rr) = sprintf("value_%s", names(rr))
  cbind(r, rr)
}

##' @export
mp_trajectory_sim.TMBCalibrator = function(model, n, probs = c(0.025, 0.25, 0.5, 0.75, 0.975)) {
  stop("Under construction")
}


TMBDynamicSimulator = function(tmb_simulator, dynamic_model) {
  self = tmb_simulator
  self$dynamic_model = dynamic_model
  return_object(self, "TMBDynamicSimulator")
}

#' @export
labels.VariableLabels = function(object, ...) object$component_list()



#' @export
labels.ModelDefRun = function(object, ...) {
  labels(object$labels)
}

#' @export
labels.TMBDynamicSimulator = function(object, ...) {
  labels(object$dynamic_model)
}

#' @export
labels.DynamicModel = function(object, ...) {
  labels(object$labels)
}

#' @export
labels.LabelsDynamic = function(object, ...) {
  object$component_list()
}

#' @export
labels.LabelsScripts = function(object, ...) {
  object$component_list()
}

TMBSimulationUtils = function() {
  self = Base()
  self$.simulation_formatter = function(r, .phases) {
    ## get raw simulation output from TMB and supply 
    ## column names (which don't exist on the TMB side)
    col_names = c("matrix", "time", "row", "col", "value")
    if (ncol(r$values) == 6L) col_names = append(col_names, "sd")
    r = setNames(as.data.frame(r$values), col_names)
    r$matrix = self$matrix_names()[r$matrix + 1L]  ## replace matrix indices with matrix names
    dn = self$tmb_model$init_mats$dimnames()  ## get the row and column names of matrices with such names
    for (mat in names(dn)) {
      i = r$matrix == mat

      ## convert to 1-based indices for R users
      row_indices = as.integer(r[i,"row"]) + 1L
      col_indices = as.integer(r[i,"col"]) + 1L

      ## add row and column names if available
      r[i, "row"] = dn[[mat]][[1L]][row_indices]
      r[i, "col"] = dn[[mat]][[2L]][col_indices]

      ## if some of the row and column names are unavailable,
      ## replace with indices -- this is important for the use case
      ## where a named matrix changes shape/size, beacuse row and column
      ## names can be set for the initial shape/size
      missing_row_nms = is.na(r[i, "row"])
      missing_col_nms = is.na(r[i, "col"])
      r[i, "row"][missing_row_nms] = as.character(row_indices[missing_row_nms])
      r[i, "col"][missing_col_nms] = as.character(col_indices[missing_col_nms])
    }
    r$time = as.integer(r$time)
    num_t = self$tmb_model$time_steps$time_steps
    if (!"before" %in% .phases) {
      r = r[r$time != 0L,,drop = FALSE]
    }
    if (!"during" %in% .phases) {
      r = r[(r$time == 0L) | (r$time == num_t + 1L),,drop = FALSE]
    }
    if (!"after" %in% .phases) {
      r = r[r$time != num_t + 1L,,drop = FALSE]
    }
    r |> filter(
        (matrix %in% self$matrix_outputs())
      | (row %in% self$row_outputs())
    )
  }
  self$.find_problematic_expression = function(row) {
    expr_num_p_table_rows = self$tmb_model$data_arg()$expr_num_p_table_rows
    expr_num = min(which(row < cumsum(expr_num_p_table_rows)))
    deparse1(self$tmb_model$expr_list$formula_list()[[expr_num]])
  }
  self$.runner = function(...
      , .phases = "during"
      , .method = c("report", "simulate", "sdreport")
      , .sort = TRUE
      , .values_only = FALSE
  ) {
    .method = match.arg(.method)
    compute_sd = FALSE
    if (.method == "sdreport") {
      .method = "report"
      compute_sd = TRUE
    } 
    fixed_params = as.numeric(unlist(list(...)))
    if (length(fixed_params) == 0L) {
      r = self$ad_fun()[[.method]]()
    } else {
      r = self$ad_fun()[[.method]](fixed_params)
    }
    if (r$error != 0L) {
      arg_report = (data.frame(
          Rows = r$arg_rows
        , Cols = r$arg_cols
        , Types = c("double", "integer")[r$arg_type_ints + 1L]
      )) |> frame_formatter()
      
      stop(
        "\nThe following error was thrown by the TMB engine:\n  ",
        self$tmb_model$log_file$err_msg(),
        "\nThis error occurred at the following expression:\n  ",
        self$.find_problematic_expression(r$expr_row),
        "\nThis error occurred at time-step ", r$time_int, 
        " in the following function:\n  ", unname(valid_funcs[[r$func_int]]),
        "\nThis function call got ", length(r$arg_rows), " arguments, ",
        "each of which is described by the rows of the following table:\n\n",
        arg_report
      )
    }
    if (compute_sd) r$values = cbind(r$values, self$sdreport()$sd)
    if (.values_only) return(r$values)
    s = self$.simulation_formatter(r, .phases)
    if (.sort) {
      s = s[order(s$time), , drop = FALSE] ## TODO: move sorting by time to the c++ side
    }
    reset_rownames(s)
  }
  return_object(self, "TMBSimulationFormatter")
}

#' TMB Simulator
#'
#' Construct an object with methods for simulating from and optimizing a
#' compartmental model made using \code{\link{TMBModel}}.
#'
#' @param tmb_model An object of class \code{\link{TMBModel}}.
#' @param tmb_cpp Name of a C++ program using TMB as the simulation engine.
#' @param initialize_ad_fun Should the TMB AD function be intialized? This
#' should usually be set to \code{TRUE} unless you want to hack the data
#' structure passed to TMB (which can be acquired using
#' `$tmb_model$make_ad_fun_arg()`) before passing it yourself to
#' `TMB::MakeADFun`. This is particularly useful if you want to modify
#' `tmb_cpp`.
#' @param outputs Character vector of matrix and/or row names to be
#' returned as output from simulation functions such as
#' \code{\link{mp_trajectory}}.
#'
#' @return Object of class \code{TMBSimulator} with the following methods.
#'
#' ## Methods
#'
#' * `$report()`: Runs simulations and returns a data frame with the following
#' columns.
#'     * `matrix`: Name of the matrix with values returned.
#'     * `time`: Time step of the values.
#'     * `row`: Row in the `matrix` containing the `value`.
#'     * `col`: Column in the `matrix` containing the `value`.
#'     * `value`: Numerical value being reported.
#' * `$error_code()`: If the simulations result in an engine error then the
#' code associated with this error is returned, otherwise the code `0` is
#' returned.
#' * `$ad_fun()`: Return the underlying [TMB](https://github.com/kaskr/adcomp)
#' object.
#'
#' @importFrom MASS mvrnorm
#' @importFrom stats quantile
#' @noRd
TMBSimulator = function(tmb_model
    , tmb_cpp = getOption("macpan2_dll")
    , initialize_ad_fun = TRUE
    , outputs = NULL
  ) {
  self = TMBSimulationUtils()

  ## Args
  self$tmb_model = tmb_model
  self$tmb_cpp = tmb_cpp
  self$outputs = outputs
  
  self$matrix_outputs = function() {
    if (is.null(self$outputs)) return(self$tmb_model$init_mats$.mats_to_return)
    initial_mats = self$tmb_model$init_mats$.initial_mats
    intersect(self$outputs, names(initial_mats))
  }
  self$row_outputs = function() {
    if (is.null(self$outputs)) return(character(0L))
    setdiff(self$outputs, self$matrix_outputs())
  }
  
  ## Standard Methods
  self$matrix_names = function() self$tmb_model$init_mats$.names()
  self$ad_fun = function() self$tmb_model$ad_fun(self$tmb_cpp)

  self$objective = function(...) {
    ## need to pass named vector or sdreport stops identifying
    ## fixed effects as `params`
    fixed_params = rep_name(as.numeric(unlist(list(...))), "params")
    self$ad_fun()$fn(fixed_params)
  }
  self$gradient = function(...) {
    fixed_params = rep_name(as.numeric(unlist(list(...))), "params")
    self$ad_fun()$gr(fixed_params)
  }
  self$hessian = function(...) {
    fixed_params = rep_name(as.numeric(unlist(list(...))), "params")
    self$ad_fun()$he(fixed_params)
  }
  self$error_code = function(...) self$ad_fun()$report(...)$error
  self$sdreport = function() TMB::sdreport(self$ad_fun(), getReportCovariance = FALSE)
  self$cov.fixed = function() self$sdreport()$cov.fixed
  self$par.fixed = function() self$sdreport()$par.fixed
  self$report = function(..., .phases = "during") {
    self$.runner(..., .phases = .phases, .method = "report")
  }
  self$report_with_sd = function(..., .phases = "during") {
    self$.runner(..., .phases = .phases, .method = "sdreport")
  }
  self$report_values = function(..., .phases = "during") {
    self$report(..., .phases = .phases)$value
  }
  self$simulate_values = function(..., .phases = "during") {
    self$simulate(..., .phases = .phases)$value
  }
  self$report_ensemble = function(...
      , .phases = "during"
      , .n = 100
      , .probs = c(0.025, 0.5, 0.975)
    ) {
    r = self$report(..., .phases = .phases)
    ff = self$par.fixed()
    cc = self$cov.fixed()
    if (isFALSE(!any(is.nan(cc)))) {
      stop("The covariance matrix of the fixed effects has NaNs. Perhaps this model has not yet been calibrated or even parameterized? Or perhaps the fit is singular?")
    }
    rr = (MASS::mvrnorm(.n, ff, cc)
      |> apply(1, self$report_values, .phases = .phases)
      |> apply(1, quantile, probs = .probs)
      |> t()
    )
    cbind(r, rr)
  }
  self$simulate = function(..., .phases = "during") {
    self$.runner(..., .phases = .phases, .method = "simulate")
  }
  self$matrix = function(..., matrix_name, time_step, .phases = "during") {
    r = self$report(..., .phases = .phases)
    i = (r$matrix == as.character(matrix_name)) & (r$time == as.integer(time_step))
    rr = r[i, c("row", "col", "value")]
    if (!any(is.na(as.integer(rr$row)))) {
      return(matrix(rr$value, max(as.integer(rr$row)) + 1L))
    }
    matrix(rr$value, max(rr$row) + 1L)
  }

  ## Composition
  self$optimize = TMBOptimizer(self)
  self$optimization_history = TMBOptimizationHistory(self)
  self$print = TMBSimulatorPrinter(self)
  self$insert = TMBSimulatorInserter(self)
  self$add = TMBSimulatorAdder(self)
  self$delete = TMBSimulatorDeleter(self)
  self$replace = TMBSimulatorReplacer(self)
  self$update = TMBSimulatorUpdater(self)
  self$reset = TMBSimulatorResetter(self)
  self$current = TMBCurrentParams(self)
  self$get = TMBSimulatorGetters(self)

  initialize_cache(self, "ad_fun", "sdreport")
  # initialize_cache(self, "ad_fun")
  if (initialize_ad_fun) {
    if (inherits(self$ad_fun(), "try-error")) {
      stop(
        "\nThe tmb_model object is malformed,",
        "\nwith the following explanation:\n",
        self$ad_fun()
      )
    }
  }
  return_object(self, "TMBSimulator")
}

#' @export
print.TMBSimulator = function(x, ...) {
  m = x$tmb_model
  time_steps = m$time_steps$time_steps
  printer = m$expr_list$print_exprs
  printer(time_steps = time_steps)
  invisible(x)
}
