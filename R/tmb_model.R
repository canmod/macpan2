#' Simulator
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
#' 
#' @export
mp_simulator = function(model
    , time_steps
    , outputs
    , default = list()
  ) {
  UseMethod("mp_simulator")
}

#' @export
mp_simulator.TMBModelSpec = function(model
    , time_steps
    , outputs
    , default = list()
  ) {
  model$simulator_fresh(time_steps, outputs, default)
}

#' @export
mp_simulator.TMBParameterizedModelSpec = function(model
  , time_steps, outputs, default = list()
) {
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
    list(
      params = self$params$vector(),
      random = self$random$vector()
    )
  }
  self$random_arg = function() {
    if (length(self$random$vector()) == 0L) return(NULL)
    return("random")
  }
  self$make_ad_fun_arg = function(
        tmb_cpp = getOption("macpan2_dll")
      , verbose = getOption("macpan2_verbose")
    ) {
    params = self$param_arg()
    if (getOption("macpan2_tmb_type") == "Fun") params$params = numeric()
    list(
        data = self$data_arg()
      , parameters = params
      , random = self$random_arg()
      , DLL = tmb_cpp
      , silent = !verbose
      , type = getOption("macpan2_tmb_type")
      , checkParameterOrder = isTRUE(getOption("macpan2_tmb_check"))
    )
  }
  self$ad_fun = function(
        tmb_cpp = getOption("macpan2_dll")
      , verbose = getOption("macpan2_verbose")
      , derivs = getOption("macpan2_tmb_derivs")
    ) {
    do.call(TMB::MakeADFun, self$make_ad_fun_arg(tmb_cpp))
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
#' @param model A model object from which to extract default values.
#' @returns A long-format data frame with default values for matrices required
#' as input to model objects. The columns of this output are `matrix`, `row`,
#' `col`, and `value`. Scalar matrices do not have any entries in the `row` or
#' `col` columns.
#' @export
mp_default = function(model) UseMethod("mp_default")

#' @export
mp_default.TMBModelSpec = function(model) {
  melt_default_matrix_list(model$default)
}

#' @export
mp_default.TMBSimulator = function(model) {
  init_mats = model$tmb_model$init_mats$.initial_mats
  default_mats = init_mats[!vapply(init_mats, is_empty_matrix, logical(1L))]
  melt_default_matrix_list(default_mats)
}

mp_initial = function(model_simulator, ...) {
  stop("under construction")
  UseMethod("mp_initial")
}

mp_initial.TMBSimulator = function(model_simulator, matrices, params = NULL) {
  stop("under construction")
  (model_simulator
    $replace
    $time_steps(time_steps)
    $update
    $matrices(.mats_to_return = matrices, .mats_to_save = matrices)
    $report(params, .phases = "before")
  )
}


mp_final = function(model_simulator, ...) {
  stop("under construction")
  UseMethod("mp_final")
}


mp_final.TMBSimulator = function(model_simulator, time_steps, outputs, ...) {
  stop("under construction")
  (model_simulator
    $replace
    $time_steps(time_steps)
    $update
    $matrices(.mats_to_return = outputs, .mats_to_save = outputs)
    $report(..., .phases = "after")
  )
}

#' Trajectory
#' 
#' Return simulations of the trajectory of the output
#' variables of a dynamical model simulator.
#' 
#' @param model A dynamical model simulator produced by
#' \code{\link{mp_simulator}}.
#' 
#' @export
mp_trajectory = function(model) {
  UseMethod("mp_trajectory")
}

#' @export
mp_trajectory.TMBSimulator = function(model) {
  model$report() |> reset_rownames()
}

#' @export
mp_trajectory.TMBCalibrator = function(model) mp_trajectory(model$simulator)


#' @param conf.int Should confidence intervals be produced?
#' @param conf.level If `conf.int` is `TRUE`, what confidence level should be
#' used?  For example, the default of `0.95` corresponds to 95% confidence
#' intervals.
#' @describeIn mp_trajectory Simulate a trajectory that includes uncertainty
#' information provided by the `sdreport` function in `TMB` with default
#' settings.
#' @export
mp_trajectory_sd = function(model, conf.int = FALSE, conf.level = 0.95) {
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
mp_trajectory_sd.TMBSimulator = function(model, conf.int = FALSE, conf.level = 0.95) {
  alpha = (1 - conf.level) / 2
  r = model$report_with_sd()
  if (conf.int) {
    r$conf.low = r$value + r$sd * qnorm(alpha)
    r$conf.high = r$value + r$sd * qnorm(1 - alpha)
  }
  r
} 

#' @export
mp_trajectory_sd.TMBCalibrator = function(model, conf.int = FALSE, conf.level = 0.95) {
  mp_trajectory_sd(model$simulator, conf.int, conf.level)
}

#' @export
mp_trajectory_ensemble.TMBSimulator = function(model, n, probs = c(0.025, 0.975)) {
  model$report_ensemble(.n = n, .probs = probs)
}

#' @export
mp_trajectory_ensemble.TMBCalibrator = function(model, n, probs = c(0.025, 0.975)) {
  mp_trajectory_ensemble(model$simulator, n, probs)
}


## not ready to export yet because we are not sure how to construct a single
## ad_fun that works both with process error simulation and deterministic 
## trajectory matching


##' Random Trajectory Simulations
##' 
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
      stop(
        "\nThe following error was thrown by the TMB engine:\n  ",
        self$tmb_model$log_file$err_msg(),
        "\nThis error occurred at the following expression:\n  ",
        self$.find_problematic_expression(r$expr_row)
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
    fixed_params = as.numeric(unlist(list(...)))
    self$ad_fun()$fn(fixed_params)
  }
  self$gradient = function(...) {
    fixed_params = as.numeric(unlist(list(...)))
    self$ad_fun()$gr(fixed_params)
  }
  self$hessian = function(...) {
    fixed_params = as.numeric(unlist(list(...)))
    self$ad_fun()$he(fixed_params)
  }
  self$error_code = function(...) self$ad_fun()$report(...)$error
  self$sdreport = function() TMB::sdreport(self$ad_fun())
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
    rr = (MASS::mvrnorm(.n, self$par.fixed(), self$cov.fixed())
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
  self$replace = TMBSimulatorReplacer(self)
  self$update = TMBSimulatorUpdater(self)
  self$reset = TMBSimulatorResetter(self)
  self$current = TMBCurrentParams(self)
  self$get = TMBSimulatorGetters(self)

  initialize_cache(self, "ad_fun", "sdreport")
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
