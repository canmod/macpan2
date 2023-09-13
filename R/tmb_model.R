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
#' @param meth_list An object of class \code{\link{MethList}}.
#' @param const_int_vecs An object of class \code{\link{ConstIntVecs}}.
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
#'       state ~ state - rowSums(flowmat) + t(colSums(flowmat))
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
#' @export
TMBModel = function(
     init_mats = MatsList()
    , expr_list = ExprList()
    , params = OptParamsList(0)
    , random = OptParamsList()
    , obj_fn = ObjectiveFunction(~0)
    , time_steps = Time(0L)
    , meth_list = MethList()
    , const_int_vecs = ConstIntVecs()
    , log_file = LogFile()
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
  self$meth_list = meth_list
  self$const_int_vecs = const_int_vecs
  self$log_file = log_file

  ## Standard Methods
  self$data_arg = function() {
    existing_literals = self$expr_list$.literals()
    c(
      self$init_mats$data_arg(),
      self$expr_list$data_arg(),
      self$params$data_arg(),
      self$random$data_arg("r"),
      self$obj_fn$data_arg(existing_literals),
      self$time_steps$data_arg(),
      self$meth_list$data_arg(),
      self$const_int_vecs$data_arg(),
      self$log_file$data_arg()
    )
  }
  self$param_arg = function() {
    p = list(
      params = self$params$vector(),
      random = self$random$vector()
    )
    if (length(p$params) == 0L) p$params = 0
    p
  }
  self$random_arg = function() {
    if (length(self$random$vector()) == 0L) return(NULL)
    return("random")
  }
  self$make_ad_fun_arg = function(tmb_cpp = "macpan2") {
    list(
        data = self$data_arg(),
        parameters = self$param_arg(),
        random = self$random_arg(),
        DLL = tmb_cpp
    )
  }
  self$ad_fun = function(tmb_cpp = "macpan2") {
    do.call(TMB::MakeADFun, self$make_ad_fun_arg(tmb_cpp))
  }

  self$simulator = function(tmb_cpp = "macpan2", initialize_ad_fun = TRUE) {
    TMBSimulator(self, tmb_cpp = tmb_cpp, initialize_ad_fun = initialize_ad_fun)
  }


  ## Composition
  self$add = TMBAdder(self)
  self$insert = TMBInserter(self)
  self$print = TMBPrinter(self)
  self$replace = TMBReplacer(self)


  ## Refreshments
  self$refresh_init_mats = function(init_mats) {
    self$init_mats = init_mats
    self$expr_list$init_mats = init_mats
    self$obj_fn$init_mats = init_mats
    self$params$init_mats = init_mats
    self$random$init_mats = init_mats
    for (m in self$meth_list$methods) m$init_mats = init_mats
  }
  self$refresh_expr_list = function(expr_list) {
    self$expr_list = expr_list
    self$refresh_init_mats(self$init_mats)
  }
  self$refresh_obj_fn = function(obj_fn) {
    self$obj_fn = obj_fn
    self$refresh_init_mats(self$init_mats)
  }
  self$refresh_params = function(params) {
    self$params = params
    self$refresh_init_mats(self$init_mats)
  }
  self$refresh_random = function(random) {
    self$random = random
    self$refresh_init_mats(self$init_mats)
  }
  self$refresh_const_int_vecs = function(const_int_vecs) {
    self$const_int_vecs = const_int_vecs
    for (m in self$meth_list$methods) m$const_int_vecs = const_int_vecs
  }
  self$refresh_meth_list = function(meth_list) {
    self$meth_list = meth_list
    self$expr_list$meth_list = meth_list
    self$obj_fn$meth_list = meth_list
    self$refresh_const_int_vecs(self$const_int_vecs)
    self$refresh_init_mats(self$init_mats)
  }
  self$refresh_init_mats(self$init_mats)
  self$refresh_meth_list(self$meth_list)

  return_object(
    valid$tmb_model$assert(self),
    "TMBModel"
  )
}


TMBCompartmentalSimulator = function(tmb_simulator, compartmental_model) {
  self = tmb_simulator
  self$compartmental_model = compartmental_model
  return_object(self, "TMBCompartmentalSimulator")
}

# SimulatorsList = function() {
#   self = Base()
#   self$.simulators = list()
#   self$simulators = function() self$.simulators
#   self$add = function(simulator) {
#     if (!any(vapply(self$.simulators, identical, logical, simulator))) {
#       self$.simulators = append(self$.simulators, simulator)
#     }
#   }
#   return_object(self, "SimulatorsList")
# }

## The copied method acts on the source when called by the target.
## This is useful when the target has the source as a composed object.
# copy_method = function(
#       method ## string giving the name of the method to transfer
#     , source ## source object to donate the method
#     , target ## target object to receive the method
#     , return = TRUE ## should the return value in the source be returned in the target?
#   ) {
#   force(method); force(source); force(target)
#   target[[method]] = function() {
#     named_args = as.list(environment())
#     dot_args = try(list(...), silent = TRUE)
#     if (inherits(dot_args, "try-error")) dot_args = list()
#     args = c(named_args, dot_args)
#     y = do.call(source[[method]], args)
#     if (return) return(y)
#   }
#   formals(target[[method]]) = formals(source[[method]])
# }
# copy_methods = function(methods, source, target, return = TRUE) {
#   for (method in methods) copy_method(method, source, target, return)
# }

TMBSimulationUtils = function() {
  self = Base()
  self$.simulation_formatter = function(r, .phases) {
    r = setNames(
      as.data.frame(r$values),
      c("matrix", "time", "row", "col", "value")
    )  ## get raw simulation output from TMB and supply column names (which don't exist on the TMB side)
    r$matrix = self$matrix_names()[r$matrix + 1L]  ## replace matrix indices with matrix names
    dn = self$tmb_model$init_mats$.dimnames ## get the row and column names of matrices with such names
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
      r = r[(r$time < 1L) | (r$time > num_t),,drop = FALSE]
    }
    if (!"after" %in% .phases) {
      r = r[r$time < num_t + 1,,drop = FALSE]
    }
    r
  }
  self$.find_problematic_expression = function(row) {
    expr_num_p_table_rows = self$tmb_model$data_arg()$expr_num_p_table_rows
    expr_num = min(which(row < cumsum(expr_num_p_table_rows)))
    deparse1(self$tmb_model$expr_list$expr_list()[[expr_num]])
  }
  self$.runner = function(...
      , .phases = c("before", "during", "after")
      , .method = c("report", "simulate")
  ) {
    .method = match.arg(.method)
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
    self$.simulation_formatter(r, .phases)
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
#' @export
TMBSimulator = function(tmb_model, tmb_cpp = "macpan2", initialize_ad_fun = TRUE) {
  self = TMBSimulationUtils()

  ## Args
  self$tmb_model = tmb_model
  self$tmb_cpp = tmb_cpp

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
  self$report = function(..., .phases = c("before", "during", "after")) {
    self$.runner(..., .phases = .phases, .method = "report")
  }
  self$report_values = function(..., .phases = c("before", "during", "after")) {
    self$report(..., .phases = .phases)$value
  }
  self$report_ensemble = function(...
      , .phases = c("before", "during", "after")
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
  self$simulate = function(..., .phases = c("before", "during", "after")) {
    self$.runner(..., .phases = .phases, .method = "simulate")
  }
  self$matrix = function(..., matrix_name, time_step) {
    r = self$report(...)
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


