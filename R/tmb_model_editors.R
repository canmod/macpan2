#' Transform a TMB Model Specification
#' 
#' Insert, update, or delete elements of a TMB model spec, produced using
#' \code{\link{mp_tmb_library}} or \code{\link{mp_tmb_model_spec}}. The
#' only difference between `mp_tmb_insert` and `mp_tmb_update` is that
#' the former shifts the positions of existing expressions to make room
#' for the new expressions, whereas the latter overwrites existing expressions
#' using the new expressions. The treatment of new `default` values and 
#' `integers` is the same. The examples below clarify this difference.
#' Note that `mp_tmb_delete` does not contain an `expressions` argument,
#' because it is not necessary to specify new expressions in the case
#' of deletion.
#' 
#' These modifications do not update the model specification in-place. Instead
#' the output of `mp_tmb_insert`, `mp_tmb_update`, and `mp_tmb_delete` define 
#' a new model specification and should be saved if you want to use the new 
#' model (e.g., \code{new_model = mp_tmb_insert(model, ...)}).
#' 
#' @param model TMB model spec object produced using
#' \code{\link{mp_tmb_library}} or \code{\link{mp_tmb_model_spec}}.
#' @param phase At what phase should `expressions` be inserted, updated, 
#' or deleted.
#' @param at Expression number, which can be identified by printing out
#' `model`, at which the `expressions` should be inserted or updated. If
#' inserted then the existing expressions with number `at` and higher are
#' shifted after the new `expressions` are added. If updated, then the
#' existing expressions with number from `at` to `at + length(expressions) - 1`
#' are replaced with the new `expressions`.
#' For `mp_tmb_delete`, a numeric vector of integers identifying expressions
#' to delete from the model.
#' @param expressions Expressions to insert into the model spec or to
#' replace existing expressions.
#' @param default Named list of objects, each of which can be coerced into 
#' a \code{\link{numeric}} \code{\link{matrix}}. The names refer to 
#' variables that appear in \code{before}, \code{during}, and \code{after}.
#' For `mp_tmb_delete`, a character vector of such objects to delete from
#' the model.
#' @param inits An optional list of initial values for the state variables.
#' These initial values can be added to the `default` list with identical 
#' results, but adding them to `inits` is better practice because it makes it 
#' clear that they are initial values that will change as the state updates.
#' @param integers Named list of vectors that can be coerced to integer
#' vectors. These integer vectors can be used by name in model formulas to
#' provide indexing of matrices and as grouping factors in 
#' \code{\link{group_sums}}.
#' For `mp_tmb_delete`, a character vector of such objects to delete from
#' the model.
#' @inheritParams mp_tmb_model_spec
#' 
#' @returns A new model spec object with updated and/or inserted information.
#' 
#' @examples
#' si = mp_tmb_library("starter_models", "si", package = "macpan2")
#' print(si)
#' 
#' ## Update the mixing process to include 
#' ## optional phenomenological heterogeneity.
#' ## We need mp_tmb_update here so that 
#' ## the previous infection expression is
#' ## overwritten.
#' mp_tmb_update(si, phase = "during"
#'   , at = 1
#'   , expressions = list(infection ~ beta * I * (S/N)^zeta)
#'   , default = list(zeta = 1)
#' )
#' 
#' ## Parameterize with log_beta in place of beta.
#' ## We need mp_tmb_insert here so that the
#' ## existing expression for computing the initial
#' ## number of susceptible indiviudals is not
#' ## overwritten.
#' mp_tmb_insert(si, phase = "before"
#'   , at = 1
#'   , expressions = list(beta ~ exp(log_beta))
#'   , default = list(log_beta = log(0.5))
#' )
#' 
#' @concept transform-model-spec
#' @export
mp_tmb_insert = function(model
    , phase = "during"
    , at = 1L
    , expressions = list()
    , default = list()
    , inits = list()
    , integers = list()
    , must_save = character()
    , must_not_save = character()
    , sim_exprs = character()
  ) {
  default = c(default, inits)
  model = assert_cls(model, "TMBModelSpec", match.call(), "?mp_tmb_model_spec")
  valid$char1$check(phase)
  at = valid$num1$assert(at)
  expressions = force_expr_list(expressions)
  
  model = model$copy()
  model[[phase]] = append(model[[phase]], expressions, after = at - 1L)
  model[["default"]][names(default)] = default
  model[["integers"]][names(integers)] = integers
  model$must_save  = unique(c(model$must_save, must_save))
  model$must_not_save  = unique(c(model$must_not_save, must_not_save))
  model$sim_exprs  = unique(c(model$sim_exprs, sim_exprs))
  
  mp_tmb_model_spec(
      before = model$before
    , during = model$during
    , after = model$after
    , default = model$default
    , integers = model$integers
    , must_save = model$must_save
    , must_not_save = model$must_not_save
    , sim_exprs = model$sim_exprs
    , state_update = model$state_update
  )
}


#' @concept transform-model-spec
#' @rdname mp_tmb_insert
#' @export
mp_tmb_update = function(model
    , phase = "during"
    , at = 1L
    , expressions = list()
    , default = list()
    , inits = list()
    , integers = list()
    , must_save = character()
    , must_not_save = character()
    , sim_exprs = character()
  ) {
  default = c(default, inits)
  model = assert_cls(model, "TMBModelSpec", match.call(), "?mp_tmb_model_spec")
  valid$char1$check(phase)
  at = valid$num1$assert(at)
  expressions = force_expr_list(expressions)
  
  model = model$copy()
  where = at - 1L + seq_along(expressions)
  model[[phase]][where] = expressions
  model[["default"]][names(default)] = default
  model[["integers"]][names(integers)] = integers
  model$must_save  = unique(c(model$must_save, must_save))
  model$must_not_save = unique(c(model$must_not_save, must_not_save))
  model$sim_exprs  = unique(c(model$sim_exprs, sim_exprs))
  
  mp_tmb_model_spec(
      before = model$before
    , during = model$during
    , after = model$after
    , default = model$default
    , integers = model$integers
    , must_save = model$must_save
    , must_not_save = model$must_not_save
    , sim_exprs = model$sim_exprs
    , state_update = model$state_update
  )
}

#' @concept transform-model-spec
#' @rdname mp_tmb_insert
#' @export
mp_tmb_delete = function(model
    , phase
    , at
    , default = character()
    , integers = character()
    , must_save = character()
    , must_not_save = character()
    , sim_exprs = character()
  ) {
  model = assert_cls(model, "TMBModelSpec", match.call(), "?mp_tmb_model_spec")
  if (!phase %in% c("before", "during", "after")) {
    stop("The simulation phase must be one of before, during, or after.")
  }
  at = valid$num1$assert(at)
  model = model$copy()
  model[[phase]][at] = NULL
  model[["default"]][default] = NULL
  model[["integers"]][integers] = NULL
  model$must_save  = setdiff(model$must_save, must_save)
  model$must_not_save  = setdiff(model$must_not_save, must_not_save)
  model$sim_exprs  = setdiff(model$sim_exprs, sim_exprs)
  
  mp_tmb_model_spec(
      before = model$before
    , during = model$during
    , after = model$after
    , default = model$default
    , integers = model$integers
    , must_save = model$must_save
    , must_not_save = model$must_not_save
    , sim_exprs = model$sim_exprs
    , state_update = model$state_update
  )
}

#' Transform a TMB Model Specification to Account for Reporting Bias
#' 
#' A version of \code{\link{mp_tmb_insert}} making it more convenient to
#' transform an incidence variable into a reports variable, which accounts 
#' for reporting delays and under-reporting. This new reports variable is
#' a convolution of the simulation history of an incidence variable with 
#' a kernel that is proportional to a Gamma distribution of reporting
#' delay times.
#' 
#' @param model A model produced by \code{\link{mp_tmb_model_spec}}.
#' @param incidence_name Name of the incidence variable to be transformed.
#' @param report_prob Value to use for the reporting probability; the
#' proportion of cases that get reported.
#' @param mean_delay Mean of the Gamma distribution of reporting delay times.
#' @param cv_delay Coefficient of variation of the Gamma distribution of
#' reporting delay times.
#' @param reports_name Name of the new reports variable.
#' @param report_prob_name Name of the variable containing `report_prob`.
#' @param mean_delay_name Name of the variable containing `mean_delay`.
#' @param cv_delay_name Name of the variable containing `cv_delay`.
#' 
#' @concept transform-model-spec
#' @export
mp_tmb_insert_reports = function(model
  , incidence_name
  , report_prob
  , mean_delay
  , cv_delay
  , reports_name = sprintf("reported_%s", incidence_name)
  , report_prob_name = sprintf("%s_report_prob", incidence_name)
  , mean_delay_name = sprintf("%s_mean_delay", incidence_name)
  , cv_delay_name = sprintf("%s_cv_delay", incidence_name)
) {
  all_names = named_vec(
      incidence_name
    , reports_name
    , report_prob_name
    , mean_delay_name
    , cv_delay_name
  )
  dup_names = duplicated(all_names)
  if (any(dup_names)) {
    all_dups = all_names[all_names %in% all_names[dup_names]]
    dup_list = tapply(names(all_dups), unname(all_dups), c, simplify = FALSE)
    usage = vapply(dup_list
      , paste, character(1L)
      , collapse = ", "
      , USE.NAMES = FALSE
    )
    frame = data.frame(variable = names(dup_list), usage = usage)
    mp_wrap(
        "The following names were assigned to variables that account for"
      , "reporting bias via `mp_tmb_insert_reports()`, but they were"
      , "inconsistently used for multiple purposes."
    ) |> c("\n\n", frame_formatter(frame)) |> stop()
  }
  model = assert_cls(model, "TMBModelSpec", match.call(), "?mp_tmb_model_spec")
  local_names = c(dist = "dist", delta = "delta", kernel = "kernel")
  map = model$name_map(local_names)
  default = setNames(
      list(report_prob, mean_delay, cv_delay)
    , c(report_prob_name, mean_delay_name, cv_delay_name)
  )
  
  cv2 = cv_delay^2
  shape = 1 / cv2
  scale = mean_delay * cv2
  kernel_length = qgamma(0.95, shape, scale = scale) |> ceiling() |> as.integer()

  expressions = c(
      sprintf("%s ~ pgamma(1:%s, 1/(%s), %s * (%s^2))", map$dist, kernel_length + 1L, cv_delay_name, mean_delay_name, cv_delay_name)
    , sprintf("%s ~ %s[1:%s] - %s[0:%s]", map$delta, map$dist, kernel_length, map$dist, kernel_length - 1L)
    , sprintf("%s ~ %s * %s / sum(%s)", map$kernel, report_prob_name, map$delta, map$delta)
    , sprintf("%s ~ convolution(%s, %s)", reports_name, incidence_name, map$kernel)
  ) |> lapply(as.formula)
  
  mp_tmb_insert(model
    , phase = "during"
    , at = Inf
    , expressions = expressions
    , default = default
    , must_save = reports_name
    , must_not_save = unlist(map, use.names = FALSE)
  )
}

#' Insert GLM Time Variation
#' 
#' @param model A model specification (see \code{\link{mp_tmb_model_spec}}).
#' @param parameter_name Character string giving the name of the parameter
#' to make time-varying.
#' @param design_matrix Matrix of time variation.
#' @param timevar_coef Initial coefficient matrix with the same number of rows
#' as there are columns of the `design_matrix`.
#' @param link_function Link function given by functions like
#' \code{\link{mp_log}}.
#' @param matrix_coef_name Name of the vector containing values of the non-zero 
#' elements of the design matrix.
#' @param matrix_row_name Name of the vector containing row indices of
#' the non-zero elements of the design matrix.
#' @param matrix_col_name Name of the vector containing column indices of
#' the non-zero elements of the design matrix.
#' @param linear_pred_name Name of the vector containing the linear 
#' predictor.
#' @param timeseries_name Name of the vector containing the time-series
#' of the varying parameter.
#' @param timevar_coef_name Name of the vector containing the time-varying
#' parameter coefficients.
#' @param time_index_name Name of the index at which the time varying
#' parameter changes.
#' @param sparsity_tolerance Make design matrix coefficients exactly zero
#' when they are below this tolerance.
#' 
#' @export
mp_tmb_insert_glm_timevar = function(model
    , parameter_name
    , design_matrix
    , timevar_coef
    , link_function = mp_log
    , matrix_coef_name = sprintf("matrix_coef_%s", parameter_name)
    , matrix_row_name = sprintf("matrix_row_%s", parameter_name)
    , matrix_col_name = sprintf("matrix_col_%s", parameter_name)
    , linear_pred_name = sprintf("linear_pred_%s", parameter_name)
    , timeseries_name = sprintf("timeseries_%s", parameter_name)
    , timevar_coef_name = sprintf("time_var_%s", parameter_name)
    , time_index_name = sprintf("time_index_%s", parameter_name)
    , sparsity_tolerance = 0
    , engine_function = c("sparse_mat_mult", "group_sums")
) {
  if (!parameter_name %in% names(model$default)) {
    sprintf(
        "Parameter '%s' not found in model$default. You must define it before adding time variation."
      , parameter_name
    ) |> stop()
  }

  design_matrix = as.matrix(design_matrix)
  if (nrow(design_matrix) < 1L) stop("The design matrix must have at least one row.")
  if (ncol(design_matrix) != nrow(timevar_coef)) {
    stop("The design_matrix and timevar_coef matrix are not compatible. The number of columns in the former must equal the number of rows in the latter.")
  }
    
  
  sparse_matrix = sparse_matrix_notation(design_matrix, tol = sparsity_tolerance)
  
  engine_function = match.arg(engine_function)
  
  matrix_coefs = sparse_matrix$values
  matrix_row = sparse_matrix$row_index
  matrix_col = sparse_matrix$col_index
  linear_pred = timeseries = matrix(0, nrow(design_matrix), ncol(timevar_coef))
  
  time_var = list(timevar_coef) |> setNames(timevar_coef_name)
  time_index = seq_along(linear_pred)
  
  default = setNames(
      list(matrix_coefs, linear_pred, timevar_coef, timeseries)
    , c(matrix_coef_name, linear_pred_name, timevar_coef_name, timeseries_name)
  )
  integers = setNames(
      list(matrix_row, matrix_col, time_index)
    , c(matrix_row_name, matrix_col_name, time_index_name)
  )
  
  if (engine_function == "group_sums") {
    matmult = sprintf("%s ~ %s + group_sums(%s * %s[%s], %s, %s)"
      , linear_pred_name, link_function$ref(parameter_name)
      , matrix_coef_name, timevar_coef_name
      , matrix_col_name, matrix_row_name, linear_pred_name
    ) |> as.formula()
  } else if(engine_function == "sparse_mat_mult") {
    matmult = sprintf("%s ~ %s + sparse_mat_mult(%s, %s, %s, %s, %s)"
      , linear_pred_name
      , link_function$ref(parameter_name)
      , matrix_coef_name, matrix_row_name, matrix_col_name
      , timevar_coef_name
      , linear_pred_name
    ) |> as.formula()
  }
  
  linkfn = sprintf("%s ~ %s"
    , timeseries_name
    , link_function$ref_inv(linear_pred_name)
  ) |> as.formula()
  getpar = sprintf("%s ~ time_var(%s, %s)"
    , parameter_name
    , timeseries_name
    , time_index_name
  ) |> as.formula()
  
  
  model = mp_tmb_insert(model
    , phase = "before", at = Inf
    , expressions = list(matmult, linkfn)
    , default = default
    , integers = integers
  )
  model = mp_tmb_insert(model
    , phase = "during", at = 1L
    , expressions = list(getpar)
  )
  return(model)
}

#' Insert Log Linear Model of Time Variation (Experimental)
#' 
#' @param model A model specification (see \code{\link{mp_tmb_model_spec}}).
#' @param parameter_name Character string giving the name of the parameter
#' to make time-varying.
#' @param design_matrices List of matrices, one for each time window, describing
#' the model of time variation.
#' @param time_var_parameters Named list of parameter vectors for each window,
#' with names giving the window names.
#' @param window_names Names for each window.
#' @param baseline_functions It is complicated -- this is a joke -- I'm tired.
#' @param link_functions List of objects representing link functions.
#' @param full_series_name Name of variable storing the full time series.
#' @param baseline_names Names of variables containing the baseline in 
#' each window.
#' @param matrix_coef_names Names of vectors containing values of the non-zero 
#' elements of the design matrices.
#' @param matrix_row_names Names of the vectors containing row indices of
#' the non-zero elements of the design matrices.
#' @param matrix_col_names Names of the vectors containing column indices of
#' the non-zero elements of the design matrices.
#' @param linear_pred_names Names of the vectors containing the linear 
#' predictors in each window.
#' @param time_var_names Names of the time-varying parameter in each window.
#' @param time_index_name Name of the index at which the time varying
#' parameter changes.
#' @param sparsity_tolerance Make design matrix coefficients exactly zero
#' when they are below this tolerance.
#' 
#' @export
mp_tmb_insert_log_linear = function(model
    , parameter_name
    , design_matrices ## list of matrices -- one for each window
    , time_var_parameters ## named list of parameter vectors for each window (names give window names)
    , window_names = names(time_var_parameters)
    #, change_points ## list of change-point integer vectors
    #, offset_references ## list of character vectors
    , baseline_functions = c(
          list(TimeVarBaselineParameter())
        , rep(list(TimeVarBaselineNumeric(0)), length(design_matrices) - 1)
      )
    , link_functions = rep(list(mp_identity), length(design_matrices)) ## list of DistrParamTrans objects
    , full_series_name = sprintf("time_var_output_%s", parameter_name)
    , baseline_names = sprintf("baseline_%s", window_names)
    , matrix_coef_names = sprintf("matrix_coef_%s", window_names)
    , matrix_row_names = sprintf("matrix_row_%s", window_names)
    , matrix_col_names = sprintf("matrix_col_%s", window_names)
    , linear_pred_names = sprintf("linear_pred_%s", window_names)
    , time_var_names = sprintf("time_var_%s", window_names)
    , time_index_name = sprintf("time_index_%s", parameter_name)
    , sparsity_tolerance = 0
  ) {
  sparse_matrices = lapply(design_matrices, sparse_matrix_notation, tol = sparsity_tolerance)
  
  matrix_coefs = lapply(sparse_matrices, getElement, "values") |> setNames(matrix_coef_names)
  matrix_row = lapply(sparse_matrices, getElement, "row_index") |> setNames(matrix_row_names)
  matrix_col = lapply(sparse_matrices, getElement, "col_index") |> setNames(matrix_col_names)
  linear_pred = lapply(design_matrices, \(x) numeric(nrow(x))) |> setNames(linear_pred_names)
  
  inv_links = character()
  before = character()
  if (baseline_functions[[1]]$not_for_first_window()) stop("Invalid baseline specification.")
  eta = ""
  for (i in seq_along(design_matrices)) {
    if (i > 1L) eta = linear_pred_names[i - 1L]
    before = append(before, baseline_functions[[i]]$calc(
        baseline_names[i]
      , eta
      , link_functions[[i]]
      , parameter_name
    ))
    before = append(before, sprintf("%s ~ %s + group_sums(%s * %s[%s], %s, %s)"
      , linear_pred_names[i], baseline_names[i]
      , matrix_coef_names[i], time_var_names[i]
      , matrix_col_names[i], matrix_row_names[i], linear_pred_names[i]
    ))
    inv_links = append(inv_links, link_functions[[i]]$ref_inv(linear_pred_names[i]))
  }
  series = paste(inv_links, collapse = ", ")
  before = append(before
    , sprintf("%s ~ c(%s)", full_series_name, series)
  ) |> lapply(as.formula)
  
  during = sprintf("%s ~ time_var(%s, %s)"
    , parameter_name
    , full_series_name
    , time_index_name
  ) |> lapply(as.formula)
  
  
  time_index = list(seq_len(length(unlist(linear_pred)))) |> setNames(time_index_name)
  time_var = time_var_parameters |> setNames(time_var_names)
  
  
  model = mp_tmb_insert(model
    , phase = "before"
    , at = 1L
    , expressions = before
    , default = c(matrix_coefs, linear_pred, time_var)
    , integers = c(matrix_row, matrix_col, time_index)
  )
  model = mp_tmb_insert(model
    , phase = "during"
    , at = 1L
    , expressions = during
  )
  return(model)
}

#' Insert Basic Transformations of Model Variables
#' 
#' @inheritParams mp_tmb_insert
#' @param variables Character vector of variables to transform.
#' @param transformation A transformation object such as \code{\link{mp_log}},
#' which is the default. See the help page for \code{\link{mp_log}} for 
#' available options.
#' 
#' @return A new model specification object with expressions for the transformed 
#' variables at the end of the simulation loop. The transformed variables
#' are identified with a prefixed name (e.g., `log_incidence` if `incidence`
#' is log transformed).
#' 
#' @seealso [mp_tmb_insert_backtrans()]
#' 
#' @examples
#' ("starter_models"
#'   |> mp_tmb_library("si", package = "macpan2")
#'   |> mp_tmb_insert_trans("infection", mp_log)
#'   |> mp_simulator(time_steps = 5L, outputs = "log_infection")
#'   |> mp_trajectory()
#' )
#' 
#' @export
mp_tmb_insert_trans = function(model
    , variables = character()
    , transformation = mp_log
) {
  expr_list = sprintf("%s ~ %s"
    , transformation$nm(variables)
    , transformation$ref(variables)
  ) |> lapply(as.formula)
  mp_tmb_insert(model, "during", Inf, expr_list)
}

#' Insert Back Transformations of Model Parameters
#' 
#' @inheritParams mp_tmb_insert_trans
#' @param variables Character vector of parameters to back transform.
#' 
#' @return A new model specification object with expressions for the 
#' untransformed (or back transformed) parameters at the beginning of the
#' `before` phase. The transformed version of the parameter is also
#' added to the defaults and are identified with a prefixed name (e.g., 
#' `log_beta` if `beta` is log transformed).
#' 
#' @seealso [mp_tmb_insert_trans()]
#' 
#' @examples
#' init_si = ("starter_models"
#'   |> mp_tmb_library("si", package = "macpan2")
#'   |> mp_tmb_insert_backtrans("beta", mp_log)
#'   |> mp_initial_list()
#' )
#' print(init_si$log_beta)
#' print(log(init_si$beta))
#' 
#' @export
mp_tmb_insert_backtrans = function(model
    , variables = character()
    , transformation = mp_log
) {
  
  trans_variables = transformation$nm(variables)
  i = !trans_variables %in% names(model$default)
  default = (model$default[variables[i]] 
    |> lapply(transformation$val) 
    |> setNames(trans_variables[i])
  )
  expr_list = sprintf("%s ~ %s"
    , variables
    , transformation$ref_inv(transformation$nm(variables))
  ) |> lapply(as.formula)
  mp_tmb_insert(model, "before", 1L, expr_list, default)
}

#' @describeIn mp_tmb_insert_trans Insert variable transformations implicitly
#' by pre-pending the name of the transformations in front of the names of the
#' `variables` that you ask for (e.g., `"log_case_reports"`) even if this
#' variable is not in the model as long as the base name
#' (e.g., `"case_reports"`) is.
#' @export
mp_tmb_implicit_trans = function(model, variables = character()) {
  if (is.null(variables)) return(model)
  vars_to_trans = get_vars_to_trans(variables, model$all_formula_vars())
  for (trans_nm in names(vars_to_trans)) {
    vars = vars_to_trans[[trans_nm]]
    trans_obj = get(sprintf("mp_%s", trans_nm))
    if (length(vars) > 0L) {
      model = mp_tmb_insert_trans(model, vars, trans_obj)
    }
  }
  return(model)
}

#' @describeIn mp_tmb_insert_backtrans Insert parameter transformations 
#' implicitly by pre-pending the name of the transformations in front of the 
#' names of the `variables` that you ask for (e.g., `"log_case_reports"`) even 
#' if this variable is not in the model as long as the base name (e.g., 
#' `"case_reports"`) is.
#' @export
mp_tmb_implicit_backtrans = function(model, variables = character()) {
  
  ## list with names of variables to transform for
  ## each type of transformation
  vars_to_trans = get_vars_to_trans(variables, model$all_formula_vars())
  
  ## loop over types of transformations (log, logit, sqrt)
  for (trans_nm in names(vars_to_trans)) {
    vars = vars_to_trans[[trans_nm]]
    trans_obj = get(sprintf("mp_%s", trans_nm))
    if (length(vars) > 0L) {
      model = mp_tmb_insert_backtrans(model, vars, trans_obj)
    }
  }
  return(model)
}

get_parameter_names = function(obj) {
  ## take that S3 dispatch
  if (is.character(obj)) return(obj)
  if (inherits(obj, "ParArg")) return(c(names(obj$params), names(obj$random)))
  if (is.list(obj)) return(names(obj))
  stop("Not a recognized object for representing parameters")
}
## TODO: function not used currently -- if used we would also need to 
## transform the data, and that is for the future.
get_trajectory_names = function(obj) {
  if (is.character(obj)) return(obj)
  if (inherits(obj, "TrajArg")) return(c(names(obj$likelihood), names(obj$condensation)))
  if (is.list(obj)) return(names(obj))
  stop("Not a recognized object for representing parameters")
}
mp_cal_implicit_trans = function(spec, cal) {
  out_nms = cal$cal_args$outputs
  par_nms = get_parameter_names(cal$cal_args$par)
  (spec
    |> mp_tmb_update(default = cal$cal_args$default)
    |> mp_tmb_implicit_trans(out_nms)
    |> mp_tmb_implicit_backtrans(par_nms)
  )
}

get_vars_to_trans = function(variables, all_variables) {
  
  simple_variables = intersect(variables, all_variables)
  complex_variables = setdiff(variables, all_variables)
  trans_variables = grep("^(log|logit|sqrt|log1p)_", complex_variables, value = TRUE)
  
  good_variables = c(simple_variables, trans_variables)
  bad_variables = setdiff(variables, good_variables)
  if (length(bad_variables) > 0L) {
    mp_wrap(
      "The following variables were required but not available in the model"
    , bad_variables
    )
  }
  
  list(
      log = sub("^log_", "", complex_variables) |> intersect(all_variables)
    , logit = sub("^logit_", "", complex_variables) |> intersect(all_variables)
    , sqrt = sub("^sqrt_", "", complex_variables) |> intersect(all_variables)
    , log1p = sub("^log1p_", "", complex_variables) |> intersect(all_variables)
  )
}

## model is a spec
## new_defaults is a list of raw defaults, each type of which has a `names` S3 method
check_default_updates = function(model, new_defaults) {
  old_defaults = model$default
  common_nms = intersect(names(old_defaults), names(new_defaults))
  for (nm in common_nms) {
    old_dim = dim(as.matrix(old_defaults[[nm]]))
    new_dim = dim(as.matrix(new_defaults[[nm]]))
    if (!identical(old_dim, new_dim)) {
      warning("Updated default ", nm, " is not the same shape as the existing default.")
    }
  }
  NULL
}

## Internal classes that handle model editing.


TMBEditor = function(model) {
  self = Base()
  self$model = model
  return_object(self, "TMBEditor")
}

## Printers ------------

TMBPrinter = function(model) {
  self = TMBEditor(model)
  self$expressions = function() self$model$expr_list$print_exprs()
  self$matrix_dims = function() self$model$init_mats$mat_dims()
  return_object(self, "TMBPrinter")
}

TMBSimulatorPrinter = function(simulator) {
  self = TMBPrinter(simulator$tmb_model)
  self$simulator = simulator
  self$matrix_dims = function() {
    y = self$model$init_mats$mat_dims()
    return(y)
  }
  self$expressions = function() {
    y = self$model$expr_list$print_exprs()
  }
  return_object(self, "TMBSimulatorPrinter")
}

## Inserters ------------

TMBInserter = function(model) {
  self = TMBEditor(model)
  self$.at = function(at, phase = c("before", "during", "after")) {
    phase = match.arg(phase)
    match_if_appropriate(at, names(self$model$expr_list[[phase]]))
  }
  self$expressions = function(...
    , .at = 1L
    , .phase = c("before", "during", "after")
    , .simulate_exprs = character(0L)
  ) {
    self$model$expr_list = self$model$expr_list$insert(...
      , .at = .at
      , .phase = .phase
      , .simulate_exprs = .simulate_exprs
    )
    self$model$expr_list$init_mats = self$model$init_mats
    self$model$obj_fn$init_mats = self$model$init_mats
    invisible(self$model)
  }
  return_object(self, "TMBInserter")
}

TMBSimulatorInserter = function(simulator) {
  self = TMBInserter(simulator$tmb_model)
  self$simulator = simulator
  self$expressions = function(...
    , .at = 1L
    , .phase = c("before", "during", "after")
    , .simulate_exprs = character(0L)
    , .vec_by = getOption("macpan2_vec_by")
  ) {
    
    ## ----  #171
    dot_args = list(...)
    spec_args = c("at", "phase", "simulate_exprs", "vec_by")
    possible_arg_typos = spec_args %in% names(dot_args)
    if (any(possible_arg_typos)) {
      possible = spec_args[possible_arg_typos]
      if ("at" %in% possible) .at = dot_args$at
      if ("phase" %in% possible) .phase = dot_args$phase
      if ("simulate_exprs" %in% possible) .simulate_exprs = dot_args$simulate_exprs
      if ("vec_by" %in% possible) .vec_by = dot_args$vec_by
      not_exprs = which(possible == names(dot_args))
      dot_args = dot_args[-not_exprs]
    }
    
    .at = self$.at(.at, .phase)
    for (v in names(.vec_by)) {
      if (.vec_by[v] == "") .vec_by[v] = "...RAW...INDICES..."
    }
    if (inherits(self$model$init_mats$.structure_labels, "NullLabels")) {
      args = dot_args
    } else {
      mat_names = names(self$model$init_mats)
      component_list = self$model$init_mats$.structure_labels$component_list()
      args = (dot_args
        |> lapply(to_special_vecs, component_list, mat_names, .vec_by)
        |> lapply(to_assign)
      )
    }
    args$.at = .at
    args$.phase = .phase
    args$.simulate_exprs = .simulate_exprs
    (self$model$expr_list$insert
      |> do.call(args)
      |> self$model$refresh$expr_list()
    )
    self$simulator$cache$invalidate()
    invisible(self$simulator)
  }
  return_object(self, "TMBSimulatorInserter")
}


## Adders ------------

TMBAdder = function(model) {
  self = TMBEditor(model)
  self$matrices = function(...
    , .mats_to_save = character(0L)
    , .mats_to_return = character(0L)
    , .dimnames = list()
  ) {
    self$model$init_mats = self$model$init_mats$add_mats(...
      , .mats_to_save = .mats_to_save
      , .mats_to_return = .mats_to_return
      , .dimnames = .dimnames
    )
    invisible(self$model)
  }
  return_object(self, "TMBAdder")
}

TMBSimulatorAdder = function(simulator) {
  self = TMBAdder(simulator$tmb_model)
  self$simulator = simulator
  self$matrices = function(...
    , .mats_to_save = character(0L)
    , .mats_to_return = character(0L)
    , .dimnames = list()
  ) {
    self$model$init_mats$add_mats(...
      , .mats_to_save = .mats_to_save
      , .mats_to_return = .mats_to_return
      , .dimnames = .dimnames
    ) |> self$model$refresh$init_mats()
    self$simulator$outputs = union(self$simulator$outputs, .mats_to_return)
    self$simulator$cache$invalidate()
    invisible(self$simulator)
  }
  self$transformations = function(..., .at = 1L, .phase = "before") {
    l = list(...)
    l = setNames(l, vapply(l, getElement, character(1L), "variable"))
    for (v in names(l)) {
      check_auto_names(self$model$init_mats$.names(), l[[v]]$trans_variable)
      if (is.null(l[[v]]$default)) {
        value = self$model$init_mats$get(v)
      } else {
        value = l[[v]]$default
      }
      trans_value = l[[v]]$trans_engine_eval(value)
      do.call(
        self$simulator$add$matrices,
        setNames(list(trans_value), l[[v]]$trans_variable)
      )
      self$simulator$insert$expressions(
        l[[v]]$inverse_two_sided_formula(),
        .at = .at, .phase = .phase
      )
    }
    self$simulator$cache$invalidate()
    invisible(self$simulator)
  }
  self$observed_trajectory = function(
    observed_values,
    observed_times,
    simulated_trajectory
  ) {stop("under construction")}
  return_object(self, "TMBSimulatorAdder")
}


## Replacers ------------

TMBReplacer = function(model) {
  self = TMBEditor(model)
  self$obj_fn = function(obj_fn_expr) {
    (obj_fn_expr
      |> ObjectiveFunction()
      |> self$model$refresh$obj_fn()
    )
    self$model
  }
  return_object(self, "TMBReplacer")
}

TMBSimulatorReplacer = function(simulator) {
  self = TMBReplacer(simulator$tmb_model)
  self$simulator = simulator
  self$obj_fn = function(obj_fn_expr) {
    (obj_fn_expr
      |> ObjectiveFunction()
      |> self$model$refresh$obj_fn()
    )
    self$simulator$cache$invalidate()
    invisible(self$simulator)
  }
  self$params_frame = function(frame) {
    new_params = OptParamsFrame(frame, self$model$init_mats$dimnames())
    self$model$refresh$params(new_params)
    self$simulator$cache$invalidate()
    valid$consistency_params_mats$check(self$model)
    invisible(self$simulator)
  }
  self$params = function(default, mat, row = 0L, col = 0L) {
    self$params_frame(data.frame(default, mat, row, col))
  }
  self$params_struc = function(param, frame) {
    new_params = OptParamsFrameStruc(param, frame = frame, .dimnames = self$model$init_mats$dimnames())
    self$model$refresh$params(new_params)
    self$simulator$cache$invalidate()
    valid$consistency_params_mats$check(self$model)
    invisible(self$simulator)
  }
  self$random_frame = function(frame) {
    new_random = OptParamsFrame(frame, self$model$init_mats$dimnames())
    self$model$refresh$random(new_random)
    self$simulator$cache$invalidate()
    valid$consistency_random_mats$check(self$model)
    invisible(self$simulator)
  }
  self$random = function(default, mat, row = 0L, col = 0L) {
    self$random_frame(data.frame(default, mat, row, col))
  }
  self$random_struc = function(random, frame) {
    new_random = OptParamsFrameStruc(random, frame = frame, .dimnames = self$model$init_mats$dimnames())
    self$model$refresh$random(new_random)
    self$simulator$cache$invalidate()
    valid$consistency_random_mats$check(self$model)
    invisible(self$simulator)
  }
  self$time_steps = function(time_steps) {
    self$model$time_steps = Time(time_steps)
    self$simulator$cache$invalidate()
    invisible(self$simulator)
  }
  self$do_prep_sdreport = function(do_pred_sdreport) {
    self$model$do_pred_sdreport = do_pred_sdreport
    self$simulator$cache$invalidate()
    invisible(self$simulator)
  }
  return_object(self, "TMBReplacer")
}


## Deleters ------------

TMBDeleter = function(model) {
  self = TMBEditor(model)
  self$.at = function(at, phase = c("before", "during", "after")) {
    phase = match.arg(phase)
    match_if_appropriate(at, names(self$model$expr_list[[phase]]))
  }
  return_object(self, "TMBUpdater")
}

TMBSimulatorDeleter = function(simulator) {
  self = TMBDeleter(simulator$tmb_model)
  self$simulator = simulator
  self$expressions = function(
        .at
      , .phase = c("before", "during", "after")
      , .simulate_exprs = character(0L)
    ) {
      args = list()
      args$.at = self$.at(.at, .phase)
      args$.phase = .phase
      args$.simulate_exprs = .simulate_exprs
      (self$model$expr_list$delete
        |> do.call(args)
        |> self$model$refresh$expr_list()
      )
      self$simulator$cache$invalidate()
      invisible(self$simulator)
  }
  return_object(self, "TMBSimulatorDeleter")
}

## Updaters ------------

TMBUpdater = function(model) {
  self = TMBEditor(model)
  self$.at = function(at, phase = c("before", "during", "after")) {
    phase = match.arg(phase)
    match_if_appropriate(at, names(self$model$expr_list[[phase]]))
  }
  return_object(self, "TMBUpdater")
}

TMBSimulatorUpdater = function(simulator) {
  self = TMBUpdater(simulator$tmb_model)
  self$simulator = simulator
  self$matrices = function(...
    , .mats_to_save = character(0L)
    , .mats_to_return = character(0L)
    , .dimnames = list()
  ) {
    self$model$init_mats = self$model$init_mats$update_mats(...
      , .mats_to_save = .mats_to_save
      , .mats_to_return = .mats_to_return
      , .dimnames = .dimnames
    )
    self$simulator$outputs = union(self$simulator$outputs, .mats_to_return)
    self$simulator$cache$invalidate()
    invisible(self$simulator)
  }
  self$transformations = function(..., .at = 1L, .phase = "before") {
    l = list(...)
    l = setNames(l, vapply(l, getElement, character(1L), "variable"))
    for (v in names(l)) {
      value = self$model$init_mats$get(v)
      trans_value = l[[v]]$trans_engine_eval(value)
      do.call(
        self$simulator$update$matrices,
        setNames(list(value), l[[v]]$trans_variable)
      )
      self$simulator$insert$expressions(
        l[[v]]$inverse_two_sided_formula(),
        .at = .at, .phase = .phase
      )
    }
    self$simulator$cache$invalidate()
    invisible(self$simulator)
  }
  self$expressions = function(...
    , .at = 1L
    , .phase = c("before", "during", "after")
    , .simulate_exprs = character(0L)
    , .vec_by = getOption("macpan2_vec_by")
  ) {
    .at = self$.at(.at, .phase)
    for (v in names(.vec_by)) {
      if (.vec_by[v] == "") .vec_by[v] = "...RAW...INDICES..."
    }
    if (inherits(self$model$init_mats$.structure_labels, "NullLabels")) {
      args = list(...)
    } else {
      mat_names = names(self$model$init_mats)
      component_list = self$model$init_mats$.structure_labels$component_list()
      args = (list(...)
        |> lapply(to_special_vecs, component_list, mat_names, .vec_by)
        |> lapply(to_assign)
      )
    }
    args$.at = .at
    args$.phase = .phase
    args$.simulate_exprs = .simulate_exprs
    (self$model$expr_list$update
      |> do.call(args)
      |> self$model$refresh$expr_list()
    )
    self$simulator$cache$invalidate()
    invisible(self$simulator)
  }
  return_object(self, "TMBSimulatorUpdater")
}


TMBSimulatorResetter = function(simulator) {
  self = TMBUpdater(simulator$tmb_model)
  self$simulator = simulator
  self$params = function() {
    self$model$params = OptParamsList(0)
    self$simulator$cache$invalidate()
  }
  self$random = function() {
    self$model$params = OptParamsList()
    self$simulator$cache$invalidate()
  }
  return_object(self, "TMBSimulatorResetter")
}
