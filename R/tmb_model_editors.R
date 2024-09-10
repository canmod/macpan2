#' Modify a TMB Model Spec
#' 
#' Insert, update, or delete elements of a TMB model spec, produced using
#' \code{\link{mp_tmb_library}} or \code{\link{mp_tmb_model_spec}}, or
#' \code{\link{mp_tmb_delete}}. The
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
#' @export
mp_tmb_insert = function(model
    , phase = "during"
    , at = 1L
    , expressions = list()
    , default = list()
    , integers = list()
    , must_save = character()
    , must_not_save = character()
    , sim_exprs = character()
  ) {
  model = assert_cls(model, "TMBModelSpec", match.call(), "?mp_tmb_model_spec")
  valid$char1$check(phase)
  at = valid$num1$assert(at)
  
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


#' @rdname mp_tmb_insert
#' @export
mp_tmb_update = function(model
    , phase = "during"
    , at = 1L
    , expressions = list()
    , default = list()
    , integers = list()
    , must_save = character()
    , must_not_save = character()
    , sim_exprs = character()
  ) {
  model = assert_cls(model, "TMBModelSpec", match.call(), "?mp_tmb_model_spec")
  valid$char1$check(phase)
  at = valid$num1$assert(at)
  
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

#' Insert Reports
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
    # if (.vec_by_states == "") .vec_by_states = "...RAW...INDICES..."
    # if (.vec_by_flows == "") .vec_by_flows = "...RAW...INDICES..."
    #component_vec_by = c(state = .vec_by_states, flow = .vec_by_flows)
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
