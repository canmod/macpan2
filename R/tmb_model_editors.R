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
    , .vec_by_states = "state"
    , .vec_by_flows = "flow"
  ) {
    if (.vec_by_states == "") .vec_by_states = "...RAW...INDICES..."
    if (.vec_by_flows == "") .vec_by_flows = "...RAW...INDICES..."
    if (inherits(self$model$init_mats$.structure_labels, "NullLabels")) {
      args = list(...)
    } else {
      mat_names = self$model$init_mats$.names()
      component_list = list(
        self$model$init_mats$.structure_labels$state(),
        self$model$init_mats$.structure_labels$flow()
      ) |> setNames(c(.vec_by_states, .vec_by_flows))
      args = (list(...)
        |> lapply(to_special_vecs, component_list, mat_names)
        |> lapply(to_assign)
      )
    }
    args$.at = .at
    args$.phase = .phase
    args$.simulate_exprs = .simulate_exprs
    self$model$expr_list = do.call(
      self$model$expr_list$insert,
      args
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
    self$model$init_mats = self$model$init_mats$add_mats(...
      , .mats_to_save = .mats_to_save
      , .mats_to_return = .mats_to_return
      , .dimnames = .dimnames
    )
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
    self$model$obj_fn = ObjectiveFunction(obj_fn_expr)
    self$model
  }
  return_object(self, "TMBReplacer")
}

TMBSimulatorReplacer = function(simulator) {
  self = TMBReplacer(simulator$tmb_model)
  self$simulator = simulator
  self$obj_fn = function(obj_fn_expr) {
    self$model$obj_fn = ObjectiveFunction(obj_fn_expr)
    self$simulator$cache$invalidate()
    invisible(self$simulator)
  }
  self$params_frame = function(frame) {
    self$model$params = OptParamsFrame(frame, self$model$init_mats$.dimnames)
    self$simulator$cache$invalidate()
    valid$consistency_params_mats$check(self$model)
    invisible(self$simulator)
  }
  self$params = function(default, mat, row = 0L, col = 0L) {
    self$params_frame(data.frame(default, mat, row, col))
  }
  self$random_frame = function(frame) {
    self$model$random = OptParamsFrame(frame, self$model$init_mats$.dimnames)
    self$simulator$cache$invalidate()
    valid$consistency_random_mats$check(self$model)
    invisible(self$simulator)
  }
  self$random = function(default, mat, row = 0L, col = 0L) {
    self$random_frame(data.frame(default, mat, row, col))
  }
  self$time_steps = function(time_steps) {
    self$model$time_steps = Time(time_steps)
    self$simulator$cache$invalidate()
    invisible(self$simulator)
  }
  return_object(self, "TMBReplacer")
}


## Deleters ------------


## Updaters ------------

TMBUpdater = function(model) {
  self = TMBEditor(model)
  return_object(self, "TMBUpdater")
}

TMBSimulationUpdater = function(simulator) {
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
  return_object(self, "TMBSimulationUpdater")
}