StandardExprAlt = function(model){
  self = Indices(model)
  self$.init_derivations_list = function(){
    # This expression is only required to ensure every model has at least
    # one expression. There is currently a bug where it is not possible to
    # create a simulator for a model with no expressions at all.
    hack_expression_list = list(
      output_names = "state",
      expression = "1*state",
      arguments = list("state"),
      simulation_phase = "before"
    )

    required_derivations = list(hack_expression_list)

    return(required_derivations)
  }
  self$.derivation_evaluator = function(derivation){
    formula = MathExpressionFromStrings(derivation$expression, derivation$arguments)
    derivation$expression = do.call(formula$symbolic$evaluate, derivation$arguments)
    return(derivation)
  }
  self$.derivations_evaluator = function(){
    return(lapply(self$.init_derivations_list(), self$.derivation_evaluator))
  }
  self$standard_expressions = self$.derivations_evaluator
  self$as_derivations = function() lapply(self$standard_expressions(), lapply, as.character)
  return_object(self, "StandardExpr")
}

Derivations2ExprListAlt = function(user_expr, standard_expr) {
  self = Base()
  self$standard_expr = standard_expr
  self$user_expr = user_expr
  self$.standard_expr_list = standard_expr$standard_expressions()
  self$.user_expr_list = user_expr$expand_vector_expressions()

  self$.expression_formatter = function(expression_list_element){
    two_sided(
      expression_list_element$output_names,
      expression_list_element$expression
    )
  }

  self$.expression_phase_sorter = function(
    phase = c(
      "before",
      "during_pre_update", "during_update", "during_post_update",
      "after")
  ) {

    phase = match.arg(phase)
    user_phases = vapply(
      self$.user_expr_list,
      getElement,
      character(1L),
      "simulation_phase"
    )
    standard_phases = vapply(
      self$.standard_expr_list,
      getElement,
      character(1L),
      "simulation_phase"
    )
    c(
      self$.user_expr_list[user_phases == phase],
      self$.standard_expr_list[standard_phases == phase]
    )
  }
  self$expr_list_per_phase = function(
    phase = c("before", "during", "after", "during_pre_update", "during_update", "during_post_update")
  ) {
    #browser()
    phases = match.arg(phase)
    if (phases == "during") {
      phases = c("during_pre_update", "during_update", "during_post_update")
    }

    l = list()
    for (phase in phases) {
      l = append(l, lapply(
        self$.expression_phase_sorter(phase),
        self$.expression_formatter
      ))
    }
    return(l)
  }
  self$expr_list = function(.simulate_exprs = character(0L)) {
    ExprList(
      before = self$expr_list_per_phase("before"),
      during = self$expr_list_per_phase("during"),
      after = self$expr_list_per_phase("after"),
      .simulate_exprs = .simulate_exprs
    )
  }
  self$math_expr_list = function(
    phase = c("before", "during", "after", "during_pre_update", "during_update", "during_post_update")
  ) {

  }
  return_object(self, "Derivations2ExprList")
}

ModelAlt = function(definition) {
  # Inheritance
  self = Base()

  # Args / Composition
  self$def = definition ## ModelFiles object

  # Compositions
  self$settings = Settings(self)
  self$variables = Variables(self)
  self$labels = VariableLabels(self$variables)
  self$indices = VariableIndices(self$labels)

  # Standard Methods
  self$flows = function() self$def$flows()
  self$flows_expanded = function() {
    expander = FlowExpander(self$def)
    expander$expand_flows()
  }
  self$flows_explicit = function() {
    optional_fields = c("from_partition", "to_partition", "flow_partition",
                        "from_to_partition", "from_flow_partition", "to_flow_partition")

    required_partition = self$settings$name()
    null_partition = self$settings$null()

    default_entries = data.frame(required_partition, required_partition, required_partition, "", "", null_partition)
    names(default_entries) = optional_fields

    default_entries = do.call("rbind", replicate(nrow(self$flows()), default_entries, simplify = FALSE))

    is_missing = function(field_name){
      return(!any(names(self$flows()) == field_name))
    }
    missing_fields = which(lapply(optional_fields, is_missing) == TRUE)
    return(cbind(self$flows(), default_entries[,missing_fields]))
  }
  self$derivations = self$def$derivations  ## look like a field but actually method forwarding
  self$expr_list = function() {
    Derivations2ExprListAlt(UserExpr(self), StandardExprAlt(self))$expr_list()
  }

  # Composition
  self$simulators = Simulators(self)

  # Set the cache in the underlying ModelFiles object
  # so that when the model definition files change
  # on disk, the caches that depend on these files
  # are invalidated.
  self$def$cache = CacheList(
    self$variables$cache,
    self$labels$cache,
    self$indices$flow ## invalidate method outside of the cache for convenience
  )

  # Validate and Return
  (self
    |> assert_variables()
    |> return_object("Model")
  )
}

CompartmentalAlt = function(model_directory){
  self = ModelAlt(ModelFiles(model_directory))
  return_object(self, "Compartmental")
}

#' Simulator Constructor
#'
#' @param model_directory A string giving a path to a directory containing
#' model definition files.
#' @param integration_method One of the functions described in
#' \link{integration_methods}, used to integrate the dynamical system.
#' @param ... Arguments to pass to \code{\link{TMBModel}}.
#' @export
SimulatorConstructor = function(model_directory, integration_method = RK4, ...){
  model = CompartmentalAlt(model_directory)

  model_simulator = model$simulators$tmb(..., .bundle_compartmental_model = TRUE)


  expanded_flows = model$flows_expanded()

  model_simulator = do.call(integration_method, list(model_simulator))

  return(model_simulator)
}
