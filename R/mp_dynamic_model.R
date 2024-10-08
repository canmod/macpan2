VariablesScripts = function(model) {
  self = Base()
  self$model = model
  self$state = function() self$model$index_data_type("state")
  self$flow_rates = function() self$model$index_data_type("flow_rates")
  self$influence_rates = function() self$model$index_data_type("influence_rates")
  self$aggregated_states = function() self$model$index_data_type("aggregated_states")
  self$normalized_state = function() self$model$index_data_type("normalized_state")
  return_object(self, "VariablesScripts")
}

LabelsScripts = function(model) {
  self = Base()
  self$model = model
  self$variables = model$variables

  self$dynamic_model = model$dynamic_model
  vs = self$dynamic_model$init_vecs
  for (nm in names(vs)) self[[nm]] = LabelsGetter(self$dynamic_model, nm)
  self$component_list = function() {
    l = list()
    vs = self$dynamic_model$init_vecs
    for (nm in names(vs)) l[[nm]] = self[[nm]]()
    l
  }

  # self$state = function() self$variables$state()$labels()
  # self$flow_rates = function() self$variables$flow_rates()$labels()
  # self$influence_rates = function() self$variables$influence_rates()$labels()
  # self$aggregated_states = function() self$variables$aggregated_states()$labels()
  # self$normalized_state = function() self$variables$normalized_state()$labels()
  # self$component_list = function() {
  #
  # }
  return_object(self, "LabelsScripts")
}

LabelsGetter = function(dynamic_model, vector_name) {
  self = Base()
  self$dynamic_model = dynamic_model
  self$vector_name = vector_name
  self$get = function() self$dynamic_model$init_vecs[[self$vector_name]] |> names()
  self$get
}

LabelsDynamic = function(dynamic_model) {
  self = Base()
  self$dynamic_model = dynamic_model
  vs = self$dynamic_model$init_vecs
  for (nm in names(vs)) self[[nm]] = LabelsGetter(self$dynamic_model, nm)
  self$component_list = function() {
    l = list()
    vs = self$dynamic_model$init_vecs
    for (nm in names(vs)) l[[nm]] = self[[nm]]()
    l
  }
  return_object(self, "LabelsDynamic")
}


DynamicModel = function(
      expr_list = ExprList()
    , ledgers = list()
    , init_vecs = list()
    , unstruc_mats = list()
  ) {
  self = Base()
  self$expr_list = expr_list
  self$ledgers = ledgers
  self$init_vecs = lapply(init_vecs, mp_structured_vector)
  self$unstruc_mats = unstruc_mats
  self$int_vec_names = function() {
    lapply(self$ledgers, getElement, "table_names") |> unlist(use.names = TRUE) |> unique()
  }
  self$derived_matrix_names = function() {
    setdiff(self$expr_list$all_formula_vars()
      , c(
            names(self$init_vecs)
          , self$int_vec_names()
          , names(self$unstruc_mats)
      )
    )
  }
  self$labels = LabelsDynamic(self)
  return_object(self, "DynamicModel")
}


#' Dynamic Model
#' 
#' This is an 'old' model specification function that was tested out 
#' at a workshop. Currently it still drives the engine-agnostic-grammar 
#' vignette, but we plan to replace this function with 
#' \code{\link{mp_tmb_model_spec}} and other model specification
#' functions.
#' 
#' @param expr_list Expression list.
#' @param ledgers Ledgers.
#' @param init_vecs Initial structured vectors.
#' @param unstruc_mats Initial unstructured matrices.
#' 
#' @export
mp_dynamic_model = DynamicModel


#' @export
print.DynamicModel = function(x, ...) {
  print(x$expr_list)
}
