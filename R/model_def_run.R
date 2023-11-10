#' @export
Compartmental2 = function(model_directory) {
  self = Base()
  self$model_directory = model_directory
  self$files = Files(model_directory
    , reader_spec("settings.json", JSONReader)
  )
  self$settings = function() self$files$get("settings")
  self$def_env = new.env(parent = parent.frame())
  sys.source(file.path(model_directory, self$settings()[["structure_file"]])
    , envir = self$def_env
    , chdir = TRUE
  )
  self$dynamic_model = self$def_env[[self$settings()[["model_object"]]]]

  self$index_data_type = function(type) {
    x = self$def_env[[self$settings()[[type]]]]
    stopifnot(inherits(x, "Index"))
    x
  }

  self$flow_ledgers = function() self$dynamic_model$ledgers$flows
  self$influence_ledgers = function() self$dynamic_model$ledgers$influences
  #self$ledgers_type = function(type) {
  #  self$dynamic_model$ledgers[[self$settings()[[type]]]]
  #}
  #self$normalization_ledgers = function() self$ledgers_type("normalization")
  #self$aggregation_ledgers = function() self$ledgers_type("aggregation")

  self$flows = function() self$flow_ledgers()$labels_frame()
  self$influences = function() self$influence_ledgers()$labels_frame()
  #self$normalization = function() self$normalization_ledgers()$labels_frame()
  #self$aggregation = function() self$aggregation_ledgers()$labels_frame()
  self$expr_list = function() self$def_env[[self$settings()[["expr_list"]]]]

  ## back-compatibility
  self$variables = VariablesScripts(self)
  self$labels = LabelsScripts(self)

  return_object(self, "ModelDefRun")
}

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

#' @export
DynamicModel = function(expr_list = ExprList(), ledgers = list(), init_vecs = list(), unstruc_mats = list()) {
  self = Base()
  self$expr_list = expr_list
  self$ledgers = ledgers
  self$init_vecs = init_vecs
  self$unstruc_mats = unstruc_mats
  self$labels = LabelsDynamic(self)
  return_object(self, "DynamicModel")
}


#' @export
mp_dynamic_model = DynamicModel

#' @export
print.DynamicModel = function(x, ...) {
  print(x$expr_list)
}
