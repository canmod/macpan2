#' Model
#'
#' Construct an object for representing a model structure.
#'
#' @param definition Output of \code{\link{ModelFiles}}.
#'
#' @export
Model = function(definition) {
  self = Base()
  self$def = definition
  self$variables = Variables(self)
  self$labels = VariableLabels(self$variables)
  self$flows = function() self$def$flows()
  self$flows_expanded = function() {
    expander = FlowExpander(self$def)
    expander$expand_flows()
  }
  self$flows_explicit = function() {
    optional_fields = c("from_partition", "to_partition", "flow_partition", 
    "from_to_partition", "from_flow_partition", "to_flow_partition")

    
    required_partition = paste0(self$def$settings()$required_partitions, collapse = ".")
    null_partition = self$def$settings()$null_partition
    
    default_entries = data.frame(required_partition, required_partition, required_partition, "", "", null_partition)
    names(default_entries) = optional_fields
    
    default_entries = do.call("rbind", replicate(nrow(self$flows()), default_entries, simplify = FALSE))
    
    is_missing = function(field_name){
      return(!any(names(self$flows())==field_name))
    }
    missing_fields = which(lapply(optional_fields, is_missing)==TRUE)
    return(cbind(self$flows(), default_entries[,missing_fields]))
  }
  self$derivations = self$def$derivations ## TODO: make this more useful
  self$expr_list = function() {
    Derivations2ExprList(UserExpr(self), StandardExpr(self))$expr_list()
  }
  self$simulators = Simulators(self)
  (self
    |> assert_variables()
    |> return_object("Model")
  )
}

assert_variables = function(model) {
  make_pipeline = function(setting, set) {
    TestPipeline(
      Summarizer(
        extract_with_name("def"),
        run_no_op_method("settings"),
        extract_with_name(setting)
      ),
      TestSubset(set)
    )
  }
  v = model$variables$all()
  ValidityMessager(
    All(
      make_pipeline("required_partitions", v$names()),
      make_pipeline("state_variables", v$labels()),
      make_pipeline("flow_variables", v$labels()),
      make_pipeline("infectious_state_variables", v$labels()),
      make_pipeline("infected_state_variables", v$labels()),
      make_pipeline("infection_flow_variables", v$labels())
    ),
    "the settings.json file is not consistent with the variables.csv file"
  )$assert(model)
}


#' Model Starter
#'
#' Create a directory with a template model definition.
#'
#' @param starter_name Currently can only be \code{sir}.
#' @param dir_name String giving the path to a directory for copying the
#' template model definition.
#'
#' @export
model_starter = function(starter_name, dir_name) {
  starter_dir = system.file("starter_models", starter_name, package = "macpan2")
  starter_files = list.files(starter_dir)
  required_files = c(
    variables_file = "variables.csv",
    derivations_file = "derivations.json",
    flows_file = "flows.csv",
    settings_file = "settings.json"
  )
  if (!all(required_files %in% starter_files)) {
    stop("Could not find a valid starter model by that name.")
  }

  starter_paths = setNames(
    file.path(starter_dir, required_files),
    names(required_files)
  )

  if (dir.exists(dir_name)) stop("Directory for the model already exists.")
  dir.create(dir_name, recursive = TRUE)

  file.copy(starter_paths, dir_name)
  ModelFiles(dir_name)
}
