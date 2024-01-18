Settings = function(model) {
  self = Base()
  self$model = model
  self$.settings = model$def$settings
  self$.synonyms = list()
  self$.synonyms$lab_part = c(
      "required_partitions", "required", "req_part"
    , "labelling_partitions", "labelling_partition", "labelling_part", "lab_part"
  )
  self$.synonyms$var_part = c(
      "vector_partition", "vector_partitions", "vec_partition", "vec_partition"
    , "var_partitions", "variable_partitions"
  )
  self$.resolve_synonyms = function(type) {
    nms = names(self$.settings())
    i = nms %in% self$.synonyms[[type]]
    if (!any(i)) {
      (
        msg(
          "At least one of the following fields must be in the settings.json",
          "file:\n%s\n",
          "The first one encountered will be used to determine the",
          "columns in the variables.csv file used to generate labels for",
          "the quantities in that file."
        )
        |> sprintf(nms)
        |> stop()
      )
    }
    nms[min(which(i))] # take the first synonym found
  }
  self$lab_part = function() self$.resolve_synonyms("lab_part")
  self$name = function() to_name(self$.settings()[[self$lab_part()]])
  self$names = function() to_names(self$.settings()[[self$lab_part()]])
  self$null = function() self$.settings()$null_partition
  self$var_part = function() {
    self$.settings()[[self$.resolve_synonyms("var_part")]]
  }
  self$variable = function(type) {
    type_nm = sprintf("%s_variables", type)
    var_nms = self$.settings()[[type_nm]]
    as.character(var_nms)  ## treat NULL values as length-zero character vectors
  }
  self$state = function() self$variable("state")
  self$flow = function() self$variable("flow")
  self$infectious_state = function() self$variable("infectious_state")
  self$infected_state = function() self$variable("infected_state")
  self$infection_flow = function() self$variable("infection_flow")
  return_object(self, "Settings")
}
