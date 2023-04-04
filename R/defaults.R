DefaultFiles = function(defaults_directory) {
  self = Files(defaults_directory
    , reader_spec("defaults.csv", CSVReader)
    , reader_spec("flows.csv", CSVReader)
    , reader_spec("derivations.json", JSONReader)
    , reader_spec("dimensions.csv", CSVReader)
  )

  self$defaults = function() self$get("defaults")
  self$flows = function() self$get("flows")
  self$derivations = function() self$get("derivations")
  self$dimensions = function() self$get("dimensions")

  return_object(self, "DefaultFiles")
}

Defaults = function(default_files) {
  self = Base()
  self$def = default_files
  self$numeric = NumericPartition(
    self$def$defaults()[names(self$def$defaults()) != "Value"],
    as.numeric(self$def$defaults()[["Value"]])
  )

  self$.special = function() {
    special_columns = c("Matrix", "Value")
    names(self$def$defaults()) %in% special_columns
  }
  self$initialized_variables = function() {
    Partition(self$def$defaults()[!self$.special()])
  }
  self$matrix_names = function() self$def$dimensions()$Matrix
  self$initialized_matrix_long = function(name) {
    i = self$def$defaults()$Matrix == name
    Partition(self$initialized_variables()$frame()[i, !self$.special(), drop = FALSE])
  }
  self$initialized_matrix = function(name) {
    x = self$initialized_matrix_long(name)
  }
  self$.make_matrix = function(name, row_part, col_part) {
    i = self$def$defaults()$Matrix == name
    vals = self$def$defaults()[i, "Value"]
    row_part = macpan2:::StringDottedScalar(row_part)
    col_part = macpan2:::StringDottedScalar(col_part)
    self$initialized_variables()$select(row_part$undot())$filter()
  }
  return_object(self, "Defaults")
}
