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
    self$def$defaults()[!names(self$def$defaults()) %in% c("Value", "Notes")],
    as.numeric(self$def$defaults()[["Value"]])
  )

  self$.special = function() {
    special_columns = c("Notes")
    names(self$def$defaults()) %in% special_columns
  }
  self$.numeric = function() {
    names(self$def$defaults()) %in% "Value"
  }
  self$.matrix = function() {
    names(self$def$defaults()) %in% "Matrix"
  }
  self$.part_cols = function() {
    !(self$.special() | self$.numeric() | self$.matrix())
  }
  self$.neither_special_nor_numeric = function() {
    !(self$.special() | self$.numeric())
  }
  self$initialized_variables = function() {
    Partition(self$def$defaults()[self$.part_cols()])
  }
  self$matrix_names = function() unique(self$def$defaults()$Matrix)
  self$initialized_matrix = function(name) {
    i = self$def$dimensions()$Matrix == name
    if (all(!i)) {
      i = self$def$defaults()$Matrix == name
      if (sum(i) == 1L) {
        m = matrix(as.numeric(self$def$defaults()$Value[i]), 1, 1)
        dimnames(m) = list("", "")
        return(m)
      }
    }
    row_part = self$def$dimensions()[i, "Row", drop = TRUE]
    col_part = self$def$dimensions()[i, "Col", drop = TRUE]
    self$numeric$matrix(name, row_part, col_part)
    # macpan2:::NumericPartition(
    #   self$def$defaults()[i, self$.neither_special_nor_numeric(), drop = FALSE],
    #   as.numeric(self$def$defaults()[i, self$.numeric(), drop = TRUE])
    # )
  }
  #self$initialized_matrix = function(name) {
  #  x = self$initialized_matrix_long(name)
  #}
  self$.make_matrix = function(name, row_part, col_part) {
    i = self$def$defaults()$Matrix == name
    vals = self$def$defaults()[i, "Value"]
    row_part = StringDottedScalar(row_part)
    col_part = StringDottedScalar(col_part)
    self$initialized_variables()$select(row_part$undot())$filter()
  }
  return_object(self, "Defaults")
}
