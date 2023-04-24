#' Model Collection
#'
#' A model definition that is untied from a set of \code{\link{ModelFiles}}.
#'
#' @param variables Return value of the `variables` method in a
#' \code{\link{ModelFiles}} object.
#' @param derivations Return value of the `derivations` method in a
#' \code{\link{ModelFiles}} object.
#' @param flows Return value of the `flows` method in a
#' \code{\link{ModelFiles}} object.
#' @param settings Return value of the `settings` method in a
#' \code{\link{ModelFiles}} object.
#'
#' @return An object of class \code{ModelCollection} with the following
#' methods.
#'
#' ## Methods
#'
#' * `$variables$all()` --
#' * `$derivations()`
#' * `$flows()`
#' * `$settings()`
#'
#' @export
ModelCollection = function(variables
    , derivations
    , flows
    , settings
  ) {
  self = Collection(variables, derivations, flows, settings)

  ## methods required of model representations
  self$variables = function() self$get("variables")
  self$derivations = function() self$get("derivations")
  self$flows = function() self$get("flows")
  self$settings = function() self$get("settings")

  return_object(self, "ModelCollection")
}

#' Model Files
#'
#' Construct objects for accessing and caching model definition files.
#'
#' @param model_directory String giving a path to a directory containing
#' the following files, `variables.csv`, `derivations.json`, `flows.csv`,
#' and `settings.json`, described by
#' [this spec](https://canmod.net/misc/model_definitions).
#' @param csv_reader Class inheriting from \code{\link{Reader}} for reading
#' csv files.
#' @param json_reader Class inheriting from \code{\link{Reader}} for reading
#' json files.
#' @param txt_reader Class inheriting from \code{\link{Reader}} for reading
#' txt files.
#'
#' @examples
#' d = system.file("starter_models", "seir_symp_vax", package = "macpan2")
#' m = ModelFiles(d)
#' m$flows()
#' expander = FlowExpander(m)
#' expander$expand_flows()
#'
#' @export
ModelFiles = function(model_directory
    , csv_reader = CSVReader
    , json_reader = JSONReader
    , txt_reader = TXTReader
) {
  self = Files(model_directory
    , reader_spec("variables.csv", csv_reader)
    , reader_spec("derivations.json", json_reader)
    , reader_spec("flows.csv", csv_reader)
    , reader_spec("settings.json", json_reader)
    , reader_spec("transmission_matrices.csv", csv_reader, optional = TRUE)
    , reader_spec("transmission_dimensions.csv", csv_reader, optional = TRUE)
  )

  ## methods required of model representations
  self$variables = function() self$get("variables")
  self$derivations = function() self$get("derivations")
  self$flows = function() self$get("flows")
  self$settings = function() self$get("settings")
  self$transmission_matrices = function() {
    self$get("transmission_matrices", optional = TRUE)
  }
  self$transmission_dimensions = function() {
    self$get("transmission_dimensions", optional = TRUE)
  }

  self$freeze = function() {
    ModelCollection(
      self$variables(),
      self$derivations(),
      self$flows(),
      self$settings()
    )
  }

  return_object(self, "ModelFiles")
}
