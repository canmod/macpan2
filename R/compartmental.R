#' Compartmental Model
#'
#' Create an object for containing a compartmental model.
#'
#' @param model_directory String giving a path to a directory containing
#' the following files, `variables.csv`, `derivations.json`, `flows.csv`,
#' and `settings.json`, described by
#' [this spec](https://canmod.github.io/macpan2/articles/model_definitions).
#'
#' @return An object with the following methods and fields.
#'
#' ## Methods
#'
#' * `$variables$all()`: \code{\link{Partition}} object of all variables.
#' * `$flows()`: Data frame with rows giving all groups of flows.
#' * `$flows_expanded()`: Data frame with rows giving all individual flows.
#' * `$variables$state()`: \code{\link{Partition}} object of all state
#' variables.
#' * `$variables$flow()`: \code{\link{Partition}} object of all flow variables.
#' * `$all_labels()`: Character vector giving the labels of all variables.
#' * `$labels$state()`: Character vector giving the labels of all state
#' variables.
#' * `$labels$flow()`: Character vector giving the labels of all flow variables.
#' * `$labels$other()`: Character vector giving the labels of all variables
#' that are neither state nor flow variables.
#' * `$expr_list()`: \code{\link{ExprList}} object containing all expressions.
#'
#' ## Fields
#'
#' * `$def`: The \code{\link{ModelFiles}} object representing the directory
#' that defines the model.
#' * `$simulators`: A \code{\link{Simulators}} instance containing methods for
#' generating simulators, which are objects that can generate simulations from
#' the model.
#'
#' @export
Compartmental = function(model_directory) {
  self = Model(ModelFiles(model_directory))
  return_object(self, "Compartmental")
}

CompartmentalMatsList = function(
      state
    , flow
    , ...
    , .mats_to_save = character(0L)
    , .mats_to_return = character(0L)
    , .dimnames = list()
  ) {
    MatsList(
      state = state
    , flow = flow
    , ...
    , state_length = length(state)
    , per_capita_from = empty_matrix
    , per_capita_to = empty_matrix
    , per_capita_flow = empty_matrix
    , absolute_from = empty_matrix
    , absolute_to = empty_matrix
    , absolute_flow = empty_matrix
    , per_capita_inflow_from = empty_matrix
    , per_capita_inflow_to = empty_matrix
    , per_capita_inflow_flow = empty_matrix
    , per_capita_outflow_from = empty_matrix
    , per_capita_outflow_to = empty_matrix
    , per_capita_outflow_flow = empty_matrix
    , absolute_inflow_from = empty_matrix
    , absolute_inflow_to = empty_matrix
    , absolute_inflow_flow = empty_matrix
    , absolute_outflow_from = empty_matrix
    , absolute_outflow_to = empty_matrix
    , absolute_outflow_flow = empty_matrix
    , per_capita = empty_matrix
    , absolute = empty_matrix
    , per_capita_inflow = empty_matrix
    , per_capita_outflow = empty_matrix
    , absolute_inflow = empty_matrix
    , absolute_outflow = empty_matrix
    , total_inflow = empty_matrix
    , total_outflow = empty_matrix
    , dummy = empty_matrix
    , .mats_to_save = .mats_to_save
    , .mats_to_return = .mats_to_return
    , .dimnames = .dimnames
    )
  }
