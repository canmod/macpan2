#' Group an Index
#' 
#' Create a new index with fewer columns to create names for
#' an aggregated vector that is labelled by the input index.
#' 
#' @param index Index to group rows.
#' @param by Column set label to group by.
#'
#' @export
mp_group = function(index, by) {
  frame = index$partition$select(to_names(by))$frame()
  nms = names(frame)[names(frame) %in% index$labelling_column_names]
  Index(frame, labelling_column_names = nms)
}

mp_factor = function(index, by) {
  frame = index$partition$select(to_names(by))$frame()
  nms = names(frame)[names(frame) %in% index$labelling_column_names]
  Index(frame, labelling_column_names = nms, reference_index = index)
}

#' Aggregate an Index
#'
#' Create a one-column ledger (see \code{\link{LedgerDefinition}}) with rows
#' identifying instances of an aggregation.
#'
#' @param index An index to aggregate over.
#' @param by A column set label to group by. By default a dummy
#' and constant \code{"Group"} column is created.
#' @param ledger_column Name of the column in the output ledger
#' that describes the groups.
#'
#' @family ledgers
#' @export
mp_aggregate = function(index, by = "Group", ledger_column = "group") {
  index_columns = to_names(by)
  if (length(index_columns) == 1L & !index_columns %in% names(index)) {
    partition = index$partition$constant(by, "a")
  } else {
    partition = index$partition
  }
  index = Index(partition) |> mp_group(by)

  Ledger(
    partition$frame(),
    initial_column_map(names(partition), ledger_column),
    initial_reference_index_list(index, ledger_column),
    setNames(list(index_columns), ledger_column)
  )
}
