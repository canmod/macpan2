#' Union of Indexes
#'
#' @param ... Indexes.
#'
#' @family indexes
#' @export
mp_union = function(...) UseMethod("mp_union")

#' @export
mp_union.Index = function(...) {
  l = list(...)
  partitions = lapply(l, getElement, "partition")
  labelling_column_names = (l
    |> lapply(getElement, "labelling_column_names")
    |> unlist(recursive = FALSE, use.names = FALSE)
    |> unique()
  )
  Index(do.call(union_vars, partitions)$frame(), labelling_column_names = labelling_column_names)
}

## not used anymore?
#' @export
mp_union.Ledger = function(...) {
  l = list(...)
  column_map = lapply(l, getElement, "column_map") |> unique()
  if (length(column_map) != 1L) {
    msg_colon(
      msg(
        "Union of inconsistent Ledger objects.",
        "All Ledger objects must have the same",
        "column_map, but the following distinct",
        "maps were found:"
      ),
      msg_indent_break(lapply(column_map, unlist))
    ) |> stop()
  }
  ## TODO: should really be checking for reference_index_list
  labelling_column_names_list = lapply(l, getElement, "labelling_column_names_list") |> unique()
  if (length(labelling_column_names_list) != 1L) {
    msg_colon(
      msg(
        "Union of inconsistent Ledger objects.",
        "All Ledger objects must have the same",
        "labelling_column_names_list, but the following",
        "distinct maps were found:"
      ),
      msg_indent_break(lapply(labelling_column_names_list, unlist))
    ) |> stop()
  }
  frame = mp_rbind(...)
  LedgerData(frame, l[[1L]]$reference_index_list, l[[1L]]$labelling_column_names_list)
}

## not used anymore?
mp_rbind = function(...) {
  (list(...)
   |> lapply(as.data.frame)
   |> do.call(what = bind_rows)  ## bind_rows is in frame_utils.R
  )
}
