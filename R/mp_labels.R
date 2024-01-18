#' Index Labels
#' 
#' Return a character vector of labels for each row of an index 
#' (or a ledger?? FIXME: what does this mean for ledgers??).
#' 
#' @param x Object
#' @param labelling_column_names What index columns should be used 
#' for generating the labels. If missing then defaults will be used.
#' (FIXME: clarify how the defaults are used.)
#' 
#' @export
mp_labels = function(x, labelling_column_names) {
  UseMethod("mp_labels")
}

#' @export
mp_labels.Index = function(x, labelling_column_names) {
  if (missing(labelling_column_names)) return(x$labels())
  x$partial_labels(labelling_column_names)
}

#' @export
mp_labels.Ledger = function(x, labelling_column_names) {
  x$labels_for[[labelling_column_names]]()
}
