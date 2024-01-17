#' @export
mp_labels = function(x, labelling_column_names) {
  UseMethod("mp_labels")
}

#' @export
mp_labels.Index = function(x, labelling_column_names) {
  if (missing(labelling_column_names)) return(x$labels())
  x$partial_labels(labelling_column_names)
}
