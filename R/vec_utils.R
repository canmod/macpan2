
#' @export
mp_zero_vector = function(x, ...) {
  UseMethod("mp_zero_vector")
}

#' @export
mp_zero_vector.character = function(x, ...) {
  (x
    |> as.vector()
    |> zero_vector()
  )
}

#' @export
mp_zero_vector.Index = function(x, labelling_column_names, ...) {
  (x
   |> mp_subset(...)
   |> mp_labels(labelling_column_names)
   |> zero_vector()
  )
}

#' @export
mp_labels.Ledger = function(x, labelling_column_names) {
  x$labels_for[[labelling_column_names]]()
}
