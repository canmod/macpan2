#' @export
basis = function(..., labelling_names) {
  f = data.frame(...)
  if (missing(labelling_names)) labelling_names = names(f)
  Basis(f, to_names(labelling_names))
}

#' @export
Basis = function(partition, labelling_names = names(frame)) {
  UseMethod("Basis")
}

#' @export
Basis.Partition = function(partition, labelling_names = names(frame)) {
  self = Base()
  self$partition = partition
  self$labelling_names = to_names(labelling_names)
  self$labels = function() self$partition$select(self$labelling_names)$labels()
  self$partial_labels = function(...) self$partition$partial_labels(...)
  return_object(self, "Basis")
}

#' @export
Basis.data.frame = function(partition, labelling_names = names(frame)) {
  partition |> Partition() |> Basis(labelling_names)
}

#' @export
Basis.Basis = function(partition, labelling_names = names(frame)) {
  partition$partition |> Basis(labelling_names)
}

#' @export
print.Basis = function(x, ...) print(x$partition)

#' @export
names.Basis = function(x) x$partition$names()
