#' @export
descriptors = function(..., labelling_names) {
  f = data.frame(...)
  if (missing(labelling_names)) labelling_names = names(f)
  Descriptors(f, to_names(labelling_names))
}

#' @export
Descriptors = function(partition, labelling_names = names(frame)) {
  UseMethod("Descriptors")
}

#' @export
Descriptors.Partition = function(partition, labelling_names = names(frame)) {
  self = Base()
  self$partition = partition
  self$labelling_names = to_names(labelling_names)
  self$labels = function() self$partition$select(self$labelling_names)$labels()
  self$partial_labels = function(...) self$partition$partial_labels(...)
  return_object(self, "Descriptors")
}

#' @export
Descriptors.data.frame = function(partition, labelling_names = names(frame)) {
  partition |> Partition() |> Descriptors(labelling_names)
}

#' @export
Descriptors.Descriptors = function(partition, labelling_names = names(frame)) {
  partition$partition |> Descriptors(labelling_names)
}

#' @export
print.Descriptors = function(x, ...) print(x$partition)

#' @export
names.Descriptors = function(x) x$partition$names()
