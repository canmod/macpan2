#' @export
core = function(..., labelling_names) {
  f = data.frame(...)
  if (missing(labelling_names)) labelling_names = names(f)
  Core(f, to_names(labelling_names))
}

#' @export
Core = function(frame, labelling_names = names(frame)) {
  self = Base()
  self$labelling_names = to_names(labelling_names)
  self$partition = Partition(frame)
  self$labels = function() self$partition$select(self$labelling_names)$labels()
  return_object(self, "Core")
}

#' @export
print.Core = function(x, ...) print(x$partition)
