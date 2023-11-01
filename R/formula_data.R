FormulaData = function(frame, reference_index_list, labelling_names_list) {
  self = Base()
  self$frame = frame
  self$reference_index_list = reference_index_list
  self$labelling_names_list = labelling_names_list
  return_object(self, "FormulaData")
}

#' @export
print.FormulaData = function(x, ...) {
  print(x$frame, row.names = FALSE)
}
