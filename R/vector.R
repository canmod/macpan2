#' @export
Vector = function(x, ...) UseMethod("Vector")

#' @export
Vector.data.frame = function(x, index, values_name = "values", ...) {
  nms = setdiff(names(x), values_name)
  bad_names = !nms %in% names(index)
  if (any(bad_names)) {
    msg_break(
      msg_colon(
        msg(
          "The following names were found in the values data",
          "but not in the model index"
        ),
        msg_indent(nms[bad_names])
      ),
      msg_colon(
        "The only names in the index are the following",
        msg_indent(names(index))
      )
    ) |> stop()
  }

  index_names = !names(x) %in% values_name
  test_index = try(Index(x[, index_names, drop = FALSE]))
  if (inherits(test_index, "try-error")) stop("Incompatible values data frame")
  bad_labels = !test_index$labels() %in% index$partial_labels(test_index$labelling_names)
  if (any(bad_labels)) {
    msg_colon(
      msg(
        "The following labels found in the values data",
        "are being ignored because they are not in the model index"
      ),
      msg_indent(test_index$labels()[bad_labels])
    ) |> warning()
  }

  f = merge(index$partition$frame(), x, all.x = TRUE, sort = FALSE) ## left join
  index_names = !names(f) %in% values_name  ## redefine index_names after merge
  mangled_order = Index(
    f[, index_names, drop = FALSE],
    labelling_names = index$labelling_names
  )$labels()
  sorted_positions = match(index$labels(), mangled_order)
  values = f[[values_name]][sorted_positions]
  values[is.na(values)] = 0  ## that's the way we roll
  f = f[sorted_positions, index_names, drop = FALSE]
  index = Index(f, labelling_names = index$labelling_names)
  v = Vector(index)
  v$set_all_numbers(values)
}

#' @export
Vector.numeric = function(x, index, ...) {
  v = Vector(index)
  args = setNames(list(x), to_name(index$labelling_names))
  do.call(v$set_numbers, args)
}

#' @export
Vector.Index = function(x, ...) {
  self = Base()
  self$index = x
  self$.numbers = zero_vector(self$index$labels())
  self$numbers = function(...) {
    l = list(...)
    if (length(l) == 0L) return(self$.numbers)
    i = mp_subset(self$index, ...)$labels()
    self$.numbers[i]
  }
  self$set_all_numbers = function(values) {
    self$.numbers = setNames(values, names(self$.numbers))
    self
  }
  self$set_numbers = function(...) {
    ## generate list with: value, filter_cond, select_cond, select_nm
    l = process_grouping_dots(...)

    args = c(list(self$index), l$filter_cond, l$select_cond)
    filter = do.call(mp_subset, args)
    replacement_values = setNames(
      l$value[filter$partition$partial_labels(l$select_nm)],
      filter$labels()
    )

    self$.numbers[names(replacement_values)] = replacement_values
    self
  }
  self$labels_frame = function() {
    data.frame(
      labels = self$index$labels(),
      values = unname(self$numbers())
    )
  }
  self$frame = function() {
    f = self$index$partition$frame()
    f$values = self$numbers()
    f
  }
  self$csv = function(file) {
    write.csv(self$frame(), file, row.names = FALSE, quote = FALSE)
  }
  self$length = function() length(self$.numbers)
  return_object(self, "Vector")
}


process_grouping_dots = function(...) {
  l = list(...)
  replacement = Filter(is.numeric, l)
  stopifnot(length(replacement) == 1L)
  filter_cond = Filter(is.character, l)
  value = replacement[[1L]]
  select_cond = lapply(replacement, names)
  select_nm = names(select_cond)
  nlist(value, filter_cond, select_cond, select_nm)
}

#' @export
length.Vector = function(x) x$length()

#' @export
print.Vector = function(x, ...) print(x$numbers())

#' @export
as.matrix.Vector = function(x, ...) x$numbers() |> as.matrix()

zero_vector = function(labels) setNames(rep(0, length(labels)), labels)

#' @export
mp_vector = Vector

#' @export
mp_set_numbers = function(vector, ...) vector$set_numbers(...)
