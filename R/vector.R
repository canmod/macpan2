#' @export
Vector = function(labeller) {
  self = Base()
  self$labeller = labeller
  self$.numbers = zero_vector(self$labeller$labels())
  self$numbers = function(...) {
    l = list(...)
    if (length(l) == 0L) return(self$.numbers)
    i = mp_subset(self$labeller, ...)$labels()
    self$.numbers[i]
  }
  self$set_numbers = function(...) {
    ## generate list with: value, filter_cond, select_cond, select_nm
    l = process_grouping_dots(...)

    args = c(list(self$labeller), l$filter_cond, l$select_cond)
    filter = do.call(mp_subset, args)
    replacement_values = setNames(
      l$value[filter$partition$partial_labels(l$select_nm)],
      filter$labels()
    )

    self$.numbers[names(replacement_values)] = replacement_values
    self
  }
  self$frame = function() {
    data.frame(
      labels = self$labeller$labels(),
      values = unname(self$numbers())
    )
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

zero_vector = function(labels) setNames(rep(0, length(labels)), labels)
