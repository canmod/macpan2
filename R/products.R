#' @export
Products = function(x) {
  self = Base()
  self$x = x
  self$cartesian = function(y) {
    x = self$x$frame()
    y = y$frame()
    matches = intersect(names(x), names(y))
    if (length(matches) != 0L) {
      matched_names = names(y)[names(y) %in% matches]
      maybe_nums = sub("^[^0-9]+([0-9]+)$", "\\1", matched_names)
      which_nums = grepl("^[0-9]+$", maybe_nums)
      matched_names = matched_names
      nums = as.integer(matched_names[which_nums])
      matched_names
    }
    Partition(merge(x, y))
  }
  return_object(self, "Products")
}

# methods for products. currently these are just for Partition
# objects and so they are not yet done as S3 or S4.

#' @export
cartesian = function(x, y) {
  x$products$cartesian(y)
}
