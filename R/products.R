#' Products
#'
#' @param x Object with a `$frame` method that returns a data frame.
#'
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

#' Cartesian Product
#'
#' @param x A \code{\link{Partition}} object.
#' @param y A \code{\link{Partition}} object.
#'
#' @export
cartesian = function(x, y) x$products$cartesian(y)

#' Self-Cartesian Product
#'
#' @param x A \code{\link{Partition}} object.
#' @param left_filter Character string giving the name of the left filter.
#' @param right_filter Character string giving the name of the right filter.
#' @param .wrt Character string giving the name of a column in `x`.
#' @param .rm Character string giving the dot-concatenated names of columns
#' to remove from `x`.
#'
#' @export
cartesian_self = function(x, left_filter, right_filter, .wrt, .rm = .wrt) {
  left_prefix = var_case_to_cls_case(left_filter)
  right_prefix = var_case_to_cls_case(right_filter)
  l = cartesian(
    x$filter(left_filter, .wrt = .wrt)$select_out(to_names(.rm))$prefix(left_prefix),
    x$filter(right_filter, .wrt = .wrt)$select_out(to_names(.rm))$prefix(right_prefix)
  )
  nms = l$names()
  nms_left = head(nms, length(nms) / 2L)
  nms_right = tail(nms, length(nms) / 2L)
  list(
    l$partial_labels(nms_left),
    l$partial_labels(nms_right)
  ) |> setNames(c(left_filter, right_filter))
}

