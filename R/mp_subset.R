#' Subset of Indexes
#'
#' Take a subset of the rows of an index table (see \code{\link{mp_index}})
#' to produce another index table. The `mp_subset` function gives rows that
#' match a certain criterion and `mp_setdiff` gives rows that do not match.
#'
#' @param x Model index.
#' @param ... Name-value pairs. The names are columns (or sets of columns
#' using dot-concatenation) in \code{x} and the values are character vectors
#' that refer to labels with respect to those columns. These values
#' determine the resulting subset.
#'
#' @family indexes
#' @export
mp_subset = function(x, ...) {
  partition = mp_choose(x, "pick", ...)$partition
  Index(partition
    , labelling_column_names = x$labelling_column_names
    , reference_index = x
  )
}

#' @rdname mp_subset
#' @export
mp_setdiff = function(x, ...) {
  partition = mp_choose_out(x, "pick", ...)$partition
  Index(partition
    , labelling_column_names = x$labelling_column_names
    , reference_index = x
  )
}


## TODO: clarify what mp_choose and mp_choose_out do specifically
mp_choose = function(x, subset_name, ...) {
  l = list(...)
  if (length(l) != 0L) valid$named_list$check(l)
  p = x$partition
  for (cc in names(l)) {
    vals = l[[cc]]
    is_blank = nchar(vals) == 0L
    vals = setdiff(vals, "")
    if (any(is_blank)) {
      p = union_vars(p$blank_on(cc), p$filter(vals, .wrt = cc))
    } else {
      p = p$filter(vals, .wrt = cc)
    }
  }
  init_merge(p$frame(), subset_name, x$reference_index(), x$labelling_column_names)
}

mp_choose_out = function(x, subset_name, ...) {
  l = list(...)
  p = x$partition
  for (cc in names(l)) {
    vals = l[[cc]]
    p = p$filter_out(vals, .wrt = cc)
  }
  init_merge(p$frame(), subset_name, x$reference_index(), x$labelling_column_names)
}
