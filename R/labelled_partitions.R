#' Partition
#'
#' Create object for manipulating partitions, which are sets of
#' labels for representing and naming model entities.
#'
#' @param frame Data frame representing the partition. The column names must
#' consist only of letters, numbers, and start with a letter. The columns of
#' the data frame must be character vectors such that each value is composed
#' entirely of letters, numbers, underscore, and must start with a letter
#' unless it is a blank string. Missing values are not allowed, but blank
#' strings are. Each row must be unique.
#'
#' @return Object of class \code{Partition} with the following methods.
#'
#' ## Methods
#'
#' * `$frame()` -- The `Partition` object as a data frame.
#' * `$dotted()` -- The `Partition` object as a data frame with one united column.
#' * `$names()` -- The names of the `Partition` (i.e. the column names).
#' * `$name()` -- The name of the `Partition` (i.e. the dot-concatenated column names).
#' * `$labels()` -- The labels of the `Partition` (i.e. the row-wise dot-concatenated columns).
#' * `$partial_labels()` -- TODO
#' * `$filter(..., .wrt, .comparison_function)` -- Filter by keeping only a subset of labels.
#' * `$filter_out(..., .wrt, .comparison_function)` -- Filter by removing a subset of labels.
#' * `$filter_ordered(..., .wrt, .comparison_function = all_equal)` -- Filter and order by labels.
#'    * `...` -- Labels to filter
#'    * `.wrt` -- The filtering labels are with respect to a particular `Partition`, and
#'    `.wrt` is the name of this `Partition`.
#'    * `.comparison_function` -- Boolean function to decide if each filtering label is
#'    equal to each label in the `Partition`.
#' * `$select(...)` -- Create a new `Partition` with a subset of names. The rows of the new
#' `Partition` are de-duplicated.
#' * `$select_out(...)` -- Create a new `Partition` without a subset of names.
#'    * `...` -- Names to keep in the resulting `Partition`.
#' * `$expand(name)` -- TODO
#' * `$union(other)` -- TODO
#'
#' ## Fields
#'
#' * `products`
#'
#' @export
Partition = function(frame) {
  self = Base()
  self$.partition = frame_to_part(frame)
  self$products = Products(self)
  self$frame = function() self$.partition$frame()
  self$dotted = function() self$.partition$dot()$frame()
  self$names = function() names(self$frame())
  self$name = function() names(self$dotted()) ## inefficient without memoisation
  self$labels = function() self$dotted()[[1L]]
  self$prefix = function(prefix) {
    f = self$frame()
    names(f) = sprintf("%s%s", prefix, self$names())
    Partition(f)
  }
  self$constant = function(name, value) {
    f = self$frame()
    f[[name]] = value
    Partition(f)
  }
  self$partial_labels = function(...) {
    new_names = list_to_names(...)
    self$.partition$change_coordinates(new_names)$dot()$frame()[[1L]]
  }
  self$.filter = function(..., .wrt, .comparison_function, .filter_type) {
    if (missing(.wrt)) .wrt = self$name()

    ## order in .wrt matters. .wrt has to come first or ordering can get
    ## mangled by the set intersection operation
    .wrt = name_set_op(.wrt, self$name(), intersect)

    nms = to_names(.wrt)
    labels = list_to_labels(...)
    if (is.null(labels)) {  ## no filtering names are supplied
      p = NullPartition(.wrt)
    } else if (!any(labels %in% self$select(nms)$labels())) {  ## no filtering names match
      p = NullPartition(.wrt)
    } else {  ##
      filterer = StringDataFromDotted(labels = labels, name = .wrt)
      p = Partition(self$.partition[[.filter_type]](filterer, .comparison_function)$frame())
    }
    return(p)
  }
  self$blank_on = function(.wrt) {
    f = self$frame()
    cols = to_names(.wrt)
    z = rep(TRUE, nrow(f))
    for (col in cols) {
      z = z & (f[[col]] == "")
    }
    Partition(f[z, , drop = FALSE])
  }
  self$filter = function(..., .wrt, .comparison_function = all_equal) {
    self$.filter(...
      , .wrt = .wrt
      , .comparison_function = .comparison_function
      , .filter_type = "filter"
    )
  }
  self$filter = memoise(self$filter)
  self$filter_out = function(..., .wrt, .comparison_function = not_all_equal) {
    self$.filter(...
      , .wrt = .wrt
      , .comparison_function = .comparison_function
      , .filter_type = "filter"
    )
  }
  self$filter_ordered = function(..., .wrt, .comparison_function = all_equal) {
    self$.filter(...
      , .wrt = .wrt
      , .comparison_function = .comparison_function
      , .filter_type = "ordered_unique_filter"
    )
  }
  self$select = function(...) {
    Partition(unique(self$.partition$change_coordinates(...)$frame()))
  }
  self$select_out = function(...) {
    self$select(setdiff(self$names(), unlist(list(...), recursive = TRUE)))
  }
  self$expand = function(name) Partition(self$.expand(name))
  self$.expand = function(name) self$.partition$expand(name)$frame()
  self$union = function(other) {
    new_name = name_set_op(self$name(), other$name(), union)
    x = self$.expand(new_name)
    y = other$.expand(new_name)
    Partition(rbind(x, y))
  }
  return_object(self, "Partition")
}
Partition = memoise(Partition)

#' @export
names.Partition = function(x) x$names()


vec = function(x, vec_name, vec_partition = "Vec") {
  x$filter(vec_name, .wrt = vec_partition)$select_out(vec_partition)
}


NullPartition = function(...) {
  self = Base()
  self$.names = list_to_names(...)
  self$frame = function() empty_frame(self$.names)
  self$dotted = function() empty_frame(to_name(self$.names))
  self$names = function() self$.names
  self$name = function() to_name(self$.names)
  self$labels = function() character(0L)
  self$partial_labels = function(...) {
    stopifnot(all(list_to_names(...) %in% self$names()))
    self$labels()
  }
  self$filter = function(..., .wrt, .comparison_function = all_equal) self
  self$filter_out = function(..., .wrt, comparison_function = not_all_equal) self
  self$filter_ordered = function(..., .wrt, .comparison_function = all_equal) self
  self$select = function(...) {
    new_names = list_to_names(...)
    stopifnot(all(new_names %in% self$names()))
    NullPartition(new_names)
  }
  self$select_out = function(...) {
    new_names = name_set_op(self$name(), list_to_names(...), setdiff)
    NullPartition(new_names)
  }
  self$expand = function(name) NullPartition(names(self$.expand(name)))
  self$.expand = function(name) empty_frame(name_set_op(self$name(), name, union))
  self$union = function(other) {
    new_name = name_set_op(self$name(), other$name(), union)
    other$expand(new_name)
  }
  self$new = function(frame) {
    if (nrow(frame) == 0L) return(NullPartition(names(frame)))
    Partition(frame)
  }
  self = return_object(self, "Partition")
  return_object(self, "NullPartition")
}

#' @export
print.Partition = function(x, ...) print(x$frame(), row.names = FALSE)

empty_frame = function(...) {
  colnames = unlist(
    lapply(list(...), as.character),
    use.names = FALSE,
    recursive = TRUE
  )
  ncol = length(colnames)
  setNames(as.data.frame(matrix(character(), 0L, ncol)), colnames)
}

enforce_schema = function(frame, ...) {
  anchor = empty_frame(...)
  if (is.null(frame)) return(anchor)
  anchor = as.list(anchor)
  for (c in names(anchor)) {
    if (c %in% names(frame)) {
      anchor[[c]] = frame[[c]]
    } else {
      anchor[[c]] = rep("", nrow(frame))
    }
  }
  as.data.frame(anchor)
}

#' Union of Variables
#'
#' Take the union of a set of variable lists, each of which is represented
#' by a \code{\link{Partition}} object.
#'
#' @param ... \code{\link{Partition}} objects to combine.
#'
#' @export
union_vars = function(...) {
  not_null = function(x) !is.null(x)
  l = Filter(not_null, list(...))
  #vec_parts = vec_part_names(l)
  y = l[[1L]]
  if (length(l) > 1L) {
    for (i in 2:length(l)) {
      y = y$union(l[[i]])
    }
  }
  y
}


vec_part_names = function(...) {
  (list(...)
   |> lapply(getElement, "vec")
   |> unlist(recursive = FALSE, use.names = FALSE)
   |> unique()
  )
}

NumericPartition = function(frame, numeric_vector) {
  if (nrow(frame) != length(numeric_vector)) stop("Inconsitent numeric partition.")
  self = Base()
  self$partition = Partition(frame)
  self$vector = setNames(
    valid$num_vec$assert(numeric_vector),
    self$partition$select_out("Matrix")$labels()
  )
  self$filter_vector = function(..., .wrt, .comparison_function = all_equal) {
    l = self$partition$filter(...
      , .wrt = .wrt
      , .comparison_function = .comparison_function
    )$select_out("Matrix")$labels()
    self$vector[l]
  }
  self$matrix = function(name, row_part, col_part) {
    row_col_names = character(0L)
    if (row_part != "") row_part = row_col_names = to_names(row_part)
    if (col_part != "") {
      col_part = to_names(col_part)
      if (length(intersect(col_part, row_part)) != 0L) {
        stop("Row and column partitions must be mutually exclusive.")
      }
      row_col_names = c(row_col_names, col_part)
    }
    all_names = self$partition$select_out("Matrix")$names()
    if (!all(row_col_names %in% all_names)) {
      stop("Matrix row and column labels must use all label components.")
    }
    if (!all(all_names %in% row_col_names)) {
      stop("Some row/column labels not found in the partition.")
    }
    v = self$filter_vector(name, .wrt = "Matrix")
    if (isTRUE(row_part == "")) {
      rn = ""
      i = 1
    } else {
      r = self$partition$filter(name, .wrt = "Matrix")$partial_labels(row_part)
      rn = unique(r)
      i = match(r, rn)
    }
    if (isTRUE(col_part == "")) {
      cn = ""
      j = 1
    } else {
      c = self$partition$filter(name, .wrt = "Matrix")$partial_labels(col_part)
      cn = unique(c)
      j = match(c, cn)
    }
    m = matrix(0, length(rn), length(cn))
    m[cbind(i, j)] = v
    dimnames(m) = list(rn, cn)
    m
  }
  return_object(self, "NumericPartition")
}

#' @export
join_partitions = function(x, y, by = "") {
  by = by_(by)
  merge(x, y
    , by.x = by$x, by.y = by$y
    , suffixes = suffixes_(x, y)
    , sort = FALSE
  )
}

#' @export
by_ = function(by) UseMethod("by_")

#' @export
by_.character = function(by) {
  if (identical(nchar(by), 0L) | isTRUE(is.na(by))) return(list())
  by = process_by_char(by)
  list(x = by, y = by)
}
#' @export
by_.formula = function(by) {
  if (is_one_sided(by)) return(by_(rhs_char(by)))
  list(
    x = process_by_char(lhs_char(by)),
    y = process_by_char(rhs_char(by))
  )
}
#' @export
by_.NULL = function(by) list()
suffixes_ = function(x, y) {
  s = c(names(x)[ncol(x)], names(y)[ncol(y)])
  sprintf(":%s", s)
}
process_by_char = function(by) {
  (by
   |> undot_anything()
   #|> wrap_colon_terms()
  )
}

  # if (length(bad_names) != 0L) {
  #   macpan2:::msg_break(
  #     macpan2:::msg_colon(
  #       macpan2:::msg(
  #         "These partition names where asked for (via the include argument)",
  #         "but were not present in the output"
  #       ),
  #       macpan2:::msg_indent(bad_names)
  #     ),
  #     macpan2:::msg_colon(
  #       "Only these were available",
  #       macpan2:::msg_indent(names(z))
  #     )
  #   ) |> stop()
  # }
  #z[, include, drop = FALSE] |> Partition()
#}
