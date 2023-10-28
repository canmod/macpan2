#' Partition
#'
#' Create object for manipulating partitions, which are sets of
#' labels for representing and naming model entities.
#'
#' @param frame Data frame representing the partition.
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
    .wrt = name_set_op(self$name(), .wrt, intersect)
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

ColumnGetter = function(labelled_partition, dimension_name, column_name) {
  self = Base()
  self$labelled_partition = labelled_partition
  self$dimension_name = dimension_name
  self$column_name = column_name
  self$get = function() {
    lp = self$labelled_partition
    i = lp$column_map[[self$dimension_name]][[self$column_name]]
    lp$frame[[i]]
  }
  self = return_object(self, "ColumnGetter")
  self$get
}

FrameGetter = function(labelled_partition, dimension_name) {
  self = Base()
  self$labelled_partition = labelled_partition
  self$dimension_name = dimension_name
  self$get_frame = function() {
    i = unlist(
      self$labelled_partition$column_map[[self$dimension_name]],
      use.names = FALSE
    )
    setNames(
      self$labelled_partition$frame[, i, drop = FALSE],
      names(self$labelled_partition$column_map[[self$dimension_name]])
    )
  }
  self$get_partition = function() self$get_frame() |> Partition()
  self$get_labels = function() {
    i = self$labelled_partition$labelling_names()
    f = self$get_frame()
    i = i[i %in% names(f)]
    f[, i, drop = FALSE] |> as.list()
    do.call(paste, c(f, sep = "."))
  }
  return_object(self, "FrameGetter")
}

#' @export
initial_column_map = function(column_names, dimension_name) {
  setNames(
    list(setNames(as.list(column_names), column_names)),
    dimension_name
  )
}

## take two Merge objects and merge their frames
## and update the provenance-preserving column maps
merge_util = function(x, y, by.x, by.y) {

  ## ----
  ## resolve non-unique column names in the output, with
  ## names of the original tables used to this point
  ## ----
  suffixes = c(
    paste0(sprintf(":%s", names(x$column_map)), collapse = ""),
    paste0(sprintf(":%s", names(y$column_map)), collapse = "")
  )

  ## ----
  ## the merge itself
  ## ----
  z = merge(
    x$frame, y$frame,
    by.x = by.x, by.y = by.y,
    suffixes = suffixes
  )

  ## ----
  ## update the column name map to preserve provenance
  ## ----

  # column names in inputs and output
  x_cnms = names(x$frame)
  y_cnms = names(y$frame)
  z_cnms = names(z)

  # numbers of types of output columns
  n_common = length(by.x)
  n_x_only = length(x_cnms) - n_common
  n_y_only = length(y_cnms) - n_common

  # column names in the output categorized
  # by contributing input table(s)
  z_common = z_cnms[seq_len(n_common)]
  z_x_only = z_cnms[n_common + seq_len(n_x_only)]
  z_y_only = z_cnms[n_common + n_x_only + seq_len(n_y_only)]

  # column names in the input tables, ordered
  # as they are in the output table
  x_z_order = c(by.x, x_cnms[!x_cnms %in% by.x])
  y_z_order = c(by.y, y_cnms[!y_cnms %in% by.y])

  # these can be used to update the column map,
  # by looping through the existing map and
  # translating to the new column names with these
  # x|y_map vectors. of course you will also need
  # to concatenate the column maps first
  x_map = setNames(c(z_common, z_x_only), x_z_order)
  y_map = setNames(c(z_common, z_y_only), y_z_order)

  x_cmap = x$column_map
  y_cmap = y$column_map
  for (tnm in names(x_cmap)) {
    for (cnm in names(x_cmap[[tnm]])) {
      old_cnm = x_cmap[[tnm]][[cnm]]
      x_cmap[[tnm]][[cnm]] = x_map[[old_cnm]]
    }
  }
  for (tnm in names(y_cmap)) {
    for (cnm in names(y_cmap[[tnm]])) {
      old_cnm = y_cmap[[tnm]][[cnm]]
      y_cmap[[tnm]][[cnm]] = y_map[[old_cnm]]
    }
  }

  z_column_map = c(x_cmap, y_cmap)

  ## ----
  ## wrap up the result with provenance-preserving column map
  ## ----
  Merged(
    z,
    z_column_map
  )
}

#' @export
init_merge = function(frame, dimension_name) {
  Merged(frame
    , initial_column_map(names(frame), dimension_name)
  )
}

mp = function(mp_func) {
  f = ("mp_%s"
    |> sprintf(mp_func)
    |> getFromNamespace("macpan2")
  )
  prototype = function(...) {l = list(...)}
  target_e = body(f)[[2L]]
  proto_e = body(prototype)[[2L]]
  if (!identical(target_e, proto_e)) stop("developer error: invalid mp function")
  body(f)[[2L]][[3L]] = (~unlist(list(...), recursive = FALSE))[[2L]]
  f
}

#' @export
mp_map_to_selection = function(x, filter_cond, select_cond) {
  filter = mp("filter")(x, "filter", filter_cond, select_cond)
  replacement = mp_select(filter, "select", names(select_cond))
  mp_join(filter, replacement, filter.select = names(select_cond))
}

#' @export
mp_cartesian = function(x, y) {
  labelling_names = union(x$labelling_names, y$labelling_names)
  f = join_partitions(x$partition$frame(), y$partition$frame())
  Basis(f, labelling_names = labelling_names)
}

#' @export
mp_square = function(x, y_labelling_names) {
  nms = to_names(y_labelling_names)
  y = (x$partition$frame()
    |> setNames(nms)
    |> Basis(nms)
  )
  mp_cartesian(x, y)
}

#' @export
mp_triangle = function(x, y_labelling_names, exclude_diag = TRUE, lower_tri = FALSE) {
  f = x$partition$frame()
  g = setNames(f, y_labelling_names)
  n = nrow(f)
  if (exclude_diag) {
    k = 2:n
    i = sequence(k - 1)
    j = rep(k, k - 1)
  } else if (!exclude_diag) {
    k = seq_len(n)
    i = sequence(k)
    j = rep(k, k)
  }
  if (lower_tri) {
    ii = i
    i = j
    j = ii
  }
  f = cbind(
    f[i, , drop = FALSE],
    g[j, , drop = FALSE]
  )
  Basis(f, names(f))
}

#' @export
mp_symmetric = function(x, y_labelling_names, exclude_diag = TRUE) {
  f = x$partition$frame()
  g = setNames(f, y_labelling_names)
  n = nrow(f)
  k = seq_len(n)

  if (exclude_diag) {
    i = rep(    k , times = n - 1L)
    j = rep(rev(k), each  = n - 1L)
  } else {
    i = rep(k, times = n)
    j = rep(k, each  = n)
  }

  f = cbind(
    f[i, , drop = FALSE],
    g[j, , drop = FALSE]
  )
  Basis(f, names(f))
}

#' @export
mp_linear = function(x, y_labelling_names) {
  f = x$partition$frame()
  g = setNames(f, y_labelling_names)
  n = nrow(f)

  k = c(1L, rep(2L, n - 2L), 1L)
  i = rep(seq_len(n), k)
  j = sequence(k, c(2, seq_len(n - 2L), n - 1L), by = 2)

  f = cbind(
    f[i, , drop = FALSE],
    g[j, , drop = FALSE]
  )
  Basis(f, names(f))
}

#' @export
mp_subset = function(x, ...) {
  partition = mp_filter(x, "pick", ...)$partition
  Basis(partition, x$labelling_names)
}

#' @export
mp_union = function(...) {
  l = list(...)
  partitions = lapply(l, getElement, "partition")
  labelling_names = (l
    |> lapply(getElement, "labelling_names")
    |> unlist(recursive = FALSE, use.names = FALSE)
    |> unique()
  )
  Basis(do.call(union_vars, partitions)$frame(), labelling_names)
}

#' @export
mp_filter = function(x, subset_name, ...) {
  l = list(...)
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
  init_merge(p$frame(), subset_name)
}

#' @export
mp_filter_out = function(x, subset_name, ...) {
  l = list(...)
  p = x$partition
  for (cc in names(l)) {
    vals = l[[cc]]
    p = p$filter_out(vals, .wrt = cc)
  }
  init_merge(p$frame(), subset_name)
}

#' @export
mp_join = function(...) {
  args = list(...)
  is_basis = vapply(args, inherits, logical(1L), "Basis")
  is_merged = vapply(args, inherits, logical(1L), "Merged")
  table_list = args[is_basis | is_merged]
  by_list = args[!is_basis & !is_merged]
  z = table_list[[1L]]
  for (i in 2:length(table_list)) {
    args = c(
      list(x = z, y = table_list[[i]]),
      by_list
    )
    z = do.call(merge_generic_by_util, args)
  }
  z
}

#' @export
mp_group = function(group, group_nm, group_labelling_names, ...) {
  filter = list(...)

}

#' @export
mp_select = function(basis, grouping_dimension) {
  frame = basis$partition$select(to_names(grouping_dimension))$frame()
  nms = names(frame)[names(frame) %in% basis$labelling_names]
  frame |> Basis(labelling_names = nms)
}

#' @export
mp_indicator = function(x, ...) {
  l = list(...)
  for (nm in names(l)) {
    l[[nm]] = x$partition$partial_labels(nm) %in% l[[nm]]
  }
  Reduce(`&`, l)
}

#' @export
mp_indices = function(x, table) {
  match(x, table) - 1L
}

#' @export
mp_labels = function(x, labelling_names) {
  UseMethod("mp_labels")
}

#' @export
mp_labels.Basis = function(x, labelling_names) {
  if (missing(labelling_names)) return(x$labels())
  x$partial_labels(labelling_names)
}

#' @export
mp_labels.Merged = function(x, labelling_names) {
  x$labels_for[[labelling_names]]()
}

split_by = function(by, table_origins) UseMethod("split_by")

#' @export
split_by.character = function(by, table_origins) {
  by = to_names(by)
  orig = to_names(table_origins)
  list(by, by) |> setNames(orig)
}

#' @export
split_by.formula = function(by, table_origins) {
  orig = to_names(table_origins)
  list(
    to_names(lhs(by)[[2L]]),
    to_names(rhs(by)[[2L]])
  ) |> setNames(orig)
}

## change to Mergable

#' @export
Merged = function(frame, column_map) {
  self = Base()
  self$frame = frame
  self$column_map = column_map
  self$labelling_names = function() {
    l = self$partition_list()
    for (nm in names(l)) {
      l[[nm]] = names(l[[nm]])
    }
    unlist(l, use.names = FALSE) |> unique()
  }
  self$labels_for = list()
  self$labels_frame = function() {
    l = list()
    for (d in names(self$column_map)) l[[d]] = self$labels_for[[d]]()
    l |> as.data.frame()
  }
  self$frame_by_dim = list()
  self$labels_for = list()
  self$partition_by_dim = list()
  for (d in names(self$column_map)) {
    getter = FrameGetter(self, d)
    self$frame_by_dim[[d]] = getter$get_frame
    self$labels_for[[d]] = getter$get_labels
    self$partition_by_dim[[d]] = getter$get_partition
  }
  self$column_by_dim = list()
  for (d in names(self$column_map)) {
    self$column_by_dim[[d]] = list()
    for (c in names(self$column_map[[d]])) {
      self$column_by_dim[[d]][[c]] = ColumnGetter(self, d, c)
    }
  }
  self$partition_list = function() {
    l = list()
    for (dn in names(self$partition_by_dim)) {
      l[[dn]] = self$partition_by_dim[[dn]]()
    }
    l
  }
  self$frame_list = function() method_apply(self$partition_list(), "frame")
  self$filter = function(condition) {
    condition = substitute(condition)
    i = eval(condition, envir = c(self$column_by_dim, self$frame))
    Merged(self$frame[i,,drop = FALSE]
      , column_map = self$column_map
    )
  }
  self$partition = self$partition_by_dim[[1L]]()  ## hack! should probably have a method and then change the partition field in Basis to a method as well
  return_object(self, "Merged")
}

apply_col_map = function(map, orig_table_nm, by) {
  map[[orig_table_nm]][by] |> unlist(use.names = FALSE)
}

filter_by_list = function(x_orig, y_orig, by_list) {
  l = list()
  nms = names(by_list)
  for (i in seq_along(by_list)) {
    nm_i = nms[[i]]
    nms_i = to_names(nm_i)
    if ((nms_i[[1L]] %in% x_orig) & (nms_i[[2L]] %in% y_orig)) {
      l[[nm_i]] = by_list[[nm_i]]
    }
  }
  l
}


merge_generic_by_util = function(x, y, ...) {
  by = filter_by_list(
    names(x$column_map),
    names(y$column_map),
    list(...)
  )
  by = mapply(split_by
    , by
    , names(by)
    , SIMPLIFY = FALSE
    , USE.NAMES = FALSE
  ) |> setNames(names(by))
  by.x = character()
  by.y = character()
  for (nm in names(by)) {
    orig = names(by[[nm]])
    by.x = append(
      by.x,
      apply_col_map(x$column_map, orig[[1L]], by[[nm]][[1L]])
    )
    by.y = append(
      by.y,
      apply_col_map(y$column_map, orig[[2L]], by[[nm]][[2L]])
    )
  }
  by = data.frame(x = by.x, y = by.y) |> unique()
  merge_util(x, y, by$x, by$y)
}

#' @export
summary.Merged = function(object, ...) {
  formats = c("name", "combined")
  structure(
    sapply(formats, merged_format_picker, x = object, simplify = FALSE, USE.NAMES = TRUE),
    class = "summary.Merged"
  )
}

#' @export
print.summary.Merged = function(x, ...) {
  msg_hline() |> message()
  msg(
    "Merged object from macpan2 describing",
    "an aspect of model shape"
  ) |> message()
  msg_hline() |> message()
  print(x$name)
  print(x$combined, row.names = FALSE)
}

#' @export
names.Merged = function(x) to_names(x$labelling_names())

merged_format_picker = function(x
    , format = c("labels", "merged", "combined", "separate", "name", "names")
  ) {
  switch (match.arg(format)
    , labels = x$labels_frame()
    , merged = x$frame
    , combined = cbind(x$labels_frame(), x$frame)
    , separate = x$frame_list()
    , name = to_name(x$labelling_names())
    , names = to_names(x$labelling_names())
  )
}

#' @export
print.Merged = function(x
    , format = c("labels", "merged", "combined", "separate", "name", "names")
    , ...
  ) {
  x = merged_format_picker(x, format)
  print(x, row.names = FALSE, ...)
}

#' @export
head.Merged = function(x
    , n = 6L
    , format = c("labels", "merged", "combined", "separate", "name", "names")
    , ...
  ) {
  x = merged_format_picker(x, format)
  if (format == "separate") {
    return(lapply(x, head, n = n, ...))
  } else {
    return(head(x, n = n, ...))
  }
}

#' @export
tail.Merged = function(x
    , n = 6L
    , format = c("labels", "merged", "combined", "separate", "name", "names")
    , ...
  ) {
  x = merged_format_picker(x, format)
  if (format == "separate") {
    return(lapply(x, tail, n = n, ...))
  } else {
    return(tail(x, n = n, ...))
  }
}

#' @export
str.Merged = function(x
    , format = c("labels", "merged", "combined", "separate", "name", "names")
    , ...
) {
  x = merged_format_picker(x, format)
  str(x, ...)
}

CompartmentalPartition = function(frame
    , special_partitions = c(vec = "Vec", type = "Type")
  ) {
  special_partitions = c(vec, type)
  if (!all(special_partitions %in% names(frame))) {
    msg_break(
      msg_colon(
        msg_csv(
          "The vec or type arguments",
          special_partitions,
          "are not in the names of the frame"),
        msg_indent(names(frame))
      )
    )
  }
  self = Base()
  self$partition = Partition(frame)
  self$vec = "Vec"
  self$state = function() vec(self, "state", self$vec)
  self$flow = function() vec(self, "flow", self$vec)
  return_object(self, "CompartmentalPartition")
}

#' Read Partition
#'
#' Read a CSV file in as a \code{\link{Partition}}.
#'
#' @param ... File path components to pass to \code{\link{CSVReader}}, and
#' subsequently to \code{\link{file.path}}.
#'
#' @export
read_partition = function(...) read_frame(...) |> Partition()

#' @export
read_frame = function(...) CSVReader(...)$read()

#' @export
partition = function(...) data.frame(...) |> Partition()

#' @export
atomic_partition = function(state_variables, flow_rates, partition_name) {
  l = list(
    c(state_variables, flow_rates),
    c(
      rep("state", length(state_variables)),
      rep("flow", length(flow_rates))
    )
  )
  (l
    |> setNames(c(partition_name, "Vec"))
    |> as.data.frame()
    |> CompartmentalPartition()
  )
}

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
  anchor = macpan2:::empty_frame(...)
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

renew_part = function(x, vec_parts) {
  if (length(vec_parts) == 1L) x = CompartmentalPartition(x, vec_parts)
  x
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

by_ = function(by) UseMethod("by_")
by_.character = function(by) {
  if (identical(nchar(by), 0L) | isTRUE(is.na(by))) return(list())
  by = process_by_char(by)
  list(x = by, y = by)
}
by_.formula = function(by) {
  if (is_one_sided(by)) return(by_(rhs_char(by)))
  list(
    x = process_by_char(lhs_char(by)),
    y = process_by_char(rhs_char(by))
  )
}
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
