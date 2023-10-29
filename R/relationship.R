#' Relationship
#'
#' @export
Relationship = function(frame, column_map, labelling_names_list) {
  self = Base()
  self$frame = frame
  self$column_map = column_map
  self$labelling_names_list = labelling_names_list
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
    Relationship(self$frame[i, , drop = FALSE]
      , column_map = self$column_map
      , labelling_names_list = self$labelling_names_list
    )
  }
  self$partition = self$partition_by_dim[[1L]]()  ## hack! should probably have a method and then change the partition field in Descriptors to a method as well
  return_object(self, "Relationship")
}

ColumnGetter = function(relationship, dimension_name, column_name) {
  self = Base()
  self$relationship = relationship
  self$dimension_name = dimension_name
  self$column_name = column_name
  self$get = function() {
    lp = self$relationship
    i = lp$column_map[[self$dimension_name]][[self$column_name]]
    lp$frame[[i]]
  }
  self = return_object(self, "ColumnGetter")
  self$get
}

FrameGetter = function(relationship, dimension_name) {
  self = Base()
  self$relationship = relationship
  self$dimension_name = dimension_name
  self$get_frame = function() {
    i = unlist(
      self$relationship$column_map[[self$dimension_name]],
      use.names = FALSE
    )
    setNames(
      self$relationship$frame[, i, drop = FALSE],
      names(self$relationship$column_map[[self$dimension_name]])
    )
  }
  self$get_partition = function() self$get_frame() |> Partition()
  self$get_labels = function() {
    i = self$relationship$labelling_names_list[[self$dimension_name]]
    f = self$get_frame()[, i, drop = FALSE]
    l = as.list(f)
    paste_args = c(l, sep = ".")
    do.call(paste, paste_args)
  }
  return_object(self, "FrameGetter")
}

initial_column_map = function(column_names, dimension_name) {
  setNames(
    list(setNames(as.list(column_names), column_names)),
    dimension_name
  )
}

initial_labelling_names_list = function(labelling_names, dimension_name) {
  setNames(
    list(labelling_names),
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
  z_lab_names_list = c(x$labelling_names_list, y$labelling_names_list)

  ## ----
  ## wrap up the result with provenance-preserving column map
  ## ----
  Relationship(
    z,
    z_column_map,
    z_lab_names_list
  )
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

#' @export
init_merge = function(frame, dimension_name, labelling_names) {
  Relationship(frame
    , initial_column_map(names(frame), dimension_name)
    , initial_labelling_names_list(labelling_names, dimension_name)
  )
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




apply_col_map = function(map, orig_table_nm, by) {
  map[[orig_table_nm]][by] |> unlist(use.names = FALSE)
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
summary.Relationship = function(object, ...) {
  formats = c("name", "combined")
  structure(
    sapply(formats, relationship_format_picker, x = object, simplify = FALSE, USE.NAMES = TRUE),
    class = "summary.Relationship"
  )
}

#' @export
print.summary.Relationship = function(x, ...) {
  msg_hline() |> message()
  msg(
    "Relationship object from macpan2 describing",
    "an aspect of model shape"
  ) |> message()
  msg_hline() |> message()
  print(x$name)
  print(x$combined, row.names = FALSE)
}

#names.Relationship = function(x) to_names(x$labelling_names())

relationship_format_picker = function(x
    , format = c("labels", "relationship", "combined", "separate")
  ) {
  switch (match.arg(format)
    , labels = x$labels_frame()
    , relationship = x$frame
    , combined = cbind(x$labels_frame(), x$frame)
    , separate = x$frame_list()
  )
}

#' @export
print.Relationship = function(x
    , format = c("labels", "relationship", "combined", "separate")
    , ...
  ) {
  x = relationship_format_picker(x, format)
  print(x, row.names = FALSE, ...)
}

#' @export
head.Relationship = function(x
    , n = 6L
    , format = c("labels", "relationship", "combined", "separate")
    , ...
  ) {
  x = relationship_format_picker(x, format)
  if (format == "separate") {
    return(lapply(x, head, n = n, ...))
  } else {
    return(head(x, n = n, ...))
  }
}

#' @export
tail.Relationship = function(x
    , n = 6L
    , format = c("labels", "relationship", "combined", "separate")
    , ...
  ) {
  x = relationship_format_picker(x, format)
  if (format == "separate") {
    return(lapply(x, tail, n = n, ...))
  } else {
    return(tail(x, n = n, ...))
  }
}

#' @export
str.Relationship = function(x
    , format = c("labels", "relationship", "combined", "separate")
    , ...
) {
  x = relationship_format_picker(x, format)
  str(x, ...)
}
