#' Link
#'
#' Make an object to describe links between the entities in an
#' \code{\link{Index}}. \code{Link} object are created by operating on existing
#' \code{\link{Index}} objects. For example, here the \code{\link{mp_join}}
#' combines two
#' ```{r}
#' age = mp_index(Age = c("young", "old"))
#' state = mp_cartesian(
#'   mp_index(Epi = c("S", "I", "R")),
#'   age
#' )
#' mp_join(
#'   mp_choose(state, "from", Epi = "S"),
#'   mp_choose(state, "to",   Epi = "I"),
#'   from.to = "Age"
#' )
#' ```
#'
#' @export
Link = function(frame, column_map, reference_index_list, labelling_column_names_list) {
  self = Base()
  self$frame = frame
  self$column_map = column_map
  self$reference_index_list = reference_index_list
  self$labelling_column_names_list = labelling_column_names_list
  # function() {
  #   lapply(self$reference_index_list, getElement, "labelling_column_names")
  # }
  self$table_names = function() names(self$column_map)
  self$labels_for = list()
  self$labels_frame = function() {
    l = list()
    for (d in names(self$column_map)) l[[d]] = self$labels_for[[d]]()
    l |> as.data.frame()
  }
  self$frame_for = list()
  self$partition_for = list()
  self$index_for = list()
  self$labels_for = list()
  self$reference_labels_for = list()
  self$positions_for = list()
  self$reference_positions_for = list()
  for (d in names(self$column_map)) {
    getter = FrameGetter(self, d)
    self$frame_for[[d]] = getter$get_frame
    self$labels_for[[d]] = getter$get_labels
    self$partition_for[[d]] = getter$get_partition
    self$index_for[[d]] = getter$get_index
    self$reference_labels_for[[d]] = getter$get_reference_labels
    self$positions_for[[d]] = getter$get_positions
    self$reference_positions_for[[d]] = getter$get_reference_positions
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
    for (dn in names(self$partition_for)) {
      l[[dn]] = self$partition_for[[dn]]()
    }
    l
  }
  self$frame_list = function() method_apply(self$partition_list(), "frame")
  self$filter = function(condition) {
    condition = substitute(condition)
    i = eval(condition, envir = c(self$column_by_dim, self$frame))
    Link(self$frame[i, , drop = FALSE]
      , column_map = self$column_map
      , reference_index_list = self$reference_index_list
      , labelling_column_names_list = self$labelling_column_names_list
    )
  }
  self$expr = function(condition) {
    substitute(condition)
    eval(condition, envir = c(self$column_by_dim, self$frame))
  }
  self$partition = self$partition_for[[1L]]()  ## hack! should probably have a method and then change the partition field in Index to a method as well
  return_object(self, "Link")
}

ColumnGetter = function(link, dimension_name, column_name) {
  self = Base()
  self$link = link
  self$dimension_name = dimension_name
  self$column_name = column_name
  self$get = function() {
    lp = self$link
    i = lp$column_map[[self$dimension_name]][[self$column_name]]
    lp$frame[[i]]
  }
  self = return_object(self, "ColumnGetter")
  self$get
}

FrameGetter = function(link, dimension_name) {
  self = Base()
  self$link = link
  self$dimension_name = dimension_name
  self$get_frame = function() {
    i = unlist(
      self$link$column_map[[self$dimension_name]],
      use.names = FALSE
    )
    setNames(
      self$link$frame[, i, drop = FALSE],
      names(self$link$column_map[[self$dimension_name]])
    )
  }
  self$get_partition = function() self$get_frame() |> Partition()
  self$get_index = function() {
    Index(
      self$get_partition(),
      self$link$labelling_column_names_list[[self$dimension_name]],
      self$link$reference_index_list[[self$dimension_name]]
    )
  }
  self$get_labels = function() {
    i = self$link$labelling_column_names_list[[self$dimension_name]]
    f = self$get_frame()[, i, drop = FALSE]
    l = as.list(f)
    paste_args = c(l, sep = ".")
    do.call(paste, paste_args)
  }
  self$get_reference_labels = function() {
    self$get_index()$reference_labels()
  }
  self$get_positions = function(zero_based = FALSE) {
    i = match(self$get_labels(), self$get_reference_labels())
    if (zero_based) i = i - 1L
    i
  }
  self$get_reference_positions = function(zero_based = FALSE) {
    i = match(self$get_reference_labels(), self$get_labels())
    if (zero_based) i = i - 1L
    i
  }
  return_object(self, "FrameGetter")
}

initial_column_map = function(column_names, dimension_name) {
  setNames(
    list(setNames(as.list(column_names), column_names)),
    dimension_name
  )
}

initial_reference_index_list = function(index, dimension_name) {
  setNames(list(index), dimension_name)
}


initial_labelling_column_names_list = function(labelling_column_names, dimension_name) {
  setNames(
    list(labelling_column_names),
    dimension_name
  )
}

## take two Merge objects and merge their frames
## and update the provenance-preserving column maps
merge_util = function(x, y, by.x, by.y, table_names_order) {

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
    suffixes = suffixes,
    sort = FALSE
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
  z_reference_index_list = c(x$reference_index_list, y$reference_index_list)
  z_lab_names_list = c(x$labelling_column_names_list, y$labelling_column_names_list)

  ## ----
  ## wrap up the result with provenance-preserving column map
  ## ----
  Link(
    z,
    z_column_map[table_names_order],
    z_reference_index_list[table_names_order],
    z_lab_names_list[table_names_order]
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
init_merge = function(frame, dimension_name, reference_index, labelling_column_names) {
  Link(frame
    , initial_column_map(names(frame), dimension_name)
    , initial_reference_index_list(reference_index, dimension_name)
    , initial_labelling_column_names_list(labelling_column_names, dimension_name)
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

## @param x Link object
## @param col_nm Name of a column to check for implicit provenance
is_provenance_implicit = function(x, col_nm) {
  (x$column_map
   |> lapply(getElement, col_nm)
   |> Filter(f = Negate(is.null))
   |> vapply(`==`, logical(1L), col_nm)
  )
}

## @param x Link object
explicit_provenance = function(x, col_nm) {
  m = x$column_map
  implicit = is_provenance_implicit(x, col_nm)
  if (length(implicit) == 0L) {
    msg_colon(
      msg("Column", col_nm, "not found in any of the original tables"),
      msg_indent(names(m))
    ) |> stop()
  }
  if (!any(implicit)) return(x)

  tabs_to_fix = (implicit
    |> Filter(f = isTRUE)
    |> names()
  )

  f = x$frame
  l = x$labelling_column_names_list
  ii = x$reference_index_list

  orig_col = f[[col_nm]]
  for (tab_nm in tabs_to_fix) {
    new_col_nm = sprintf("%s:%s", col_nm, tab_nm)
    f[[new_col_nm]] = orig_col
    m[[tab_nm]][[col_nm]] = new_col_nm
  }
  f[[col_nm]] = NULL
  ## TODO: update with four-arg form of Link
  ## frame, column_map, reference_index_list, labelling_column_names_list
  Link(f, m, ii, l)
}


merge_generic_by_util = function(x, y, table_names_order, ...) {
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

  dup.x = duplicated(by$x)
  dup.y = duplicated(by$y)
  if (any(dup.x) | any(dup.y)) {
    if (any(dup.x)) {
      cols_to_fix = by$x[dup.x]
      for (col_nm in cols_to_fix) {
        x = explicit_provenance(x, col_nm)
      }
    }
    if (any(dup.y)) {
      cols_to_fix = by$y[dup.y]
      for (col_nm in cols_to_fix) {
        y = explicit_provenance(y, col_nm)
      }
    }
    merge_generic_by_util(x, y, ...)
  } else {
    merge_util(x, y, by$x, by$y, table_names_order)
  }
}

#' @export
as.data.frame.Link = function(x, row.names = NULL, optional = FALSE, ...) {
  x$labels_frame()
}

#' @export
summary.Link = function(object, ...) {
  formats = c("name", "combined")
  structure(
    sapply(formats, link_format_picker, x = object, simplify = FALSE, USE.NAMES = TRUE),
    class = "summary.Link"
  )
}

#' @export
print.summary.Link = function(x, ...) {
  msg_hline() |> message()
  msg(
    "Link object from macpan2 describing",
    "an aspect of model shape"
  ) |> message()
  msg_hline() |> message()
  print(x$name)
  print(x$combined, row.names = FALSE)
}

#' @export
names.Link = function(x) names(x$frame)

#' @export
labelling_column_names.Link = function(x) x$labelling_column_names_list


link_format_picker = function(x
    , format = c("labels", "link", "combined", "separate")
  ) {
  switch(match.arg(format)
    , labels = x$labels_frame()
    , link = x$frame
    , combined = cbind(x$labels_frame(), x$frame)
    , separate = x$frame_list()
  )
}

#' @export
print.Link = function(x
    , format = c("labels", "link", "combined", "separate")
    , ...
  ) {
  x = link_format_picker(x, format)
  print(x, row.names = FALSE, ...)
}

#' @export
head.Link = function(x
    , n = 6L
    , format = c("labels", "link", "combined", "separate")
    , ...
  ) {
  x = link_format_picker(x, format)
  if (format == "separate") {
    return(lapply(x, head, n = n, ...))
  } else {
    return(head(x, n = n, ...))
  }
}

#' @export
tail.Link = function(x
    , n = 6L
    , format = c("labels", "link", "combined", "separate")
    , ...
  ) {
  x = link_format_picker(x, format)
  if (format == "separate") {
    return(lapply(x, tail, n = n, ...))
  } else {
    return(tail(x, n = n, ...))
  }
}

#' @export
str.Link = function(x
    , format = c("labels", "link", "combined", "separate")
    , ...
) {
  x = link_format_picker(x, format)
  str(x, ...)
}


#' @export
LinkList = function() {
  self = Base()
  self$list = list() |> setNames(as.character())
  self$add = function(new_link, ...) {
      new_links = list(...)
    if (missing(new_link)) {
      macpan2:::valid$named_list$check(new_links)
    } else if(length(new_links) == 0L) {
      new_nm = deparse1(substitute(new_link))
      new_links = setNames(list(new_link), new_nm)
    } else {
      stop("If supplying more than one join result, please name them with {name} = {join_result}")
    }

    for (nm in names(new_links)) {
      if (nm %in% names(self$list)) {
        msg(
          "Join result", nm, "is already in the list.",
          "Overwriting the existing one."
        ) |> message()
      }
      self$list[[nm]] = new_links[[nm]]
    }
  }
  return_object(self, "LinkList")
}
