#' Join Indexes
#'
#' Join two or more index tables (see \code{\link{mp_index}}) to produce a
#' ledger (see \code{\link{LedgerDefinition}}).
#'
#' When two index tables are passed to `...`, `mp_join` behaves very much like
#' an ordinary [inner join](https://en.wikipedia.org/wiki/Join_(SQL)).
#' When more than two tables are passed to `...`, `mp_join` iteratively joins
#' pairs of tables to produce a final ledger. For example, if index tables `A`
#' `B`, and `C` are passed to `mp_join`, an inner join of `A` and `B` is
#' performed and the result is joined with `C`. In each of these successive
#' internal joins. The properties of inner
#' joins ensures that the order of tables does not affect the set of rows in
#' the final table (SW states without proof!).
#'
#' When two index tables are passed to `...`, the `by` argument is just a
#' character vector of column names on which to join (as in standard R functions
#' for joining data frames), or the dot-concatenation of these column names.
#' For example,
#' ```{r join, echo = TRUE, eval = TRUE}
#' state = mp_index(
#'   Epi = c("S", "I", "S", "I"),
#'   Age = c("young", "young", "old", "old")
#' )
#' mp_join(
#'   from = mp_subset(state, Epi = "S"),
#'   to = mp_subset(state, Epi = "I"),
#'   by = "Age"
#' )
#' ```
#' If there are more than two tables then the `by` argument must be a named
#' list of character vectors, each describing how to join the columns of
#' a pair of tables in `...`. The names of this list are dot-concatenations
#' of the names of pairs of tables in `...`. For example,
#' ```{r rates, echo = TRUE, eval = TRUE}
#' rates = mp_index(
#'   Epi = c("lambda", "lambda"),
#'   Age = c("young", "old")
#' )
#' mp_join(
#'   from = mp_subset(state, Epi = "S"),
#'   to = mp_subset(state, Epi = "I"),
#'   rate = mp_subset(rates, Epi = "lambda"),
#'   by = list(
#'     from.to = "Age",
#'     from.rate = "Age"
#'   )
#' )
#' ```
#' If the `by` columns have different names in two tables, then you can
#' specify these using formula notation where the left-hand-side
#' is a dot-concatenation of columns in the first table and the
#' right-hand-side is a dot-concatenation of the columns in the second
#' table. For example,
#' ```{r contact_join}
#' contact = mp_index(
#'   AgeSusceptible = c("young", "young", "old", "old"),
#'   AgeInfectious = c("young", "old", "young", "old")
#' )
#' mp_join(
#'   sus = mp_subset(state, Epi = "S"),
#'   inf = mp_subset(state, Epi = "I"),
#'   con = contact,
#'   by = list(
#'     sus.con = "Age" ~ "AgeSusceptible",
#'     inf.con = "Age" ~ "AgeInfectious"
#'   )
#' )
#' ```
#'
#' @param ... Named arguments giving indexes created by
#' \code{\link{mp_index}} or another function that manipulates indexes.
#' Each argument will become a position vector used to subset
#' or expand numeric vectors in archetype formulas.
#' @param by What columns to use to join the indexes. See below on
#' how to specify this argument.
#'
#' @family ledgers
#' @export
mp_join = function(..., by = empty_named_list()) {
  table_list = valid$named_list$assert(list(...))
  table_nms = names(table_list)

  if (length(table_nms) < 2L) stop("cannot join fewer than two index objects.")
  if (is.character(by)) {
    # if (length(table_nms) != 2L) {
    #   stop("joining more than one index requires a list-valued by argument.")
    # }
    table_pairs = to_name_pairs(table_nms)
    by = rep(list(by), length(table_pairs)) |> setNames(table_pairs)
  }

  by_list = valid$named_list$assert(by)
  if (length(by_list) > 1L) {
    table_order = (by_list
      |> names()
      |> lapply(to_names)
      |> unlist()
      |> unique()
      |> union(names(table_list))
    )
  } else {
    table_order = names(table_list)
  }
  ordered_table_list = table_list[table_order]

  by_nms = names(by_list) |> strsplit(".", fixed = TRUE)
  good_by_nms = (by_nms
    |> lapply(`%in%`, table_nms)
    |> vapply(all, logical(1L))
  )
  if (any(!good_by_nms)) {
    msg_break(
      msg_colon(
        "Indices to join were given the following names for the join output",
        table_nms
      ),
      msg_colon(
        msg(
          "But the names of arguments that specify what columns will be",
          "The following arguments were supplied to",
          "determine what columns to join on"
        ),
        msg_indent_break(by_nms[!good_by_nms])
      ),
    ) |> stop()
  }
  if (!is.null(table_nms)) {
    for (nm in names(ordered_table_list)) {
      if (nm != "" & inherits(ordered_table_list[[nm]], "Index")) {
        ordered_table_list[[nm]] = mp_choose(ordered_table_list[[nm]], nm)
      }
    }
  }
  orig_tab_nms = (ordered_table_list
    |> method_apply("table_names")
    |> unname()
    |> unlist(recursive = FALSE)
  )
  table_nm_diffs = (by_nms
    |> lapply(factor, levels = orig_tab_nms)
    |> lapply(as.integer)
    |> vapply(diff, integer(1L))
  )

  bad_by_args = table_nm_diffs < 1L ## table names in the wrong order
  if (any(bad_by_args)) {
    fixed_by_nms = (by_nms[bad_by_args]
      |> lapply(rev)
      |> lapply(paste0, collapse = ".")
    )
    fixed_by_args = (by_list[bad_by_args]
      |> lapply(swap_sides)
    )
    by_list[bad_by_args] = fixed_by_args
    names(by_list)[bad_by_args] = fixed_by_nms
  }

  z = ordered_table_list[[1L]]
  for (i in 2:length(ordered_table_list)) {
    args = c(
      list(
        x = z,
        y = ordered_table_list[[i]]
      ),
      by_list
    )
    z = do.call(merge_generic_by_util, args)
  }
  z$reorder(names(table_list))
}

#' Ledgers
#'
#' A ledger is a table with rows that identify specific instances of a
#' functional form used to define a \code{\link{mp_dynamic_model}}. Ledgers
#' are most commonly created using the \code{\link{mp_join}} function as in the 
#' following example.
#' ```{r ledger_join}
#' age = mp_index(Age = c("young", "old"))
#' state = mp_cartesian(
#'   mp_index(Epi = c("S", "I", "R")),
#'   age
#' )
#' mp_join(
#'   from = mp_subset(state, Epi = "S"),
#'   to = mp_subset(state, Epi = "I"),
#'   by = list(from.to = "Age")
#' )
#' ```
#' @name LedgerDefinition
NULL


# @export
#mp_flow_rate = function(from, to, rate, )


Ledger = function(frame, column_map, reference_index_list, labelling_column_names_list) {
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
    Ledger(self$frame[i, , drop = FALSE]
      , column_map = self$column_map
      , reference_index_list = self$reference_index_list
      , labelling_column_names_list = self$labelling_column_names_list
    )
  }
  self$expr = function(condition) {
    substitute(condition)
    eval(condition, envir = c(self$column_by_dim, self$frame))
  }
  self$reorder = function(table_names_order) {
    ## NB: in-place!
    self$labelling_column_names_list = self$labelling_column_names_list[table_names_order]
    self$column_map = self$column_map[table_names_order]
    self$reference_index_list = self$reference_index_list[table_names_order]
    self
  }

  ## hack! should probably have a method and then change the partition
  ## field in Index to a method as well
  self$partition = self$partition_for[[1L]]()
  return_object(self, "Ledger")
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
      labelling_column_names = self$link$labelling_column_names_list[[self$dimension_name]],
      reference_index = self$link$reference_index_list[[self$dimension_name]]
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
    suffixes = suffixes,
    sort = FALSE
  )

  if (nrow(z) == 0L) stop("Nothing matched in a join resulting in a ledger with zero rows, which is not allowed")

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
  Ledger(
    z,
    z_column_map,
    z_reference_index_list,
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

init_merge = function(frame, dimension_name, reference_index, labelling_column_names) {
  Ledger(frame
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

## @param x Ledger object
## @param col_nm Name of a column to check for implicit provenance
is_provenance_implicit = function(x, col_nm) {
  (x$column_map
   |> lapply(getElement, col_nm)
   |> Filter(f = Negate(is.null))
   |> vapply(`==`, logical(1L), col_nm)
  )
}

## @param x Ledger object
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

  ## frame, column_map, reference_index_list, labelling_column_names_list
  Ledger(f, m, ii, l)
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
    merge_util(x, y, by$x, by$y)
  }
}

#' @export
as.data.frame.Ledger = function(x, row.names = NULL, optional = FALSE, ...) {
  x$labels_frame()
}

#' @export
summary.Ledger = function(object, ...) {
  formats = c("name", "combined")
  structure(
    sapply(formats, link_format_picker, x = object, simplify = FALSE, USE.NAMES = TRUE),
    class = "summary.Ledger"
  )
}

#' @export
print.summary.Ledger = function(x, ...) {
  msg_hline() |> message()
  msg(
    "Ledger object from macpan2 describing",
    "an aspect of model shape"
  ) |> message()
  msg_hline() |> message()
  print(x$name)
  print(x$combined, row.names = FALSE)
}

#' @export
names.Ledger = function(x) names(x$frame)

#' @export
labelling_column_names.Ledger = function(x) x$labelling_column_names_list


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
print.Ledger = function(x
    , format = c("labels", "link", "combined", "separate")
    , ...
  ) {
  x = link_format_picker(x, format)
  print(x, row.names = FALSE, ...)
}

#' @importFrom utils head
#' @export
head.Ledger = function(x
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

#' @importFrom utils tail
#' @export
tail.Ledger = function(x
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

#' @importFrom utils str
#' @export
str.Ledger = function(object
    , format = c("labels", "link", "combined", "separate")
    , ...
) {
  x = link_format_picker(object, format)
  str(x, ...)
}

## not used??
LedgerList = function() {
  self = Base()
  self$list = list() |> setNames(as.character())
  self$add = function(new_link, ...) {
      new_ledgers = list(...)
    if (missing(new_link)) {
      valid$named_list$check(new_ledgers)
    } else if(length(new_ledgers) == 0L) {
      new_nm = deparse1(substitute(new_link))
      new_ledgers = setNames(list(new_link), new_nm)
    } else {
      stop("If supplying more than one join result, please name them with {name} = {join_result}")
    }

    for (nm in names(new_ledgers)) {
      if (nm %in% names(self$list)) {
        msg(
          "Join result", nm, "is already in the list.",
          "Overwriting the existing one."
        ) |> message()
      }
      self$list[[nm]] = new_ledgers[[nm]]
    }
  }
  return_object(self, "LedgerList")
}
