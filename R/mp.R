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


#' Cartesian Product of Index Tables
#'
#' Produce a new index table by taking all possible pairwise combinations
#' of the input tables. This is useful for producing product models
#' that expand model components through stratification.
#'
#' @param ... Index tables (see \code{\link{mp_index}}).
#'
#' @examples
#' mp_cartesian(
#'   mp_index(Epi = c("S", "I")),
#'   mp_index(Age = c("young", "old"))
#' )
#'
#' si = mp_index(Epi = c("S", "I"))
#' age = mp_index(Age = c("young", "old"))
#' loc = mp_index(City = c("hamilton", "toronto"))
#' vax = mp_index(Vax = c("unvax", "vax"))
#' (si
#'   |> mp_cartesian(age)
#'   |> mp_cartesian(loc)
#'   |> mp_cartesian(vax)
#' )
#'
#' flow_rates = mp_index(Epi = c("infection", "recovery"))
#' mp_union(
#'   mp_cartesian(
#'     mp_subset(flow_rates, Epi = "infection"),
#'     age
#'   ),
#'   mp_subset(flow_rates, Epi = "recovery")
#' )
#'
#' @family indexes
#' @family products
#' @export
mp_cartesian = function(...) Reduce(mp_cartesian_binary, list(...))

mp_cartesian_binary = function(x, y) {
  shared_columns = intersect(names(x), names(y))
  if (length(shared_columns) != 0) {
    msg_break(
      msg_colon(
        msg(
          "Cannot take the Cartesian product of two indexes that",
          "share columns names. But the input indexes share the",
          "following columns"
        ),
        msg_indent(shared_columns)
      ),
      msg("Perhaps mp_join is more suitable?")
    ) |> stop()
  }
  labelling_column_names = union(x$labelling_column_names, y$labelling_column_names)
  f = join_partitions(x$partition$frame(), y$partition$frame())
  Index(f, labelling_column_names = labelling_column_names)
}

#' Self Cartesian Product
#'
#' @param suffixes Length-2 character vector giving suffixes that
#' disambiguate the column names in the output.
#' @inheritParams cartesian
#' @family products
#' @export
mp_square = function(x, suffixes = c("A", "B")) {
  l1 = sprintf("%s%s", x$labelling_column_names, suffixes[1L])
  l2 = sprintf("%s%s", x$labelling_column_names, suffixes[2L])
  n1 = sprintf("%s%s", names(x), suffixes[1L])
  n2 = sprintf("%s%s", names(x), suffixes[2L])
  x = (x$partition$frame()
    |> setNames(n1)
    |> Index(labelling_column_names = l1)
  )
  y = (x$partition$frame()
    |> setNames(n2)
    |> Index(labelling_column_names = l2)
  )
  mp_cartesian(x, y)
}

#' Self Cartesian Product Excluding One Off-Diagonal Side
#'
#' @inheritParams cartesian
#' @param y_labelling_column_names TODO
#' @param exclude_diag Should 'diagonal' commponents be excluded from the output.
#' @param lower_tri Should the lower triangular components be include from the
#' output. If \code{FALSE} the result is upper triangular.
#'
#' @family products
#' @export
mp_triangle = function(x, y_labelling_column_names, exclude_diag = TRUE, lower_tri = FALSE) {
  f = x$partition$frame()
  g = setNames(f, y_labelling_column_names)
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
  Index(f, labelling_column_names = names(f))
}

#' Symmetric Self Cartesian Product
#'
#' @inheritParams mp_triangle
#' @family products
#' @export
mp_symmetric = function(x, y_labelling_column_names, exclude_diag = TRUE) {
  f = x$partition$frame()
  g = setNames(f, y_labelling_column_names)
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
  Index(f, labelling_column_names = names(f))
}

#' Linear Chain Product
#'
#' TODO: what does this mean?
#'
#' @inheritParams mp_square
#' @family products
#' @export
mp_linear = function(x, y_labelling_column_names) {
  f = x$partition$frame()
  g = setNames(f, y_labelling_column_names)
  n = nrow(f)

  k = c(1L, rep(2L, n - 2L), 1L)
  i = rep(seq_len(n), k)
  j = sequence(k, c(2, seq_len(n - 2L), n - 1L), by = 2)

  f = cbind(
    f[i, , drop = FALSE],
    g[j, , drop = FALSE]
  )
  Index(f, labelling_column_names = names(f))
}

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

#' Aggregate an Index
#'
#' Create a one-column ledger (see \code{\link{LedgerDefinition}}) with rows
#' identifying instances of an aggregation.
#'
#' @family ledgers
#' @export
mp_aggregate = function(index, by = "Group", ledger_column = "group") {
  index_columns = to_names(by)
  if (length(index_columns) == 1L & !index_columns %in% names(index)) {
    partition = index$partition$constant(by, "a")
  } else {
    partition = index$partition
  }
  index = Index(partition) |> mp_group(by)

  Ledger(
    partition$frame(),
    initial_column_map(names(partition), ledger_column),
    initial_reference_index_list(index, ledger_column),
    setNames(list(index_columns), ledger_column)
  )
}

#' Union of Indexes
#'
#' @param ... Indexes.
#'
#' @family indexes
#' @export
mp_union = function(...) UseMethod("mp_union")

#' @export
mp_union.Index = function(...) {
  l = list(...)
  partitions = lapply(l, getElement, "partition")
  labelling_column_names = (l
    |> lapply(getElement, "labelling_column_names")
    |> unlist(recursive = FALSE, use.names = FALSE)
    |> unique()
  )
  Index(do.call(union_vars, partitions)$frame(), labelling_column_names = labelling_column_names)
}

## not used anymore?
#' @export
mp_union.Ledger = function(...) {
  l = list(...)
  column_map = lapply(l, getElement, "column_map") |> unique()
  if (length(column_map) != 1L) {
    msg_colon(
      msg(
        "Union of inconsistent Ledger objects.",
        "All Ledger objects must have the same",
        "column_map, but the following distinct",
        "maps were found:"
      ),
      msg_indent_break(lapply(column_map, unlist))
    ) |> stop()
  }
  ## TODO: should really be checking for reference_index_list
  labelling_column_names_list = lapply(l, getElement, "labelling_column_names_list") |> unique()
  if (length(labelling_column_names_list) != 1L) {
    msg_colon(
      msg(
        "Union of inconsistent Ledger objects.",
        "All Ledger objects must have the same",
        "labelling_column_names_list, but the following",
        "distinct maps were found:"
      ),
      msg_indent_break(lapply(labelling_column_names_list, unlist))
    ) |> stop()
  }
  frame = mp_rbind(...)
  LedgerData(frame, l[[1L]]$reference_index_list, l[[1L]]$labelling_column_names_list)
}

## not used anymore?
mp_rbind = function(...) {
  (list(...)
   |> lapply(as.data.frame)
   |> do.call(what = bind_rows)  ## bind_rows is in frame_utils.R
  )
}

#' @export
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

#' @export
mp_choose_out = function(x, subset_name, ...) {
  l = list(...)
  p = x$partition
  for (cc in names(l)) {
    vals = l[[cc]]
    p = p$filter_out(vals, .wrt = cc)
  }
  init_merge(p$frame(), subset_name, x$reference_index(), x$labelling_column_names)
}

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
#' ```{r, echo = TRUE, eval = TRUE}
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
#' ```{r, echo = TRUE, eval = TRUE}
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
#' ```{r}
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
    if (length(table_nms) != 2L) {
      stop("joining more than one index requires a list-valued by argument.")
    }
    by = setNames(list(by), to_name(table_nms))
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

mp_aggregate_old = function(formula
  , group_by
  , index
  , ...
) {
  prototypes = list(
    group_sums = macpan2:::MethodPrototype(y ~ group_sums(x), c("x", "y"), character())
  )
  consistent_agg_meths = (prototypes
    |> method_apply("consistent", formula)
    |> vapply(c, logical(1L), USE.NAMES = TRUE)
  )
  if (!any(consistent_agg_meths)) {
    f = method_apply(prototypes, "as_character")
    msg_break(
      msg_colon(
        "The following aggregation formula",
        msg_indent(formula_as_character(formula))
      ),
      msg_colon(
        msg(
          "was not consistent with any of the",
          "available aggregation prototypes"
        ),
        msg_indent_break(f)
      )
    ) |> stop()
  }
  agg_meth = (consistent_agg_meths
    |> which()
    |> names()
    |> getElement(1L)  ## first match takes precedence
  )
  agg = prototypes[[agg_meth]]

  strata = mp_select(x, stratify_by)
  subset = mp_subset(x, ...)
  grouping_indices = mp_indices(
    mp_labels(subset, stratify_by),
    mp_labels(strata)
  )
  subset_indices = mp_indices(
    mp_labels(subset),
    mp_labels(x)
  )

}


#' @export
mp_decompose = function(formula, index, decomp_name, ...) {
  input_formula = formula
  table_args = list(...)
  output_name = lhs_char(formula)
  by_args = table_args
  names(by_args) = sprintf("%s.%s", output_name, names(table_args))
  for (nm in names(table_args)) {
    table_args[[nm]] = mp_select(index, table_args[[nm]])
  }
  linked_indices = do.call(mp_join,
    c(setNames(list(index), output_name), table_args, by_args)
  )
  int_vecs = setNames(
    vector(mode = "list", length(table_args)),
    sprintf("%s_%s", names(table_args), decomp_name)
  )
  iv_nms = names(int_vecs)
  tab_nms = names(table_args)
  expand_iv_nms = sprintf("%s[%s]", tab_nms, iv_nms)
  replacement_formulas = mapply(two_sided
    , tab_nms, expand_iv_nms
    , SIMPLIFY = FALSE, USE.NAMES = FALSE
  )

  for (i in seq_along(table_args)) {
    int_vecs[[iv_nms[i]]] = mp_indices(
      linked_indices$labels_for[[tab_nms[i]]](),
      linked_indices$partition_for[[tab_nms[i]]]()$labels()
    )
  }
  formula = update_formula(formula, replacement_formulas)
  nlist(
    formula,
    input_formula,
    linked_indices,
    int_vecs
  )
}

#' @export
mp_reference = function(x, dimension_name) {
  UseMethod("mp_reference")
}

#' @export
mp_reference.Ledger = function(x, dimension_name) {
  ii = x$reference_index_list[[dimension_name]]
  ii$reset_reference_index()
  ii
}

#' @export
mp_reference.Index = function(x, dimension_name) {
  x$reference_index()
}

#' @export
mp_extract = function(x, dimension_name) {
  UseMethod("mp_extract")
}

#' @export
mp_extract.Ledger = function(x, dimension_name) {
  ii = x$index_for[[dimension_name]]()
  ii$reset_reference_index()
  ii
}

#' @export
mp_extract.DynamicModel = function(x, dimension_name) {
  y = try(x$init_vecs[[dimension_name]]$index, silent = TRUE)
  if (!inherits(y, "Index")) {
    msg(
      "Failed to find an index for",
      dimension_name, "in this object"
    ) |> stop()
  }
  y
}

#' @export
mp_extract.ModelDefRun = function(x, dimension_name) {
  mp_extract(x$dynamic_model, dimension_name)
}

#' Rename Index Columns
#'
#' @param ... Name-value pairs. The name gives the new name and the value
#' is a character vector giving the old name.
#'
#' @family indexes
#' @export
mp_rename = function(x, ...) {
  l = list(...)
  new_nms = names(l)
  old_nms = unlist(l, recursive = FALSE, use.names = FALSE)
  f = x$partition$frame()
  labs = x$labelling_column_names
  i = match(old_nms, names(f))
  if (any(is.na(i))) {
    msg_break(
      msg_colon(
        "Attempted to replace the following names that do not exist",
        msg_indent(old_nms[is.na(i)])
      ),
      msg_colon(
        "These are the only names that are available",
        msg_indent(names(f))
      )
    ) |> stop()
  }
  j = match(old_nms, labs)
  names(f)[i] = new_nms
  labs[j[!is.na(j)]] = new_nms[!is.na(j)]
  Index(f, labelling_column_names = labs)
}

#' @export
mp_group = function(index, by) {
  frame = index$partition$select(to_names(by))$frame()
  nms = names(frame)[names(frame) %in% index$labelling_column_names]
  Index(frame, labelling_column_names = nms)
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
mp_labels = function(x, labelling_column_names) {
  UseMethod("mp_labels")
}

#' @export
mp_labels.Index = function(x, labelling_column_names) {
  if (missing(labelling_column_names)) return(x$labels())
  x$partial_labels(labelling_column_names)
}

#' @export
mp_zero_vector = function(x, ...) {
  UseMethod("mp_zero_vector")
}

#' @export
mp_zero_vector.character = function(x, ...) {
  (x
    |> as.vector()
    |> zero_vector()
  )
}

#' @export
mp_zero_vector.Index = function(x, labelling_column_names, ...) {
  (x
   |> mp_subset(...)
   |> mp_labels(labelling_column_names)
   |> zero_vector()
  )
}

#' @export
mp_labels.Ledger = function(x, labelling_column_names) {
  x$labels_for[[labelling_column_names]]()
}

#' @export
mp_expr_group_sum = function(x
  , stratify_by
  , output_name
  , vector_name
  , subset_name
  , grouping_name
  , length_name
  , ...
) {
  strata = mp_select(x, stratify_by)
  subset = mp_subset(x, ...)
  grouping_indices = mp_indices(
    mp_labels(subset, stratify_by),
    mp_labels(strata)
  )
  subset_indices = mp_indices(
    mp_labels(subset),
    mp_labels(x)
  )
  length_int = strata$labels() |> length()
  e_lhs = output_name
  e_rhs = sprintf("group_sums(%s[%s], %s, %s)"
    , vector_name
    , subset_name
    , grouping_name
    , length_name
  )
  list(
    formula = two_sided(e_lhs, e_rhs),
    int_vecs = setNames(
      list(grouping_indices, subset_indices, length_int),
      c(grouping_name, subset_name, length_name)
    ),
    strata = strata,
    subset = subset
  )
}

#' @export
mp_expr_binop = function(x, y
  , stratify_by
  , output_name
  , vector_name
  , subset_name
  , grouping_name
  , length_name
  , ...
) {}


#' Lookup
#'
#' Lookup a subset or factor index associated with a symbol, and return the
#' index associated with that symbol.
#'
#' @param index Index table (see \code{\link{mp_index}}).
#' @param symbol Character string that could possibly be associated with a
#' subset or factor of `index`.
#'
#' @export
mp_lookup = function(index, symbol) {

  ## check if we are referring to possibly multiple grouping factors
  dim_names = to_names(symbol)
  if (all(dim_names %in% names(index))) {
    return(mp_group(index, symbol))
  }
  all_dim_names = names(index)
  ii = increasing_int_seq(length(all_dim_names), length(dim_names))
  for (i in ii) {
    named_symbol = (symbol
      |> list()
      |> setNames(to_name(all_dim_names[i]))
    )
    args = c(list(index), named_symbol)
    guess = try(do.call(mp_subset, args), silent = TRUE)
    if (!inherits(guess, "try-error")) return(guess)
  }
  for (i in ii) {
    all_perms = apply_permutations(i)
    for (j in seq_len(nrow(all_perms))) {
      named_symbol = (symbol
        |> list()
        |> setNames(to_name(all_dim_names[all_perms[j,]]))
      )
      args = c(list(index), named_symbol)
      guess = try(do.call(mp_subset, args), silent = TRUE)
      if (!inherits(guess, "try-error")) return(guess)
    }
  }
  stop("failed to find symbol")
}


# @param l result of mp_slices or mp_factor or just a named list of indices
mp_unpack = function(l, unpack = c('no', 'maybe', 'yes'), env) {
  unpack = match.arg(unpack)
  if (unpack %in% c('maybe', 'yes')) {
    for (nm in names(l)) {
      already_there = exists(nm, envir = env)
      if (already_there) {
        if (unpack == 'maybe') {
          stop('cannot unpack because slice names already exist')
        } else if (unpack == 'yes') {
          warning('masking or overwriting existing objects with slices')
        }
      }
    }
    for (nm in names(l)) assign(nm, l[[nm]], envir = env)
  }
}

#' @export
mp_factors = function(index, unpack = c('no', 'maybe', 'yes')) {
  unpack = match.arg(unpack)
  factors = list()
  for (d in names(index)) factors[[d]] = mp_group(index, d)
  pf = parent.frame()
  force(pf)
  mp_unpack(factors, unpack, pf)
  factors
}

mp_subset_list = function(index, ..., unpack = c('no', 'maybe', 'yes')) {
  unpack = match.arg(unpack)
  subsets = list(...)

  for (s in names(subsets)) {
    args = c(list(index), subsets[[s]])
    subsets[[s]] = do.call(mp_subset, args)
  }

  pf = parent.frame()
  force(pf)
  mp_unpack(subsets, unpack, pf)
  subsets
}

mp_custom_slices = function(index, ..., unpack = c('no', 'maybe', 'yes')) {
  unpack = match.arg(unpack)
  slice_refs = list(...)
  nms = names(slice_refs)
  for (i in seq_along(slice_refs)) {
    r = slice_refs[[i]]
    if (is.null(nms[i]) | nchar(nms[i]) == 0L) {
      slice_refs[[i]] = (index
        |> mp_group(r)
        |> as.data.frame()
        |> unlist(recursive = FALSE, use.names = FALSE)
      )
      names(slice_refs)[[i]] = r
    }
  }
  slices = list()
  for (d in names(slice_refs)) {
    for (s in slice_refs[[d]]) {
      if (s %in% names(slices)) {
        warning("duplicated slice")
      } else {
        args = c(list(index), setNames(list(s), d))
        slices[[s]] = do.call(mp_subset, args)
      }
    }
  }
  pf = parent.frame()
  force(pf)
  mp_unpack(slices, unpack, pf)
  slices
}

## TODO: list of indices where precedence is higher left and top
#' @export
mp_slices = function(index, unpack = c('no', 'maybe', 'yes')) {
  unpack = match.arg(unpack)
  possible_slices = (index
    |> as.data.frame()
    |> lapply(unique)
    |> lapply(setdiff, "")
  )
  slices = list()
  for (d in names(possible_slices)) {
    for (s in possible_slices[[d]]) {
      if (s %in% names(slices)) {
        warning("duplicated slice")
      } else {
        args = c(list(index), setNames(list(s), d))
        slices[[s]] = do.call(mp_subset, args)
      }
    }
  }
  pf = parent.frame()
  force(pf)
  mp_unpack(slices, unpack, pf)
  slices
}
