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
  Descriptors(f, labelling_names = labelling_names)
}

#' @export
mp_square = function(x, y_labelling_names) {
  nms = to_names(y_labelling_names)
  y = (x$partition$frame()
    |> setNames(nms)
    |> Descriptors(nms)
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
  Descriptors(f, names(f))
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
  Descriptors(f, names(f))
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
  Descriptors(f, names(f))
}

#' @export
mp_subset = function(x, ...) {
  partition = mp_choose(x, "pick", ...)$partition
  Descriptors(partition, x$labelling_names)
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
  Descriptors(do.call(union_vars, partitions)$frame(), labelling_names)
}

#' @export
mp_choose = function(x, subset_name, ...) {
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
  init_merge(p$frame(), subset_name, x$labelling_names)
}

#' @export
mp_choose_out = function(x, subset_name, ...) {
  l = list(...)
  p = x$partition
  for (cc in names(l)) {
    vals = l[[cc]]
    p = p$filter_out(vals, .wrt = cc)
  }
  init_merge(p$frame(), subset_name, x$labelling_names)
}

#' @export
mp_join = function(...) {
  args = list(...)
  is_basis = vapply(args, inherits, logical(1L), "Descriptors")
  is_relationship = vapply(args, inherits, logical(1L), "Relationship")
  table_list = args[is_basis | is_relationship]
  by_list = args[!is_basis & !is_relationship]
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
  frame |> Descriptors(labelling_names = nms)
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
mp_labels.Descriptors = function(x, labelling_names) {
  if (missing(labelling_names)) return(x$labels())
  x$partial_labels(labelling_names)
}

#' @export
mp_zero_vector = function(x, labelling_names, ...) {
  (x
   |> mp_subset(...)
   |> mp_labels(labelling_names)
   |> zero_vector()
  )
}

#' @export
mp_labels.Relationship = function(x, labelling_names) {
  x$labels_for[[labelling_names]]()
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
  e_rhs = sprintf("groupSums(%s[%s], %s, %s)"
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
