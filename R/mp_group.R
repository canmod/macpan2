#' @export
mp_group = function(index, by) {
  frame = index$partition$select(to_names(by))$frame()
  nms = names(frame)[names(frame) %in% index$labelling_column_names]
  Index(frame, labelling_column_names = nms)
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
