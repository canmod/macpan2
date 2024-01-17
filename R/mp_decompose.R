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
