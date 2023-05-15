to_special_vecs = function(formula, component_list, matrix_list) {
  arg_signature = c(
    unlist(component_list, use.names = FALSE, recursive = FALSE),
    matrix_list
  )
  state_components = c("S", "I", "R")
  flow_components = c("foi", "gamma")
  form_parts = vapply(formula, deparse1, character(1L))
  stopifnot(form_parts[1L] == "~")
  ee = MathExpressionFromStrings(form_parts[[3L]], arg_signature)
  hh = MathExpressionFromStrings(form_parts[[2L]], arg_signature)
  for (nm in names(component_list)) {
    component_list[[nm]] = sprintf("%s[%s]", nm, seq_along(component_list[[nm]]) - 1L)
  }
  args = as.list(c(
    unlist(component_list, use.names = FALSE, recursive = FALSE),
    matrix_list
  ))
  as.formula(sprintf("%s ~ %s", do.call(hh$symbolic$evaluate, args), do.call(ee$symbolic$evaluate, args)))
}

to_assign = function(formula) {
  lhs = formula[[2L]]
  rhs = formula[[3L]]
  lhs_char = vapply(lhs, deparse1, character(1L))
  if (lhs_char[1L] != "[") return(formula)
  if (length(lhs_char) == 3L) lhs_char = append(lhs_char, "0")
  rhs_char = deparse1(rhs)
  args = as.list(c(
    "dummy ~ assign(%s, %s, %s, %s)",
    lhs_char[-1L],
    rhs_char
  ))
  as.formula(do.call(sprintf, args))
}
