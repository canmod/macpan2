to_special_vecs = function(formula, component_list, matrix_list) {
  arg_signature = c(
    unlist(component_list, use.names = FALSE, recursive = FALSE),
    matrix_list
  )
  form_parts = vapply(formula, deparse1, character(1L))
  stopifnot(form_parts[1L] == "~")
  ee = MathExpressionFromStrings(form_parts[[3L]], arg_signature)
  hh = MathExpressionFromStrings(form_parts[[2L]], arg_signature)
  for (nm in names(component_list)) {
    if (nm == "...RAW...INDICES...") { ## this token probably won't clash ... right??
      component_list[[nm]] = as.character(seq_along(component_list[[nm]]) - 1L)
    } else {
      component_list[[nm]] = sprintf("%s[%s]", nm, seq_along(component_list[[nm]]) - 1L)
    }
  }
  args = as.list(c(
    unlist(component_list, use.names = FALSE, recursive = FALSE),
    matrix_list
  ))
  lhs = do.call(hh$symbolic$evaluate, args)
  rhs = do.call(ee$symbolic$evaluate, args)
  ("%s ~ %s"
    |> sprintf(lhs, rhs)
    |> as.formula()
  )
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

## for character vectors lhs and rhs, return a formula
one_sided = function(rhs) {
  as.formula(sprintf("~ %s", as.character(rhs)))
}
two_sided = function(lhs, rhs) {
  as.formula(sprintf("%s ~ %s", as.character(lhs), as.character(rhs)))
}