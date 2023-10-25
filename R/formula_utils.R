#' To Special Vectors
#'
#' Takes a formula that refers to components of special vectors (e.g.
#' `state`, `flow`, `trans`), and returns a formula that can be processed
#' by the engine.
#'
#' @param formula A formula
#' @param component_list Named list of character vectors, with names
#' giving special vectors and character vectors giving the names of the
#' components of those special vectors. For example,
#' `list(state = c("S", "I"), flow = "infection")`.
#' @param matrix_list Character vector of the names of matrices and methods.
#' @param component_vec_by Named character vector, with names giving the
#' special vectors. The values associated with these names gives the vector
#' that will be accessed by the component names.
#' @noRd
to_special_vecs = function(
    formula, component_list, matrix_list,
    component_vec_by = c(state = "state", flow = "flow")
  ) {
  arg_signature = c(
    unlist(component_list, use.names = FALSE, recursive = FALSE),
    matrix_list
  )
  form_parts = vapply(formula, deparse1, character(1L))
  stopifnot(form_parts[1L] == "~")
  ee = MathExpressionFromStrings(form_parts[[3L]], arg_signature)
  hh = MathExpressionFromStrings(form_parts[[2L]], arg_signature)
  for (nm in names(component_list)) {

    ## this ...RAW...INDICES... token won't clash
    ## it just means that if you
    ## name a new special vector (e.g. state, flow) it cannot be called
    ## ...RAW...INDICES..., which seems fine
    if (component_vec_by[[nm]] == "...RAW...INDICES...") {
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
  two_sided(lhs, rhs)
}


to_assign = function(formula, dummy = "dummy") {
  lhs = formula[[2L]]
  rhs = formula[[3L]]
  lhs_char = vapply(lhs, deparse1, character(1L))
  if (lhs_char[1L] != "[") return(formula)
  if (length(lhs_char) == 3L) lhs_char = append(lhs_char, "0")
  rhs_char = deparse1(rhs)
  args = as.list(c(
    "%s ~ assign(%s, %s, %s, %s)",
    dummy,
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

## how many sides does a formula have?
is_zero_sided = function(formula) {
  (length(formula) == 1L) & inherits(formula, "formula")
}
is_one_sided = function(formula) {
  (length(formula) == 2L) & inherits(formula, "formula")
}
is_two_sided = function(formula) {
  (length(formula) == 3L) & inherits(formula, "formula")
}

rhs = function(formula) {
  if (is_two_sided(formula)) formula = formula[-2L]
  formula
}
lhs = function(formula) {
  if (is_two_sided(formula)) {
    formula = formula[-3L]
  } else if (is_one_sided(formula)) {
    formula = formula[1L]
  }
  formula
}

lhs_char = function(formula) {
  if (is_two_sided(formula)) {
    return(deparse(formula[[2L]], 500))
  }
  ""
}

rhs_char = function(formula) {
  i = 2L
  if (is_two_sided(formula)) i = 3L
  deparse(formula[[i]], 500)
}

# formula parsing in macpan2 works one side at a time. but sometimes
# it is helpful to parse two-sided formulas. this function does so
# by parsing each side at a time and rbinding the results.
concat_parse_table = function(formula) {
  if (is_one_sided(formula)) return(method_parser(formula))
  rbind(method_parser(lhs(formula)), method_parser(rhs(formula)))
}

# When looking at a formula without any additional information (e.g.
# no information on the matrices in the model), one can classify three
# types of components that make up a formula:
# 1. variables
# 2. functions
# 3. literals
#
# In the context of a model, variables can be things like matrices,
# methods, integer vectors.
#
# This function returns a list with three components each giving a list of
# the components of that type.
formula_components = function(formula) {
  parse_table = concat_parse_table(formula)
  is_var_or_lit = parse_table$n == 0L
  is_func = parse_table$n > 0L
  is_lit = grepl("^[0-9]*\\.?[0-9]*$", parse_table$x)
  list(
    variables = parse_table$x[is_var_or_lit & !is_lit] |> unique(),
    functions = parse_table$x[is_func] |> unique(),
    literals = parse_table$x[is_var_or_lit & is_lit] |> as.numeric() |> unique()
  )
}
