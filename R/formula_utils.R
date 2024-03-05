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
    component_vec_by = getOption("macpan2_vec_by")
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

## update formula symbolically with additional formulas
## in replacers, each of which has a lhs matching a symbol in
## the focal formula and a rhs to replace that symbol with
update_formula = function(formula, replacers) {
  nms = lapply(replacers, lhs_char)
  l = lapply(replacers, rhs_expr) |> setNames(nms)
  do.call('substitute', list(formula, l))
}
update_formulas = function(list, replacers) {
  nms = lapply(replacers, lhs_char)
  l = lapply(replacers, rhs_expr) |> setNames(nms)
  for (i in seq_along(list)) {
    formula = list[[i]]
    
    ## do not fully understand this magic
    updated_formula = do.call('substitute', list(formula, l))
    if (!inherits(updated_formula, "formula") & is.call(updated_formula)) {
      updated_formula = eval(updated_formula)
    }
    
    list[[i]] = updated_formula
  }
  list
}

rhs_sum = function(...) {
  blank_to_zero = function(x) if (x == "") return("0") else return(x)
  no_zeros = function(x) x[trimws(x) != "0"]
  (list(...) 
   |> vapply(rhs_char, character(1L)) 
   |> no_zeros()
   |> paste(collapse = " + ")
   |> blank_to_zero()
   |> sprintf(fmt = "~%s") 
   |> as.formula()
  )
}


## for character vectors lhs and rhs, return a formula
one_sided = function(rhs) {
  as.formula(sprintf("~ %s", as.character(rhs)))
}
two_sided = function(lhs, rhs) {
  as.formula(sprintf("%s ~ %s", as.character(lhs), as.character(rhs)))
}


swap_sides = function(x) UseMethod("swap_sides")

#' @export
swap_sides.formula = function(x) {
  stopifnot(is_two_sided(x))
  formula = x
  formula[[2L]] = x[[3L]]
  formula[[3L]] = x[[2L]]
  formula
}

#' @export
swap_sides.character = function(x) {
  stopifnot(length(x) == 1L)
  x
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

is_lhs_symbol = function(formula) is.symbol(lhs_expr(formula))
is_rhs_symbol = function(formula) is.symbol(rhs_expr(formula))

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

rhs_expr = function(formula) {
  if (is_two_sided(formula)) return(formula[[3L]])
  formula[[2L]]
}
lhs_expr = function(formula) {
  if (is_two_sided(formula)) return(formula[[2L]])
  msg_colon(
    "There is no left-hand-side in this formula",
    deparse(formula, width.cutoff = 500)
  ) |> stop()
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

formula_as_character = function(formula) {
  sprintf("%s ~ %s", lhs_char(formula), rhs_char(formula))
}

# formula parsing in macpan2 works one side at a time. but sometimes
# it is helpful to parse two-sided formulas. this function does so
# by parsing each side at a time and rbinding the results.
concat_parse_table = function(formula, side = c("both", "left", "right")) {
  side = match.arg(side)
  if (is_one_sided(formula)) return(method_parser(formula))
  switch(side
    , both = rbind(method_parser(lhs(formula)), method_parser(rhs(formula)))
    , left = method_parser(lhs(formula))
    , right = method_parser(rhs(formula))
  )
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
formula_components = function(formula, side = c("both", "left", "right")) {
  parse_table = concat_parse_table(formula, side)
  is_var_or_lit = parse_table$n == 0L
  is_func = parse_table$n > 0L
  is_lit = grepl("^[0-9]*\\.?[0-9]*$", parse_table$x)
  list(
    variables = parse_table$x[is_var_or_lit & !is_lit] |> unique(),
    functions = parse_table$x[is_func] |> unique(),
    literals = parse_table$x[is_var_or_lit & is_lit] |> as.numeric() |> unique()
  )
}

trans_lhs_var = function(formula) {
  lhs_vars = formula_components(formula, side = "left")$variables
  if (length(lhs_vars) != 1L) stop("Transformations must have a single, unsubsetted variable on the left-hand-side")
  lhs_vars
}
trans_rhs_var = function(formula) {
  rhs_vars = formula_components(formula, side = "right")$variables
  if (length(rhs_vars) != 1L) stop("Transformations must have a single variable on the right-hand-side")
  rhs_vars
}

lhs_pieces = function(formula) {
  table = method_parser(formula)

  subset_vector_case = all(
    identical(table$x[1:2], c("~", "[")),
    identical(table$n, c(1L, 2L, 0L, 0L)),
    identical(table$i, c(1L, 2L, 0L, 0L))
  )

  matrix_case = all(
    identical(table$x[1], "~"),
    identical(table$n, c(1L, 0L)),
    identical(table$i, c(1L, 0L))
  )

  if (subset_vector_case) {
    l = list(
      variable = table$x[3L],
      positions = table$x[4L]
    )
  } else if (matrix_case) {
    l = list(
      variable = table$x[2L],
      positions = character()
    )
  } else {
    l = list(
      variable = character(),
      positions = character()
    )
  }
  return(l)
}
