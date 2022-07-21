#' Generate an Arithmetic Expression Parser
#'
#' @param parser_name character vector with names of valid functions
#' @param valid_vars character vector with names of valid variables
#'
#' @export
make_expr_parser = function(
    parser_name = 'parse_expr',
    finalizer = force
  ) {

  is_name_or_number = function(x) {
    is.name(x) | is.numeric(x)
  }

  # convert a formula to the initial state of a list that could be
  # recursively parsed using parse_expr
  formula_to_parsing_list = function(x) {
    stopifnot(as.character(x[[1]]) == '~')
    stopifnot(length(x) == 2L)
    valid = as.list(environment(x))
    list(
      x = list(x), n = 0L, i = 0L,
      valid_funcs = valid$valid_funcs,
      valid_vars = valid$valid_vars
    )
  }

  # take one step towards reducing an expression into its components
  # @param x parsing list
  # @param expr_id index for the expression to be reduced
  reduce_expr = function(x, expr_id) {
      # how many expressions do we currently have?
      n_new = length(x$x)

      # add function arguments of current expression to
      # the list of expressions
      x$x = append(x$x, as.list(x$x[[expr_id]])[-1L])

      # add placeholders for the i and n that are associated
      # with these new arguments -- these placeholders
      # will get updated at the next recursion if it if
      # discovered that they are also functions
      rep_0 = rep(0L, length(x$x[[expr_id]]) - 1L)
      x$n = append(x$n, rep_0)
      x$i = append(x$i, rep_0)

      # update the current expression to reflect how it has
      # been reduced
      x$n[[expr_id]] = length(x$x[[expr_id]]) - 1L
      x$x[[expr_id]] = x$x[[expr_id]][[1L]]
      x$i[[expr_id]] = n_new + 1L

      x
  }

  function(x) {

    # parse_expr recursively adjusts a specially-structured
    # list of expressions, but the user should be able to
    # just supply a call expression initially
    if (is.call(x)) x = formula_to_parsing_list(x)

    # continue the recursion until each expression in the list is
    # either a name or a number
    is_reduced = unlist(lapply(x$x, is_name_or_number), recursive = FALSE)
    if (all(is_reduced)) return(finalizer(x))

    # loop over expressions that haven't been reduced to
    # a name or number, and continue the reduction process
    not_reduced_id = which(!is_reduced)
    for (expr_id in not_reduced_id) {x = reduce_expr(x, expr_id)}

    # recusively call to check if the reduction process is complete
    # TODO: prove that infinite recursion is not possible
    do.call(parser_name, list(x))
  }
}


#' @export
finalizer_char = function(x) {
  data.frame(x = unlist(lapply(x$x, as.character)), n = x$n, i = x$i)
}

#' @export
finalizer_index = function(x) {
  find_vec_indices = McMasterPandemic:::find_vec_indices
  valid_funcs = x$valid_funcs
  valid_vars = x$valid_vars
  x$valid_funcs = x$valid_vars = NULL

  # remove the tilde function, which is in the first position,
  # and adjust the indices in i accordingly
  x = within(x, {
    x <- x[-1]
    n <- n[-1]
    i <- i[-1]
    i <- i - 1L
    i[i == -1L] = 0
  })

  # classify the different types of objects
  is_literal = unlist(lapply(x$x, is.numeric))
  is_func = x$n != 0L
  is_var = (!is_func) & (!is_literal)

  # identify literals with -1 in the 'number of arguments'
  valid_literals = x$x[is_literal]
  x$n[is_literal] = -1L

  # convert character identifiers to integers
  x_char = unlist(lapply(x$x, as.character))
  x_int = integer(length(x$x))
  if (any(is_func)) {
    x_int[is_func] = find_vec_indices(x_char[is_func], names(valid_funcs))
  }
  if (any(is_var)) {
    x_int[is_var] = find_vec_indices(x_char[is_var], names(valid_vars))
  }
  if (any(is_literal)) {
    x_int[is_literal] = seq_along(valid_literals)
  }
  x$x = x_int

  McMasterPandemic::nlist(
    parse_table = as.data.frame(x),
    valid_funcs, valid_vars, valid_literals
  )
}
