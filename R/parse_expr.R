#' Generate an Arithmetic Expression Parser
#'
#' @param parser_name Name of the parsing function as a character
#' string.
#' @param finalizer Function used to post-process a parsed formula.
#' The default is the identity finalizer, which returns the parsed
#' formula itself.  Other good choices are \code{\link{finalizer_char}},
#' which can be used to understand how the formula has been parsed,
#' and \code{\link{finalizer_index}}, which can be passed to the C++ engine.
#'
#' The result of this function is another function that takes a single
#' argument, \code{x}.  This resulting function is recursive.  The \code{x}
#' argument should be a one-sided formula the first time this recursive
#' function is called.  In subsequent evaluations of the recursion, \code{x}
#' will be a list with the following structure.  When \code{x} is a formula,
#' it must contain a named list of functions called \code{valid_funcs} and
#' a named list of variables called \code{valid_vars}.
#'
#' \describe{
#'   \item{x}{list of names and numeric objects that represent each
#'   leaf of the parse tree}
#'   \item{n}{integer vector the same length as \code{x} that give the
#'   number of arguments of the associated functions in \code{x} or \code{0}
#'   otherwise}
#'   \item{i}{index identifying the element of \code{x} corresponding to the
#'   first argument of the associated function or \code{0} if this is not
#'   a function}
#'   \item{valid_funcs}{named list of valid functions that was extracted
#'   from the environment of the formula being parsed}
#'   \item{valid_vars}{named list of default values of valid variables extracted
#'   from the environment of the formula being parsed}
#'   \item{input_expr_as_string}{the input formula stored as a string}
#' }
#'
#' @examples
#' parser = make_expr_parser('parser', finalizer_char)
#' foi = ~ beta * I / 100
#' valid_funcs = setNames(
#'   list(`*`, `/`),
#'   c("*", "/")
#' )
#' valid_vars = list(beta = 0.1, I = 30)
#' parser(foi)
#'
#' @export
make_expr_parser = function(
    parser_name = 'parse_expr',
    finalizer = force  # the identity finalizer is the default
  ) {

  # convert a formula to the initial state of a list that could be
  # recursively parsed using parse_expr
  formula_to_parsing_list = function(x) {
    stopifnot(
      "formulas are the only calls that can be parsed" =
        as.character(x[[1]]) == '~'
    )
    stopifnot(
      "only one-sided formulas can be parsed" =
        length(x) == 2L
    )
    valid = as.list(environment(x))
    list(
      x = list(x), n = 0L, i = 0L,
      valid_funcs = valid$valid_funcs,
      valid_vars = valid$valid_vars
      #input_expr_as_string = as.character(x)[2L]
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
  valid_funcs = x$valid_funcs
  valid_vars = x$valid_vars
  valid_literals = as.numeric(x$valid_literals)
  x$valid_funcs = x$valid_vars = x$valid_literals = NULL

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
  new_valid_literals = as.numeric(x$x[is_literal])
  x$n[is_literal] = -1L

  # convert character identifiers to integers
  x_char = unlist(lapply(x$x, as.character))
  x_int = integer(length(x$x))
  if (any(is_func)) {
    x_int[is_func] = get_indices(x_char[is_func]
      , vec = names(valid_funcs)
      , vec_type = "functions"
      , expr_as_string = x$input_expr_as_string
      , zero_based = FALSE
    )
  }
  if (any(is_var)) {
    x_int[is_var] = get_indices(x_char[is_var]
      , vec = names(valid_vars)
      , vec_type = "variables"
      , expr_as_string = x$input_expr_as_string
      , zero_based = TRUE
    )
  }
  if (any(is_literal)) {
    x_int[is_literal] = length(valid_literals) + seq_along(new_valid_literals) - 1L
    valid_literals = c(valid_literals, new_valid_literals)
  }
  x$x = x_int

  McMasterPandemic::nlist(
    parse_table = as.data.frame(x),
    valid_funcs, valid_vars, valid_literals
  )
}

get_indices = function(x, vec, vec_type, expr_as_string, zero_based = FALSE) {
  if (!is.character(vec)) vec = names(vec)
  missing_items = x[!x %in% vec]
  if (length(missing_items) > 0L) {
    stop(
      "\nthe expression given by:\n",
      expr_as_string, "\n\n",
      "contained the following ", vec_type, ":\n",
      paste0(missing_items, collapse = " "), "\n\n",
      " that were not found in the list of available ", vec_type, ":\n",
      paste0(vec, collapse = " ") # TODO: smarter pasting when this list gets big
    )
  }
  one_based = apply(
    outer(
      as.character(x),
      vec,
      "=="
    ),
    1,
    which
  )
  if (zero_based) return(one_based - 1L)
  return(one_based)
}
