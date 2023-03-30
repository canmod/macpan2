is_name_or_number = function(x) {
  is.name(x) | is.numeric(x)
}


#' Generate an Arithmetic Expression Parser
#'
#' @param parser_name Name of the parsing function as a character
#' string.
#' @param finalizer Function used to post-process a parsed formula.
#' The default is the identity finalizer, which returns the parsed
#' formula itself.  Other good choices are \code{finalizer_char},
#' which can be used to understand how the formula has been parsed,
#' and \code{finalizer_index}, which can be passed to the C++ engine.
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

  parser_name = force(parser_name)

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
    #valid = as.list(environment(x))
    offset = environment(x)$offset
    if (is.null(offset)) offset = 0L
    list(
      x = list(x), n = 0L, i = 0L,
      valid_funcs = environment(x)$valid_funcs,
      valid_vars = environment(x)$valid_vars,
      valid_literals = environment(x)$valid_literals,
      offset = offset,
      input_expr_as_string = as.character(x)[2L]
    )
  }

  # take one step towards reducing an expression into its components
  # @param x parsing list
  # @param expr_id index for the expression to be reduced
  reduce_expr = function(x, expr_id) {
      #browser()
      # how many expressions do we currently have?
      n_new = length(x$x)

      # add function arguments of current expression to
      # the list of expressions
      x$x = append(x$x, as.list(x$x[[expr_id]])[-1L])

      # add placeholders for the i and n that are associated
      # with these new arguments -- these placeholders
      # will get updated at the next recursion if it is
      # discovered that they are also functions
      rep_0 = rep(0L, length(x$x[[expr_id]]) - 1L)
      x$n = append(x$n, rep_0)
      x$i = append(x$i, rep_0)

      # update the current expression to reflect how it has
      # been reduced
      x$n[[expr_id]] = length(x$x[[expr_id]]) - 1L
      x$x[[expr_id]] = x$x[[expr_id]][[1L]]
      x$i[[expr_id]] = n_new + x$offset

      x
  }

  function(x) {
    #penv = parent.frame()
    #browser()

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

    # recursively call to check if the reduction process is complete
    # TODO: prove that infinite recursion is not possible
    #writeLines(ls(parent.frame()), "~/Development/macpan2/misc/testing.txt")
    do.call(get(parser_name, parent.frame()), list(x))
  }
}

#' Finalizers
#' @param x Raw parsed expression.
#' @name finalizer
NULL

#' @describeIn finalizer Finalize parsed expression so that the parse table is
#' a little more human readable.
#' @export
finalizer_char = function(x) {
  data.frame(x = unlist(lapply(x$x, as.character)), n = x$n, i = x$i)
}

#' @describeIn finalizer Finalize parsed expression so that the parse table can
#' be passed to the C++ engine.
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
    i[i == -1L] = 0L
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
      , zero_based = TRUE
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
  x$x = as.integer(x_int)
  x$i = as.integer(x$i)
  #browser()
  nlist(
    parse_table = as.data.frame(x),
    valid_funcs, valid_vars, valid_literals
  )
}

parse_expr = make_expr_parser(finalizer = finalizer_index)

get_indices = function(x, vec, vec_type, expr_as_string, zero_based = FALSE) {
  if (!is.character(vec)) vec = names(vec)
  missing_items = x[!x %in% vec]
  if (length(missing_items) > 0L) {
    pointers = ""
    if (vec_type == "functions") {
      pointers = "\nPlease see ?engine_functions for more information on the available functions."
    } else if (vec_type == "variables") {
      pointers = paste(
        "\nPlease ensure that all variables are being initialized",
        "\nEven variables that are derived and not dependencies of",
        "\nother expressions must at least be initialized as an ?empty_matrix.",
        "\n",
        sep = ""
      )
    }
    stop(
      "\nthe expression given by:\n",
      expr_as_string, "\n\n",
      "contained the following ", vec_type, ":\n",
      paste0(missing_items, collapse = " "), "\n\n",
      " that were not found in the list of available ", vec_type, ":\n",
      paste0(vec, collapse = " "), # TODO: smarter pasting when this list gets big
      pointers
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


#' Initial Valid Variables
#'
#' @param valid_var_names Character vector of variable names.
#'
#' @export
initial_valid_vars = function(valid_var_names) {
  setNames(
    rep(list(matrix(0)), length(valid_var_names)),
    valid_var_names
  )
}

#' Empty Matrix
#'
#' Empty matrices are useful when defining matrices that do not need to be
#' initialized because they will get computed before they are required by
#' other expressions. They are also a useful placeholder for matrices that
#' should only have a value after a certain phase in the simulation.
#'
#' @name empty_matrix
#' @format A \code{\link{numeric}} \code{\link[base]{matrix}} with zero rows
#' and zero columns.
#' @examples
#' s = TMBModel(
#'   init_mats = MatsList(x = empty_matrix
#'     , .mats_to_save = "x"
#'     , .mats_to_return = "x"
#'   ),
#'   expr_list = ExprList(during = list(x ~ time_step(0))),
#'   time_steps = Time(2)
#' )
#' s$simulator()$report()
#' @export
empty_matrix = matrix(numeric(0L), 0L, 0L)

#' Parse Expression List
#'
#' Parse a list of one-sided formulas representing expressions
#' in a compartmental model.
#'
#' @param expr_list List of one-sided formulas.
#' @param valid_vars Named list of numerical matrices that can
#' be referred to in the formulas.
#' @param valid_literals An optional existing numeric vector of valid literals
#' from a related expression list.
#' @param offset The zero-based row index for the first row of the table.
#' This is useful when combining tables.
#'
#' @export
parse_expr_list = function(expr_list
    , valid_vars
    , valid_literals = numeric(0L)
    , offset = 0L
  ) {
  eval_env = list2env(nlist(valid_funcs, valid_vars, valid_literals, offset))
  pe_list = list()
  for (i in seq_along(expr_list)) {
    environment(expr_list[[i]]) = eval_env
    #browser()
    pe_list[[i]] = parse_expr(expr_list[[i]])
    eval_env$valid_literals = pe_list[[i]]$valid_literals
    eval_env$offset = eval_env$offset + nrow(pe_list[[i]]$parse_table)
  }
  p_tables = lapply(pe_list, getElement, "parse_table")
  list(
    parse_table = do.call(rbind, p_tables),
    valid_literals = eval_env$valid_literals,
    num_p_table_rows = vapply(p_tables, nrow, integer(1L))
  )
}
