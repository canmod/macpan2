is_name_or_number = function(x) {
  is.name(x) | is.numeric(x)
}
is_na = function(x) {
  i = is.vector(x)
  if (any(i)) i[i] = length(x[i]) == 1L
  if (any(i)) i[i] = is.na(x[i])
  return(i)
}

#' Generate an Arithmetic Expression Parser
#'
#' @param parser_name Name of the parsing function as a character
#' string. No longer used, but still present for back-compatibility.
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
#' parser = make_expr_parser(finalizer = finalizer_char)
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
    parser_name = NULL,
    finalizer = force  # the identity finalizer is the default
  ) {

  if (!is.null(parser_name) & interactive()) {
    warning("This parser_name argument is here for back-compatibility only, and will be removed in future versions of macpan2.")
  }

  # convert a formula to the initial state of a list that could be
  # recursively parsed using parse_expr
  formula_to_parsing_list = function(x) {
    #if (interactive()) browser()
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
      valid_methods = environment(x)$valid_methods,
      valid_int_vecs = environment(x)$valid_int_vecs,
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

    # parse_expr recursively adjusts a specially-structured
    # list of expressions, but the user should be able to
    # just supply a call expression initially
    if (is.call(x)) x = formula_to_parsing_list(x)
    
    # literal NA tokens to be treated as missing _numeric_ values
    which_na = vapply(x$x, is_na, logical(1L))
    if (any(which_na)) x$x[which_na] = rep(list(NA_real_), sum(which_na))

    # continue the recursion until each expression in the list is
    # either a name or a number
    is_reduced = unlist(lapply(x$x, is_name_or_number), recursive = FALSE)
    
    # prevent the use of argument names
    if (any(nzchar(names(is_reduced)))){
      stop("Argument names cannot be used in engine expressions. The following argument names were used: "
           ,paste0(names(is_reduced)[which(nzchar(names(is_reduced)))],collapse = ","))
    }
    
    if (all(is_reduced)) return(finalizer(x))

    # loop over expressions that haven't been reduced to
    # a name or number, and continue the reduction process
    not_reduced_id = which(!is_reduced)
    for (expr_id in not_reduced_id) {x = reduce_expr(x, expr_id)}

    # recursively call to check if the reduction process is complete
    Recall(x)
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
  valid_methods = x$valid_methods
  valid_int_vecs = x$valid_int_vecs
  valid_literals = as.numeric(x$valid_literals)
  x$valid_funcs = x$valid_vars = x$valid_literals = x$valid_methods = x$valid_int_vecs = NULL

  # remove the tilde function, which is in the first position,
  # and adjust the indices in i accordingly
  x = within(x, {
    x <- x[-1]
    n <- n[-1]
    i <- i[-1]
    i <- i - 1L
    i[i == -1L] = 0L
  })

  x_char = unlist(lapply(x$x, as.character))

  # classify the different types of objects
  is_literal = unlist(lapply(x$x, is.numeric))
  is_func = x$n != 0L
  is_meth = x_char %in% names(valid_methods)
  is_int_vec = x_char %in% names(valid_int_vecs)
  is_var = x_char %in% names(valid_vars)
  is_found = is_literal | is_func | is_meth | is_int_vec | is_var
  is_not_found = !is_found
  is_broad_sense_var = is_var | is_meth | is_int_vec

  if (any(is_not_found)) {
    missing_items = x_char[is_not_found]
    available_items = x_char[is_broad_sense_var]
    expr_msg = msg_colon(
        "The expression given by"
      , msg_indent(x$input_expr_as_string)
    )
    missing_msg = msg_colon(
        msg(
            "contained the following symbols"
          , "representing model variables"
        )
        , msg_indent(missing_items)
    )
    if (length(available_items) == 0L) {
      issue_msg = "but no variables were declared in the model."
    } else {
      issue_msg = msg_colon(
        msg(
            "that were neither functions nor one of"
          , "the following valid variables"
        ),
        msg_indent(available_items)
      )
    }
    msg_break(expr_msg, missing_msg, issue_msg) |> msg_hline() |> stop()
  }

  # identify literals with -1 in the 'number of arguments'
  new_valid_literals = as.numeric(x$x[is_literal])
  x$n[is_literal] = -1L

  # identify methods with -2 in the 'number of arguments'
  x$n[is_meth] = -2L

  # identify integer vectors with -2 in the 'number of arguments'
  x$n[is_int_vec] = -3L

  # convert character identifiers to integers
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
    x_int[is_literal] = (length(valid_literals)
      + seq_along(new_valid_literals)
      - 1L
    )
    valid_literals = c(valid_literals, new_valid_literals)
  }
  if (any(is_meth)) {
    x_int[is_meth] = get_indices(x_char[is_meth]
      , vec = valid_methods
      , vec_type = "methods"
      , expr_as_string = x$input_expr_as_string
      , zero_based = TRUE
    )
  }
  if (any(is_int_vec)) {
    x_int[is_int_vec] = get_indices(x_char[is_int_vec]
      , vec = valid_int_vecs
      , vec_type = "int_vecs"
      , expr_as_string = x$input_expr_as_string
      , zero_based = TRUE
    )
  }
  x$x = as.integer(x_int)
  x$i = as.integer(x$i)
  nlist(
    parse_table = as.data.frame(x),
    valid_funcs, valid_vars, valid_methods, valid_int_vecs, valid_literals
  )
}

parse_expr = make_expr_parser(finalizer = finalizer_index)
method_parser = make_expr_parser(finalizer = finalizer_char)

get_indices = function(x, vec, vec_type, expr_as_string, zero_based = FALSE) {
  if (!is.character(vec)) vec = names(vec)
  missing_items = x[!x %in% vec]
  if (length(missing_items) > 0L) {
    pointers = ""
    if (vec_type == "functions") {
      pointers = "\nPlease see ?engine_functions for more information on the available functions."
    } else if (vec_type == "variables") {
      pointers = msg(
        "Please ensure that all variables are being initialized",
        "Even variables that are derived and not dependencies of",
        "other expressions must at least be initialized as an",
        "?empty_matrix.\n"
      )
    } else if (vec_type == "methods") {
      pointers = msg("Help for ?engine_methods is under construction.")
    } else if (vec_type == "int_vecs") {
      pointers = msg(
        "Please ensure that engine methods refer",
        "to the right integer vectors"
      )
    }
    msg_break(
      msg_colon(
        "The expression given by",
        msg_indent(expr_as_string)
      ),
      msg_colon(
        msg("contained the following", vec_type),
        msg_indent(missing_items)
      ),
      msg_colon(
        msg("that were not found in the list of available", vec_type),
        msg_indent(vec)
      ),
      pointers
    ) |> stop()
  }
  one_based = apply(outer(as.character(x), vec, "=="), 1, which)
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
#' other expressions. They can also provide a useful placeholder for matrices 
#' that should only have a value after a certain phase in the simulation.
#'
#' @name empty_matrix
#' @format A \code{\link{numeric}} \code{\link[base]{matrix}} with zero rows
#' and zero columns.
#' @examples
#' spec = mp_tmb_model_spec(during = list(x ~ time_step(0)))
#' identical(spec$empty_matrices()$x, empty_matrix) ## TRUE
#' @export
empty_matrix = matrix(numeric(0L), 0L, 0L)

#' Empty Trajectory
#' 
#' Output of \code{\link{mp_trajectory}} if nothing is simulated.
#' 
#' @name empty_trajectory
#' @format A data frame with zero rows and the following columns: `matrix`,
#' `time`, `row`, `col`, `value`.
#' @export
empty_trajectory = data.frame(
    matrix = character()
  , time = integer()
  , row = numeric()
  , col = numeric()
  , value = numeric()
)


is_empty_matrix = function(x) {
  if (!is.matrix(x)) return(FALSE)
  d = dim(x)
  (length(d) == 2L) & (all(d == 0L))
}

#' Parse Expression List
#'
#' Parse a list of one-sided formulas representing expressions
#' in a compartmental model. All parsed expressions in the
#' output list will share an environment.
#' this environment contains the same set of
#' functions, variables (aka matrices),
#' literals, methods, and offset. these components are key
#' to the definition of the model and therefore should be common for each
#' expression in the model.
#'
#' @param expr_list List of one-sided formulas.
#' @param valid_vars Named list of numerical matrices that can
#' be referred to in the formulas.
#' @param valid_literals An optional numeric vector of initial valid literals
#' from a related expression list. Additional literals in the expressions
#' themselves will be discovered and added to this list.
#' @param valid_methods \code{\link{MethList}} object.
#' @param valid_int_vecs \code{\link{IntVecs}} object.
#'
#' @noRd
parse_expr_list = function(expr_list
    , valid_vars
    , valid_literals = numeric(0L)
    , valid_methods = MethList()
    , valid_int_vecs = IntVecs()
  ) {
  offset = 0L
  eval_env = nlist(
    valid_funcs, valid_vars, valid_literals,
    valid_methods, valid_int_vecs, offset
  ) |> list2env()
  pe_list = list()
  for (i in seq_along(expr_list)) {

    ## all expressions should share an environment.
    ## this environment contains the same set of
    ## functions, variables (aka matrices),
    ## literals, methods, and offset. these components
    ## define the model
    environment(expr_list[[i]]) = eval_env

    ## do the parsing
    pe_list[[i]] = parse_expr(expr_list[[i]])

    ## grow valid literals as they are discovered in the expressions
    eval_env$valid_literals = pe_list[[i]]$valid_literals

    ## bump the row-index offset to prepare for the next expression
    ## to be parsed
    eval_env$offset = eval_env$offset + nrow(pe_list[[i]]$parse_table)
  }
  p_tables = lapply(pe_list, getElement, "parse_table")
  list(
    parse_table = do.call(rbind, p_tables),
    valid_literals = eval_env$valid_literals,
    num_p_table_rows = vapply(p_tables, nrow, integer(1L))
  )
}
#parse_expr_list = memoise(parse_expr_list)
