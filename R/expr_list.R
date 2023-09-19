ExprListUtils = function() {
  self = Base()
  self$.mat_names = function(...) {
    as.character(unlist(list(...)))
  }
  self$.init_valid_vars = function() {
    initial_valid_vars(names(self$init_mats))
  }
  self$.parsed_expr_list = function(...
      , .existing_literals = numeric(0L)
      , .offset = 0L
    ) {
    parse_expr_list(self$.all_rhs(self$expr_list())
      , valid_vars = self$.init_valid_vars()
      , valid_literals = .existing_literals
      , offset = .offset
      , valid_methods = self$engine_methods$meth_list
    )
  }
  self$.set_name_prefix = function(x, prefix) {
    setNames(x, paste(prefix, names(x), sep = ""))
  }
  self$.does_assign = function(x) {
    raw_lhs = self$.lhs(x)
  }
  self$.lhs = function(x) {
    as.character(x[[2L]])
  }
  self$.rhs = function(x) {
    if (length(x) == 3L) e = x[c(1L, 3L)] else e = x
    e
  }
  self$.all_lhs = function(x) vapply(x, self$.lhs, character(1L))
  self$.all_rhs = function(x) lapply(x, self$.rhs)
  return_object(self, "ExprListUtils")
}

#' Expression List
#'
#' Create a list of expressions for defining a compartmental model in TMB.
#'
#' @param before List of formulas to be evaluated in the order provided before
#' the simulation loop begins. Each \code{\link{formula}} must have a left hand
#' side that gives the name of the matrix being updated, and a right hand side
#' giving an expression containing only the names of matrices in the model,
#' functions defined in \code{macpan2.cpp}, and numerical literals (e.g.
#' \code{3.14}). The available functions are described in
#' \code{\link{engine_functions}}. Names can be provided for the components of
#' \code{before}, and these names do not have to be unique.  These names are
#' used by the \code{.simulate_exprs} argument.
#' @param during List of formulas to be evaluated at every iteration of the
#' simulation loop, with the same rules as \code{before}.
#' @param after List of formulas to be evaluated after the simulation loop,
#' with the same rules as \code{before}.
#' @param .simulate_exprs Character vector of names of expressions to be
#' evaluated within TMB simulate blocks. This is useful when an expression
#' cannot be evaluated during the computation of the objective function and
#' its gradients (e.g. if the expression contains randomness or other
#' discontinuities that will break the automatic differentiation machinery
#' of TMB).
#'
#' @return Object of class \code{ExprList} with the following methods.
#'
#' ## Methods
#'
#' * `$data_arg(...)`: Return the following components of the data structure
#' to pass to C++.
#'     * `expr_output_id` -- Indices into the list of matrices identifying the
#'     matrix being produced.
#'     * `expr_sim_block` -- Identified whether or not the expression should be
#'     evaluated inside a simulate macro within TMB.
#'     * `expr_num_p_table_rows` -- Number of rows associated with each
#'     expression in the parse table (`p_table_*`)
#'     * `eval_schedule` -- Vector giving the number of expressions to evaluate
#'     in each phase (before, during, or after) of the simulation.
#'     * `p_table_x` -- Parse table column giving an index for looking up either
#'     function, matrix, or literal.
#'     * `p_table_n` -- Parse table column giving the number of arguments in
#'     functions.
#'     * `p_table_i` -- Parse table column giving indices for looking up the
#'     rows in the parse table corresponding with the first argument of the
#'     function.
#'
#' ## Method Arguments
#'
#' * `...`: Character vector containing the names of the matrices in the model.
#'
#'
#' @importFrom oor method_apply
#' @export
ExprList = function(
      before = list()
    , during = list()
    , after = list()
    , .simulate_exprs = character(0L)
  ) {
  self = ExprListUtils()
  lhs = function(x) x[[2L]]
  valid_expr_list = ValidityMessager(
    All(
      is.list,  ## list of ...
      MappedAllTest(Is("formula")),  ## ... formulas that are ...
      TestPipeline(MappedSummarizer(length), MappedAllTest(TestRange(3L, 3L))),  ## ... two-sided formula
      TestPipeline(MappedSummarizer(lhs, is.symbol), MappedAllTest(TestTrue()))  ## ... only one symbol on the lhs
    ),
    "Model expressions must be two-sided assignment formulas,",
    "without subsetting on the left-hand-side",
    "(i.e. x ~ 1 is fine, but x[0] ~ 1 is not)."
  )

  ## Args
  self$before = valid_expr_list$assert(before)
  self$during = valid_expr_list$assert(during)
  self$after = valid_expr_list$assert(after)
  self$.simulate_exprs = valid$char$assert(.simulate_exprs)

  self$expr_list = function() unname(c(self$before, self$during, self$after))
  self$expr_nms = function() {
    nms = names(c(self$before, self$during, self$after))
    if (is.null(nms)) nms = rep("", length(self$expr_list()))
    nms
  }

  self$.eval_schedule = function() {
    c(length(self$before), length(self$during), length(self$after))
  }

  self$.expr_sim_block = function() {
    as.integer(self$expr_nms() %in% self$.simulate_exprs)
  }

  self$.expr_output_id = function() {
    all_names = names(self$init_mats)
    output_names = valid$engine_outputs(all_names)$assert(
      self$.all_lhs(self$expr_list())
    )
    m = match(output_names, all_names)
    if (any(is.na(m))) {
      stop(
        "\nThe following updated variables are not "
      )
    }
    as.integer(m - 1L)
  }
  self$.expr_num_p_table_rows = function(...) {
    self$.parsed_expr_list(...)$num_p_table_rows
  }

  ## list of three equal length integer vectors
  ## p_table_x, p_table_n, p_table_i
  self$.parse_table = function(...) {
    l = as.list(self$.parsed_expr_list(...)$parse_table[c("x", "n", "i")])
    self$.set_name_prefix(l, "p_table_")
  }
  self$.literals = function(...) {
    self$.parsed_expr_list(...)$valid_literals
  }

  self$data_arg = function() {
    expr_output_id = self$.expr_output_id()
    r = c(
      list(
        expr_output_id = as.integer(expr_output_id),
        expr_sim_block = as.integer(self$.expr_sim_block()),
        expr_num_p_table_rows = as.integer(self$.expr_num_p_table_rows()),
        eval_schedule = as.integer(self$.eval_schedule())
      ),
      self$.parse_table()
    )
    valid$expr_arg$assert(r)
  }
  self$insert = function(...
    , .at = 1L
    , .phase = c("before", "during", "after")
    , .simulate_exprs = character(0L)
  ) {
    .phase = match.arg(.phase)
    input = list(before = self$before, during = self$during, after = self$after)
    input[[.phase]] = append(input[[.phase]], list(...), after = .at - 1L)
    input$.simulate_exprs = unique(c(self$.simulate_exprs, .simulate_exprs))
    do.call(ExprList, input)
  }
  self$print_exprs = function(file = "") {
    to = cumsum(self$.eval_schedule())
    from = c(0L, to[1:2]) + 1L
    msgs = c(
      "Before the simulation loop (t = 0):",
      "At every iteration of the simulation loop (t = 1 to T):",
      "After the simulation loop (t = T):"
    )
    for (i in 1:3) {
      if (self$.eval_schedule()[i] > 0L) {
        expr_strings = lapply(self$expr_list()[from[i]:to[i]], deparse)
        tab_size = nchar(self$.eval_schedule()[i])
        fmt = sprintf("%%%ii: %%s", tab_size)
        tab = paste0(rep(" ", tab_size), collapse = "")
        expr_n_lines = vapply(expr_strings, length, integer(1L))
        make_expr_numbers = function(s, i) {
          s[1L] = sprintf(fmt, i, s[1L])
          if (length(s) > 1L) {
            s[-1L] = paste(tab, s[-1L], sep = "")
          }
          s
        }
        expr_char = unlist(mapply(make_expr_numbers
          , expr_strings
          , seq_len(self$.eval_schedule()[i])
          , SIMPLIFY = FALSE
          , USE.NAMES = FALSE
        ))
        lines = c(
          "---------------------",
          msgs[i],
          "---------------------",
          expr_char,
          ""
        )
        cat(lines, file = file, sep = "\n", append = i != 1L)
      }
    }
  }

  # Composition
  self$init_mats = MatsList()
  self$engine_methods = EngineMethods()

  return_object(self, "ExprList")
}
