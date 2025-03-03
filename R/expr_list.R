ExprListUtils = function() {
  self = Base()
  self$.mat_names = function(...) {
    as.character(unlist(list(...)))
  }
  self$.init_valid_vars = function() {
    initial_valid_vars(names(self$init_mats))
  }
  self$.parsed_expr_list = function(
        .get_formula_side = rhs
      , .existing_literals = numeric(0L)
    ) {
    ## TODO: handle the null case
    (self$formula_list()
     |> lapply(.get_formula_side)
     |> parse_expr_list(
          valid_vars = self$.init_valid_vars()
        , valid_literals = .existing_literals
        , valid_methods = self$engine_methods$meth_list
        , valid_int_vecs = self$engine_methods$int_vecs
      )
    )
  }
  self$.set_name_prefix = function(x, prefix) {
    setNames(x, paste(prefix, names(x), sep = ""))
  }
  self$.does_assign = function(x) {
    raw_lhs = self$.lhs(x)
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
      self$.all_lhs(self$formula_list())
    )
    m = match(output_names, all_names)
    if (any(is.na(m))) {
      stop(
        "\nThe following updated variables are not "
      )
    }
    as.integer(m - 1L)
  }
  self$.init_table = function(parse_table, prefix) {
    if (is.null(parse_table)) {
      p_table = data.frame(x = integer(), n = integer(), i = integer())
    } else {
      p_table = parse_table[c("x", "n", "i")] |> as.list()
    }
    self$.set_name_prefix(p_table, prefix)
  }
  self$.p_table = function() {
    l = self$.parsed_expr_list(rhs)
    p_table = self$.init_table(l$parse_table, "p_table_")
  }
  self$.a_table = function() {
    l = self$.parsed_expr_list(lhs, self$.expr_literals())
    p_table = self$.init_table(l$parse_table, "a_table_")
  }
  self$.expr_literals = function() {
    self$.parsed_expr_list(rhs)$valid_literals
  }
  self$.literals = function() {
    self$.parsed_expr_list(lhs, self$.expr_literals())$valid_literals
  }
  self$.expr_num_p_table_rows = function() {
    self$.parsed_expr_list(rhs)$num_p_table_rows
  }
  self$.assign_num_a_table_rows = function() {
    self$.parsed_expr_list(lhs, self$.expr_literals())$num_p_table_rows
  }
  return_object(self, "ExprListUtils")
}

ExprList = function(
      before = list()
    , during = list()
    , after = list()
    , .simulate_exprs = character(0L)
  ) {
  self = ExprListUtils()
  valid_expr_list = AllValid(
    ValidityMessager(is.list, "not a list"),  ## list of ...
    ValidityMessager(MappedAllTest(Is("formula")), "not all formulas in an expression list"),  ## ... formulas that are ...
    ValidityMessager(TestPipeline(MappedSummarizer(length), MappedAllTest(TestRange(3L, 3L))), "not all formulas are two-sided"),  ## ... two-sided formula
    #TestPipeline(MappedSummarizer(lhs, is.symbol), MappedAllTest(TestTrue()))  ## ... only one symbol on the lhs
    
    ## TODO: fix this error message now that expressions can have formulas
    ## on the left-hand-side. should also make a new test that actually
    ## checks the requirements.
    .msg = paste("Invalid list of formulas.")
  )

  ## Args
  self$before = valid_expr_list$assert(before)
  self$during = valid_expr_list$assert(during)
  self$after = valid_expr_list$assert(after)
  self$.simulate_exprs = valid$char$assert(.simulate_exprs)

  self$formula_list = function() unname(c(self$before, self$during, self$after))
  self$expr_nms = function() {
    nms = names(c(self$before, self$during, self$after))
    if (is.null(nms)) nms = rep("", length(self$formula_list()))
    nms
  }
  self$all_formula_vars = function(side = c("both", "left", "right")) {
    (self$formula_list()
      |> lapply(formula_components, side)
      |> lapply(getElement, "variables")
      |> unlist(use.names = FALSE, recursive = FALSE)
      |> unique()
    )
  }
  self$all_default_vars = function() {
    setdiff(self$all_formula_vars(), self$all_derived_vars())
  }
  self$all_derived_vars = function() {
    all_vars = self$all_formula_vars()
    all_exprs = self$formula_list()
    lhs_comp_list = lapply(all_exprs, formula_components, "left")
    rhs_comp_list = lapply(all_exprs, formula_components, "right")
    lhs_list = lapply(lhs_comp_list, getElement, "variables")
    rhs_list = lapply(rhs_comp_list, getElement, "variables")
    lhs_func_list = lapply(lhs_comp_list, getElement, "functions")
    
    full_matrix_assign = lhs_func_list %in% list("~", c("~", "c"))
    
    is_var_derived = function(var_nm) {
      var_in_expr = vapply(lhs_list, \(expr_vars) var_nm %in% expr_vars, logical(1L))
      
      ## what expressions is the variable made (i.e. assigned) vs used?
      made = var_in_expr & full_matrix_assign
      used = vapply(rhs_list, `%in%`, logical(1L), x = var_nm)
      
      if (!any(made)) return(FALSE) # variables that are never made, must not be 'derived'
      if (!any(used)) return(TRUE)  # variables that are never used, must be 'derived'
      
      which(made)[1L] < which(used)[1L]
    }
    Filter(is_var_derived, all_vars)
  }

  self$data_arg = function() {
    r = c(
      list(
        expr_sim_block = as.integer(self$.expr_sim_block()),
        expr_num_p_table_rows = as.integer(self$.expr_num_p_table_rows()),
        assign_num_a_table_rows = as.integer(self$.assign_num_a_table_rows()),
        eval_schedule = as.integer(self$.eval_schedule())
      ),
      self$.p_table(),
      self$.a_table()
    )
    valid$expr_arg$assert(r)
  }
  self$delete = function(.at
    , .phase = c("before", "during", "after")
    , .simulate_exprs = character(0L)
  ) {
    .phase = match.arg(.phase)
    input = list(before = self$before, during = self$during, after = self$after)
    input[[.phase]][.at] = NULL
    input$.simulate_exprs = setdiff(self$.simulate_exprs, .simulate_exprs)
    do.call(ExprList, input)
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
  self$update = function(...
    , .at = 1L
    , .phase = c("before", "during", "after")
    , .simulate_exprs = character(0L)
  ) {
    .phase = match.arg(.phase)
    input = list(before = self$before, during = self$during, after = self$after)
    exprs = list(...)
    where = .at + seq_along(exprs) - 1L
    input[[.phase]][where] = exprs
    input$.simulate_exprs = unique(c(self$.simulate_exprs, .simulate_exprs))
    do.call(ExprList, input)
  }
  self$print_exprs = function(file = "", time_steps = "T") {
    model_steps_printer(
        self$formula_list()
      , self$.eval_schedule()
      , file
      , time_steps
    )
  }

  # Composition
  self$init_mats = MatsList()
  self$engine_methods = EngineMethods()

  return_object(self, "ExprList")
}

# @param steps_list list of model steps (e.g. a formula, a ChangeComponent).
# @param eval_schedule length-3 integer vector summing to the length of the
# steps_list, giving the number of steps in each of the three phases 
# (before, during, after).
# @param file optional file path to write to. default is to display on the
# screen.
# @param time_steps string used to represent the number of time steps in
# the during phase.
model_steps_printer = function(steps_list, eval_schedule, file = "", time_steps = "T") {
  if (sum(eval_schedule) == 0L) {
    lines = c(
        "---------------------",
        "No simulation steps",
        "---------------------"
      ) |> cat(file = file, sep = "\n", append = FALSE)
  } else {
    to = cumsum(eval_schedule)
    from = c(0L, to[1:2]) + 1L
    if (is.numeric(time_steps)) {
      time_steps_p1 = as.character(as.integer(time_steps + 1))
    } else {
      time_steps_p1 = paste(as.character(time_steps), "1", sep = " + ")
    }
    msgs = c(
      "Before the simulation loop (t = 0):",
      sprintf("At every iteration of the simulation loop (t = 1 to %s):"
        , as.character(time_steps)
      ),
      sprintf("After the simulation loop (t = %s):", time_steps_p1)
    )
  
    ## TODO: give better advice here on how to engage the simulation loop.
    if (time_steps == 0) {
      msgs[2L] = "At every iteration of the simulation loop (number of iterations = 0):"
    }
  
    for (i in 1:3) {
      if (eval_schedule[i] > 0L) {
        expr_strings = lapply(steps_list[from[i]:to[i]], to_string)
        tab_size = nchar(eval_schedule[i])
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
          , seq_len(eval_schedule[i])
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
mp_tmb_expr_list = ExprList

#' @export
print.ExprList = function(x, ...) x$print_exprs()
