ExprListUtils = function() {
  self = Base()
  self$.lhs_string = function(x) as.character(x[[2L]])
  self$.rhs = function(x, mat_names) {
    if (length(x) == 3L) e = x[c(1L, 3L)] else e = x
    environment(e) = list2env(list(
      valid_vars = initial_valid_vars(mat_names),
      valid_funcs = valid_funcs
    ))
    e
  }
  self$.all_lhs_strings = function(x) vapply(x, self$.lhs_string, character(1L))
  self$.all_rhs = function(x, mat_names) {
    lapply(x, self$.rhs, mat_names)
  }
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
#' @export
ExprList = function(before = list(), during = list(), after = list(), .simulate_exprs = character(0L)) {
  self = ExprListUtils()
  self$.expr_list = c(before, during, after)
  expr_nms = names(self$.expr_list)
  if (is.null(expr_nms)) expr_nms = rep("", length(self$.expr_list))
  self$.expr_nms = expr_nms
  self$.expr_list = unname(self$.expr_list)
  self$.simulate_exprs = .simulate_exprs
  self$.eval_schedule = c(length(before), length(during), length(after))
  self$.expr_sim_block = expr_nms %in% .simulate_exprs
  self$.expr_output_id = function(...) {
    mat_names = as.character(unlist(list(...)))
    as.integer(match(self$.all_lhs_strings(self$.expr_list), mat_names) - 1L)
  }
  self$.expr_num_p_table_rows = function(...) {
    vapply(
      self$.parse_tables(...),
      nrow,
      integer(1L),
      USE.NAMES = FALSE
    )
  }
  self$.expr_output_count = function() {
    rep(1L, length(self$.expr_list))  ## deprecated
  }
  self$.unparsed_expr_list = function(...) {
    mat_names = as.character(unlist(list(...)))
    self$.all_rhs(self$.expr_list, mat_names)
  }
  self$.raw_parse_tables = function(...) {
    lapply(self$.unparsed_expr_list(...), parse_expr)
  }
  self$.parse_tables = function(...) {
    lapply(self$.raw_parse_tables(...), getElement, "parse_table")
  }
  self$.literals_list = function(...) {
    lapply(self$.raw_parse_tables(...), getElement, "valid_literals")
  }
  self$.parsed_expr_obj = function(...) {
    TMBExpressions(
      self$.raw_parse_tables(...),
      initial_valid_vars(as.character(unlist(list(...)))),
      self$.literals_list(...)
    )
  }
  self$.parse_table = function(...) {
    self$.parsed_expr_obj(...)$parse_table
  }
  self$.literals = function(...) {
    unlist(self$.literals_list(...))
  }
  self$data_arg = function(...) {
    c(
      list(
        expr_output_count = self$.expr_output_count(),
        expr_output_id = self$.expr_output_id(...),
        expr_sim_block = self$.expr_sim_block,
        expr_num_p_table_rows = self$.expr_num_p_table_rows(...),
        eval_schedule = self$.eval_schedule
      ),
      self$.parse_table(...)
    )
  }
  return_object(self, "ExprList")
}

#' Matrix List
#'
#' Create a list of initial values for matrices used to define a compartmental
#' model in TMB.
#'
#' @param ... Named objects that can be coerced to numerical matrices.
#' @param .mats_to_save Character vector naming matrices to be saved at each
#' set in the simulation so that some calculations can make use of past value
#' (e.g. delayed effects) and/or to be able to retrieved the simulation
#' history after the simulation is complete.
#' @param .mats_to_return Character vector naming matrices to be returned
#' after the simulate is complete.
#'
#' @return Object of class \code{MatsList} with the following methods.
#'
#' ## Methods
#'
#' * `$data_arg()`: Return the following components of the data structure
#' to pass to C++.
#'     * `mats` -- Unnamed list of numeric matrices.
#'     * `mats_save_hist` -- Boolean vector identifying which matrices should
#'     have their history saved.
#'     * `mats_return` -- Boolean vector identifying which matrices should be
#'     returned after a simulation.
#'
#' @export
MatsList = function(..., .mats_to_save = character(0L), .mats_to_return = character(0L)) {
  self = Base()
  self$.initial_mats = lapply(list(...), as.matrix)
  self$.mats_save_hist = names(self$.initial_mats) %in% .mats_to_save
  self$.mats_return = names(self$.initial_mats) %in% .mats_to_return
  self$.names = function() names(self$.initial_mats)
  self$.mats = function() unname(self$.initial_mats)
  self$data_arg = function() {
    list(
      mats = self$.mats(),
      mats_save_hist = self$.mats_save_hist,
      mats_return = self$.mats_return
    )
  }
  return_object(self, "MatsList")
}


#' Optimization Parameters List
#'
#' Create an object for specifying matrix elements to be optimized or integrated
#' out of the objective function using a Laplace transform.
#'
#' @param ... Objects that can be coerced to numeric vectors, which will be
#' concatenated to produce the parameter vector.
#' @param par_id Integer vector identifying elements of the parameter vector
#' to be used to replace elements of the model matrices.
#' @param mat Character vector the same length as `par_id` giving the names of
#' the matrices containing the elements to replace.
#' @param row_id Integer vector the same length as `par_id` giving the row
#' indices of the matrix elements to replace with parameter values.
#' @param col_id Integer vector the same length as `par_id` giving the column
#' indices of the matrix elements to replace with parameter values.
#'
#' @return Object of class \code{OptParamsList} with the following methods.
#'
#' ## Methods
#'
#' * `$data_arg(..., .type_string = c("p", "r"))`: Return the following components of the data structure
#' to pass to C++.
#'     * `{.type_string}_par_id` -- Integers identifying the replacing parameter.
#'     * `{.type_string}_mat_id` -- Integers identifying the matrix within which
#'     an element is to be replaced.
#'     * `{.type_string}_row_id` -- Integers identifying the rows within matrices
#'     to replace.
#'     * `{.type_string}_col_id` -- Integers identifying the columns within
#'     matrices to replace.
#' * `$vector()`: Return the initial value of the numerical parameter vector.
#'
#' ## Method Arguments
#'
#' * `...`: Character vector containing the names of the matrices in the model.
#' * `.type_string`: Either `"p"` or `"r"` indicating whether the object
#' is to be used to represent fixed parameters to be optimized or random
#' parameters to be integrated out using the Laplace transform.
#'
#' @export
OptParamsList = function(..., par_id = integer(0L), mat = character(0L), row_id = integer(0L), col_id = integer(0L)) {
  self = Base()
  self$.vector = as.numeric(unlist(list(...)))
  self$vector = function() self$.vector

  # TMB needs at least one parameter (unless this is for a random effect),
  # so setting to zero without setting anything else so that it doesn't
  # actually get used
  #if (length(self$.vector) == 0L) self$.vector = 0
  self$.mat = mat
  self$.par_id = par_id
  self$.row_id = row_id
  self$.col_id = col_id
  self$.mat_id = function(...) {
    match(self$.mat, as.character(unlist(list(...)))) - 1L
  }
  self$data_arg = function(..., .type_string = c("p", "r")) {
    .type_string = match.arg(.type_string)
    setNames(
      list(self$.par_id, self$.mat_id(...), self$.row_id, self$.col_id),
      paste(.type_string, c("par", "mat", "row", "col"), "id", sep = "_")
    )
  }
  return_object(self, "OptParamsList")
}

#' Objective Function
#'
#' Define the objective function of a compartmental model in TMB.
#'
#' @param obj_fn_expr One sided \code{\link{formula}} giving the objective
#' function of a TMB model. The right hand side expression must contain only
#' the names of matrices in the model, functions defined in \code{macpan2.cpp},
#' and numerical literals (e.g. \code{3.14}).
#'
#' @return Object of class \code{ObjectiveFunction} with the following methods.
#'
#' ## Methods
#'
#' * `data_arg(..., .existing_literals)` -- Return the following components of the data structure
#' to pass to C++.
#'     * `o_table_x` -- Objective function parse table column giving an index for looking up either
#'     function, matrix, or literal.
#'     * `o_table_n` -- Objective function parse table column giving the number of arguments in
#'     functions.
#'     * `o_table_i` -- Objective function parse table column giving indices for looking up the
#'     rows in the parse table corresponding with the first argument of the
#'     function.
#'     * `literals` -- Numeric vector of literals that can were used in the
#'     expressions of the model.
#'
#' ## Method Arguments
#'
#' * `...`: Character vector containing the names of the matrices in the model.
#' * `.existing_literals`: Numeric vector giving the literals used in the
#' model expressions produced before the objective function.
#'
#' @export
ObjectiveFunction = function(obj_fn_expr) {
  self = ExprListUtils()
  self$.obj_fn_expr = obj_fn_expr
  self$.raw_parse_table = function(...) {
    mat_names = as.character(unlist(list(...)))
    parse_expr(self$.rhs(self$.obj_fn_expr, mat_names))
  }
  self$.obj_fn_obj = function(..., .existing_literals) {
    TMBObjectiveFunction(self$.raw_parse_table(...)$parse_table
      , self$.raw_parse_table(...)$.valid_literals
      , .existing_literals
    )
  }
  self$.parse_table = function(..., .existing_literals) {
    self$.obj_fn_obj(..., .existing_literals = .existing_literals)$parse_table
  }
  self$.literals = function(..., .existing_literals) {
    self$.obj_fn_obj(..., .existing_literals = .existing_literals)$literals
  }
  self$data_arg = function(..., .existing_literals) {
    p = self$.parse_table(..., .existing_literals = .existing_literals)
    list(
      literals = self$.literals(..., .existing_literals = .existing_literals),
      o_table_n = p$n,
      o_table_x = p$x,
      o_table_i = p$i
    )
  }
  return_object(self, "ObjectiveFunction")
}

#' Time
#'
#' Define the number of time steps in a compartmental model in TMB.
#'
#' @param time_steps Number of time steps in the simulation loop.
#'
#' @return Object of class \code{Time} with the following methods.
#'
#' ## Methods
#'
#' * `$data_arg()` -- Return the following components of the data structure
#' to pass to C++.
#'     * `time_steps` -- Number of time steps in the simulation loop.
#'
#' @export
Time = function(time_steps) {
  self = Base()
  self$.time_steps = time_steps
  self$data_arg = function() list(time_steps = self$.time_steps)
  return_object(self, "Time")
}

#' TMB Model
#'
#' Define a compartmental model in TMB. This model uses the spec
#' \url{https://canmod.net/misc/cpp_side}.
#'
#' @param init_mats An object of class \code{\link{MatsList}}.
#' @param expr_list An object of class \code{\link{ExprList}}.
#' @param params An object of class \code{\link{OptParamsList}}.
#' @param random An object of class \code{\link{OptParamsList}}.
#' @param obj_fn An object of class \code{\link{ObjectiveFunction}}.
#' @param time_steps An object of class \code{\link{Time}}.
#'
#' @return Object of class \code{TMBModel} with the following methods.
#'
#' ## Methods
#'
#' * `$data_arg()` -- Return all of the components of the data structure
#' to pass to C++.
#' * `$param_arg()` -- Return all of the components of the parameter list
#' to pass to C++.
#'
#' @examples
#' sir = TMBModel(
#'   init_mats = MatsList(
#'     state = c(1 - 1e-2, 1e-2, 0),
#'     beta = 0.3,
#'     gamma = 0.2,
#'     N = 1,
#'     foi = 0,
#'     ratemat = matrix(0, 3, 3),
#'     flowmat = matrix(0, 3, 3),
#'     .mats_to_save = c("state", "N", "foi"),
#'     .mats_to_return = c("state", "N", "foi")
#'   ),
#'   expr_list = ExprList(
#'     before = list(
#'       N ~ sum(state)
#'     ),
#'     during = list(
#'       foi ~ beta * state[1, 0] / N,
#'       ratemat ~ matrix(c(
#'         0,   0,     0,
#'         foi, 0,     0,
#'         0,   gamma, 0), 3, 3),
#'       flowmat ~ ratemat * state,
#'       state ~ state - rowSums(flowmat) + t(colSums(flowmat))
#'     )
#'   ),
#'   params = OptParamsList(0.3
#'     , par_id = 0L
#'     , mat = "beta"
#'     , row_id = 0L
#'     , col_id = 0L
#'   ),
#'   random = OptParamsList(),
#'   obj_fn = ObjectiveFunction(~ foi + 1),
#'   time_steps = Time(time_steps = 30L)
#' )
#' sir$data_arg()
#' sir$param_arg()
#'
#' @export
TMBModel = function(init_mats, expr_list, params, random, obj_fn, time_steps) {
  self = Base()
  self$.expr_list = expr_list
  self$.init_mats = init_mats
  self$.params = params
  self$.random = random
  self$.obj_fn = obj_fn
  self$.time_steps = time_steps
  self$data_arg = function() {
    existing_literals = self$.expr_list$.literals(self$.init_mats$.names())
    c(
      self$.init_mats$data_arg(),
      self$.expr_list$data_arg(self$.init_mats$.names()),
      self$.params$data_arg(self$.init_mats$.names()),
      self$.random$data_arg(self$.init_mats$.names(), .type_string = "r"),
      self$.obj_fn$data_arg(self$.init_mats$.names()
        , .existing_literals = existing_literals
      ),
      self$.time_steps$data_arg()

    )
  }
  self$param_arg = function() {
    p = list(
      params = self$.params$vector(),
      random = self$.random$vector()
    )
    if (length(p$params) == 0L) p$params = 0
    p
  }
  self$random_arg = function() {
    if (length(self$.random$vector()) == 0L) {
      return(NULL)
    }
    return("random")
  }
  return_object(self, "TMBModel")
}
