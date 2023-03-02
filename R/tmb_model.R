ExprListUtils = function() {
  self = Base()
  self$.mat_names = function(...) {
    as.character(unlist(list(...)))
  }
  self$.init_valid_vars = function(...) {
    initial_valid_vars(self$.mat_names(...))
  }
  self$.parsed_expr_list = function(...
      , .existing_literals = numeric(0L)
      , .offset = 0L
    ) {
    parse_expr_list(self$.all_rhs(self$.expr_list)
      , valid_vars = self$.init_valid_vars(...)
      , valid_literals = .existing_literals
      , offset = .offset
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
#' @export
ExprList = function(
      before = list()
    , during = list()
    , after = list()
    , .simulate_exprs = character(0L)
  ) {
  self = ExprListUtils()
  valid_expr_list = ValidityMessager(
    All(
      is.list,  ## list of ...
      MappedAllTest(Is("formula")),  ## ... formulas that are ...
      TestPipeline(MappedSummarizer(length), MappedAllTest(TestRange(3L, 3L)))  ## ... two-sided
    ),
    "Model expressions must be two-sided formulas"
  )
  self$.expr_list = c(
    valid_expr_list$assert(before),
    valid_expr_list$assert(during),
    valid_expr_list$assert(after)
  )
  expr_nms = names(self$.expr_list)
  if (is.null(expr_nms)) expr_nms = rep("", length(self$.expr_list))
  self$.expr_nms = expr_nms
  self$.expr_list = unname(self$.expr_list)
  self$.simulate_exprs = valid$char$assert(.simulate_exprs)
  self$.eval_schedule = c(length(before), length(during), length(after))
  self$.expr_sim_block = as.integer(expr_nms %in% .simulate_exprs)
  self$.expr_output_id = function(...) {
    m = match(
      self$.all_lhs(self$.expr_list),
      self$.mat_names(...)
    )
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

  self$data_arg = function(...) {
    r = c(
      list(
        expr_output_id = as.integer(self$.expr_output_id(...)),
        expr_sim_block = as.integer(self$.expr_sim_block),
        expr_num_p_table_rows = as.integer(self$.expr_num_p_table_rows(...)),
        eval_schedule = as.integer(self$.eval_schedule)
      ),
      self$.parse_table(...)
    )
    valid$expr_arg$assert(r)
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
#' @param .dimnames No longer used. Set dimnames by adding dimnames to the
#' matrices passed to \code{...}.
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
MatsList = function(...
    , .mats_to_save = character(0L)
    , .mats_to_return = character(0L)
    , .dimnames = list()
  ) {
  self = Base()
  self$.initial_mats = lapply(list(...), as.matrix)
  self$.mats_save_hist = names(self$.initial_mats) %in% .mats_to_save
  self$.mats_return = names(self$.initial_mats) %in% .mats_to_return
  self$.names = function() names(self$.initial_mats)
  self$.mats = function() unname(self$.initial_mats)
  dimnames_handle_nulls = function(x) {
    if (is.null(dimnames(x))) return(NULL)
    if (is.null(rownames(x))) rownames(x) = ""
    if (is.null(colnames(x))) colnames(x) = ""
    dimnames(x)
  }
  not_null = function(x) !is.null(x)
  self$.dimnames = Filter(not_null
    , lapply(self$.initial_mats, dimnames_handle_nulls)
  )
  self$data_arg = function() {
    r = list(
      mats = self$.mats(),
      mats_save_hist = self$.mats_save_hist,
      mats_return = self$.mats_return
    )
    valid$mats_arg$assert(r)
  }
  return_object(self, "MatsList")
}

#' @export
names.MatsList = function(x) x$.names()


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
OptParamsList = function(...
    , par_id = integer(0L)
    , mat = character(0L)
    , row_id = integer(0L)
    , col_id = integer(0L)
  ) {
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
    r = setNames(
      list(self$.par_id, self$.mat_id(...), self$.row_id, self$.col_id),
      paste(.type_string, c("par", "mat", "row", "col"), "id", sep = "_")
    )
    valid$opt_params_list_arg$assert(r)
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
  self$.expr_list = list(obj_fn_expr)
  self$.literals = function(..., .existing_literals) {
    self$.parsed_expr_list(..., .existing_literals = .existing_literals)$valid_literals
  }
  self$.parse_table = function(..., .existing_literals) {
    l = as.list(self$.parsed_expr_list(..., .existing_literals = .existing_literals)$parse_table)
    self$.set_name_prefix(l[c("x", "n", "i")], "o_table_")
  }
  self$data_arg = function(..., .existing_literals) {
    p = self$.parse_table(..., .existing_literals = .existing_literals)
    p$literals = self$.literals(..., .existing_literals = .existing_literals)
    p
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
#' @useDynLib macpan2
#' @importFrom TMB MakeADFun
#' @export
TMBModel = function(
    init_mats = MatsList(),
    expr_list = ExprList(),
    params = OptParamsList(0),
    random = OptParamsList(),
    obj_fn = ObjectiveFunction(~0),
    time_steps = Time(0L)
  ) {
  self = Base()
  self$.expr_list = expr_list
  self$.init_mats = init_mats
  self$.params = params
  self$.random = random
  self$.obj_fn = obj_fn
  self$.time_steps = time_steps
  self$data_arg = function() {
    existing_literals = self$.expr_list$.literals(self$.init_mats$.names())
    expr_list = self$.expr_list$data_arg(self$.init_mats$.names())
    c(
      self$.init_mats$data_arg(),
      expr_list,
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
  self$make_ad_fun = function(DLL = "macpan2") {
    try(TMB::MakeADFun(
      data = self$data_arg(),
      parameters = self$param_arg(),
      random = self$random_arg(),
      DLL = DLL
    ))
  }
  return_object(self, "TMBModel")
}

#' TMB Simulator
#'
#' Construct an object with methods fore simulating from and optimizing a
#' compartmental model made using \code{\link{TMBModel}}.
#'
#' @param tmb_model An object of class \code{\link{TMBModel}}.
#' @param tmb_cpp Name of a C++ program using TMB as the simulation engine.
#'
#' @return Object of class \code{TMBSimulator} with the following methods.
#'
#' ## Methods
#'
#' * `$ad_fun()` -- TODO
#' * `$report()` -- TODO
#'
#' @export
TMBSimulator = function(tmb_model, tmb_cpp = "macpan2") {
  self = Base()
  self$tmb_model = tmb_model
  self$tmb_cpp = tmb_cpp
  self$matrix_names = self$tmb_model$.init_mats$.names()
  self$ad_fun = self$tmb_model$make_ad_fun(self$tmb_cpp)
  self$report = function(..., .phases = c("before", "during", "after")) {
    fixed_params = as.numeric(unlist(list(...)))
    r = setNames(
      as.data.frame(self$ad_fun$report(fixed_params)$values),
      c("matrix", "time", "row", "col", "value")
    )  ## get raw simulation output from TMB and supply column names (which don't exist on the TMB side)
    r$matrix = self$matrix_names[r$matrix + 1L]  ## replace matrix indices with matrix names
    dn = self$tmb_model$.init_mats$.dimnames ## get the row and column names of matrices with such names
    for (mat in names(dn)) {
      i = r$matrix == mat
      r[i,"row"] = dn[[mat]][[1L]][as.integer(r[i,"row"]) + 1L]
      r[i,"col"] = dn[[mat]][[2L]][as.integer(r[i,"col"]) + 1L]
    }
    r$time = as.integer(r$time)
    num_t = self$tmb_model$.time_steps$.time_steps
    if (!"before" %in% .phases) {
      r = r[r$time != 0L,,drop = FALSE]
    }
    if (!"during" %in% .phases) {
      r = r[(r$time < 1L) | (r$time > num_t),,drop = FALSE]
    }
    if (!"after" %in% .phases) {
      r = r[r$time < num_t + 1,,drop = FALSE]
    }
    r
  }
  self$matrix = function(..., matrix_name, time_step) {
    r = self$report(...)
    i = (r$matrix == as.character(matrix_name)) & (r$time == as.integer(time_step))
    rr = r[i, c("row", "value")]
    matrix(rr$value, max(rr$row) + 1L)
  }
  return_object(self, "TMBSimulator")
}
