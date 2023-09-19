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
    parse_expr_list(self$.all_rhs(self$expr_list())
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

  self$.expr_output_id = function(...) {
    all_names = self$.mat_names(...)
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

  self$data_arg = function(...) {
    expr_output_id = self$.expr_output_id(...)
    r = c(
      list(
        expr_output_id = as.integer(expr_output_id),
        expr_sim_block = as.integer(self$.expr_sim_block()),
        expr_num_p_table_rows = as.integer(self$.expr_num_p_table_rows(...)),
        eval_schedule = as.integer(self$.eval_schedule())
      ),
      self$.parse_table(...)
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
#' @param .dimnames Named list of \code{\link{dimnames}} for matrices that change
#' their dimensions over the simulation steps. These names correspond to the
#' names of the matrices. The output of the simulations will try their best
#' to honor these names, but if the shape of the matrix is too inconsistent
#' with the \code{\link{dimnames}} then numerical indices will be used instead.
#' For matrices that do not change their dimensions, set \code{\link{dimnames}}
#' by adding \code{\link{dimnames}} to the matrices passed to \code{...}.
#' @param .structure_labels An optional object for obtaining labels of
#' elements of special vectors and matrices. Such an object can be found in
#' the `$labels` field of a \code{\link{Compartmental}} model. Note that this
#' is an advanced technique.
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
#' * `$mat_dims()`: Return a data frame giving the numbers of rows and columns
#' of each matrix in the list.
#' * `$add_mats(...)`: Add matrices to the list and return a new
#' regenerated \code{MatsList} object.
#'
#' @export
MatsList = function(...
    , .mats_to_save = character(0L)
    , .mats_to_return = character(0L)
    , .dimnames = list()
    , .structure_labels = NullLabels()
    #, .init_saved_dims = list()
  ) {
  # self = EditableArgs(MatsList
  #   , lapply(list(...), as.matrix)
  #   , list()
  # )
  self = Base()

  ## Args -- TODO: these shouldn't be private
  self$.initial_mats = lapply(list(...), as.matrix)
  self$.mats_to_save = .mats_to_save
  self$.mats_to_return = .mats_to_return
  self$.dimnames = .dimnames
  self$.structure_labels = .structure_labels
  #self$.init_saved_dims = .init_saved_dims

  self$mats_save_hist = function() names(self$.initial_mats) %in% self$.mats_to_save
  self$mats_return = function() names(self$.initial_mats) %in% self$.mats_to_return
  # self$mats_save_dims = function() {
  #   msh = self$mats_save_hist()
  #   msd = setNames(
  #     lapply(self$.initial_mats[msh], dim),
  #     self$.names()[msh]
  #   )
  #   for (m in names(self$.init_saved_dims)) {
  #
  #   }
  # }

  ## Standard methods
  self$get = function(variable_name) {
    i = which(self$.names() == valid$char1$assert(variable_name))
    if (length(i) == 1L) {
      return(self$.mats()[[i]])
    } else {
      i = which(variable_name == self$.structure_labels$state())
      if (length(i) == 1L) {
        return(self$get("state")[i])
      }
      i = which(variable_name == self$.structure_labels$flow())
      if (length(i) == 1L) {
        return(self$get("flow")[i])
      }
    }
    stop(
      "\nNo variable called ", variable_name, " in the list:\n",
      paste0(
        c(
          self$.names(),
          self$.structure_labels$state(),
          self$.structure_labels$flow()
        ),
        collapse = "; "
      )
    )
  }
  self$.names = function() names(self$.initial_mats)
  self$.mats = function() unname(self$.initial_mats)
  dimnames_handle_nulls = function(x) {
    if (is.null(dimnames(x))) return(NULL)
    if (is.null(rownames(x))) rownames(x) = ""
    if (is.null(colnames(x))) colnames(x) = ""
    dimnames(x)
  }
  not_null = function(x) !is.null(x)
  dn = lapply(self$.initial_mats, dimnames_handle_nulls)
  for (mat_nm in names(.dimnames)) dn[[mat_nm]] = .dimnames[[mat_nm]]
  self$.dimnames = Filter(not_null, dn)

  self$.dim = setNames(
    lapply(self$.mats(), dim),
    self$.names()
  )
  self$.nrow = vapply(self$.mats(), nrow, integer(1L), USE.NAMES = FALSE)
  self$.ncol = vapply(self$.mats(), ncol, integer(1L), USE.NAMES = FALSE)
  self$mat_dims = function() {
    data.frame(mat = self$.names(), nrow = self$.nrow, ncol = self$.ncol)
  }
  self$data_arg = function() {
    r = list(
      mats = self$.mats(),
      mats_save_hist = self$mats_save_hist(),
      mats_return = self$mats_return()
    )
    valid$mats_arg$assert(r)
  }

  ## add _new_ matrices -- error if a matrix with the
  ## same name already exists
  self$add_mats = function(...
    , .mats_to_save = character(0L)
    , .mats_to_return = character(0L)
    , .dimnames = list()
  ) {
    args = c(self$.initial_mats, list(...))
    dups = duplicated(names(args))
    if (any(dups)) {
      stop(
        "\nThe following matrices were added, but already existed:\n",
        paste0(names(args)[dups], collapse = ", ")
        ## TODO: fill in what to do about it
      )
    }
    args$.mats_to_save = union(self$.mats_to_save, .mats_to_save)
    args$.mats_to_return = union(self$.mats_to_return, .mats_to_return)
    args$.dimnames = c(self$.dimnames, .dimnames)
    args$.structure_labels = self$.structure_labels
    do.call(MatsList, args)
  }

  ## add new matrices or update existing matrices as the
  ## case may be -- no error or warning if you happen to
  ## overwrite an existing matrix
  self$update_mats = function(...
    , .mats_to_save = character(0L)
    , .mats_to_return = character(0L)
    , .dimnames = list()
  ) {
    args = self$.initial_mats
    new_args = list(...)
    args[names(new_args)] = new_args
    args$.mats_to_save = union(self$.mats_to_save, .mats_to_save)
    args$.mats_to_return = union(self$.mats_to_return, .mats_to_return)
    args$.dimnames = self$.dimnames
    args$.dimnames[names(.dimnames)] = .dimnames
    args$.structure_labels = self$.structure_labels
    do.call(MatsList, args)
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
#' concatenated to produce the default value of the parameter vector.
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
#' * `$data_frame()`: Return a data frame with each row describing a parameter.
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
  self$data_frame = function(...) {
    d = data.frame(par_id = self$.par_id
      , mat = self$.mat
      , row = self$.row_id
      , col = self$.col_id
      , default = self$.vector[self$.par_id + 1L]
    )
    alternative_vectors = list(...)
    for (v in names(alternative_vectors)) {
      d[[v]] = alternative_vectors[[v]][self$.par_id + 1L]
    }
    d
  }
  self$data_arg = function(..., .type_string = c("p", "r")) {
    .type_string = match.arg(.type_string)
    r = setNames(
      list(self$.par_id
        , self$.mat_id(...)
        , self$.row_id
        , self$.col_id
      ),
      paste(.type_string, c("par", "mat", "row", "col"), "id", sep = "_")
    )
    valid$opt_params_list_arg$assert(r)
  }
  return_object(self, "OptParamsList")
}

OptParamsFrameStruc = function(..., frame) {
  OptParamsList(...
    , par_id = frame$par_id
    , mat = frame$mat
    , row_id = frame$row_id
    , col_id = frame$col_id
  )
}


## alternative constructor of OptParamsList
OptParamsFrame = function(frame, .dimnames = list()) {
  for (c in names(frame)) {
    if (is.character(frame[[c]])) frame[[c]] = trimws(frame[[c]])
  }
  row_col_ids = make_row_col_ids(frame$mat, frame$row, frame$col, .dimnames)
  args = c(
    as.list(as.numeric(frame$default)),
    list(
      par_id = seq_len(nrow(frame)) - 1L,  ## zero-based c++ indices
      mat = frame$mat,
      row_id = row_col_ids$row_id,
      col_id = row_col_ids$col_id
    )
  )
  do.call(OptParamsList, args)
}

OptParamsFile = function(file_path
      , csv_reader = CSVReader
      , json_reader = JSONReader
      , txt_reader = TXTReader
    ) {
  self = Files(dirname(file_path)
    , reader_spec(basename(file_path), csv_reader)
  )
  self$.col_map = c(
      Matrix = "mat"
    , mat = "mat"
    , Mat = "mat"
    , Row = "row"
    , row = "row"
    , Column = "col"
    , Col = "col"
    , col = "col"
    , type = "type"
    , Type = "type"
    , value = "default"
    , Value = "default"
    , Val = "default"
    , val = "default"
    , Default = "default"
    , default = "default"
  )
  self$.param = c("param", "par", "fixed", "fixef")
  self$.random = c("random", "ran", "rand", "ranef")
  self$frame = function() {
    x = self$get("parameters")
    names(x) = self$.col_map[names(x)]
    x
  }
  self$params_frame = function(.dimnames = list()) {
    f = self$frame()
    f[tolower(f[["type"]]) %in% self$.param, , drop = FALSE]
    #if (nrow(f) == 0L) return(OptParamsList())
  }
  self$random_frame = function(.dimnames = list()) {
    f = self$frame()
    f[tolower(f[["type"]]) %in% self$.random, , drop = FALSE]
    #if (nrow(f) == 0L) return(OptParamsList())
  }
  self
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

  ## Inherit Private Methods
  self = ExprListUtils()

  ## Args
  self$obj_fn_expr = obj_fn_expr

  ## Standard Methods
  self$expr_list = function() list(self$obj_fn_expr)
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
  self$time_steps = time_steps
  self$data_arg = function() list(time_steps = self$time_steps)
  return_object(self, "Time")
}

DiffTime = function(start_time, end_time) {
  self = Base()
  self$start_time = valid$scalar$assert(start_time)
  self$end_time = valid$scalar$assert(end_time)
  self$start = function() {}
}

Daily = function(start_date, end_date) {
  self = Base()
  self$start_date = as.Date(start_date)
  self$end_date = as.Date(end_date)
  self$time_steps = function() {
    (self$end_date
     |> difftime(self$start_date, units = "days")
     |> as.integer()
    )
  }
  self$data_arg = function() list(list_steps = self$time_steps())
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
#' @param do_pred_sdreport A logical flag (\code{FALSE}/\code{TRUE}, or any value evaluating to 1 for \code{TRUE}) indicating whether predicted values should be accessible via \code{TMB::sdreport()}
#'
#' @return Object of class \code{TMBModel} with the following methods.
#'
#' ## Methods
#'
#' * `$data_arg()` -- Return all of the components of the data structure
#' to pass to C++.
#' * `$param_arg()` -- Return all of the components of the parameter list
#' to pass to C++.
#' * `$simulator()` -- Return an object of class \code{\link{TMBSimulator}},
#' which can be used to simulate data from the model.
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
    time_steps = Time(0L),
    do_pred_sdreport = TRUE
  ) {
  ## Inheritance
  self = Base()

  ## Args
  self$expr_list = expr_list
  self$init_mats = init_mats
  self$params = params
  self$random = random
  self$obj_fn = obj_fn
  self$time_steps = time_steps
  self$do_pred_sdreport = do_pred_sdreport

  ## Standard Methods
  self$data_arg = function() {
   existing_literals = self$expr_list$.literals(self$init_mats$.names())
   expr_list = self$expr_list$data_arg(self$init_mats$.names())
    c(
      self$init_mats$data_arg(),
      expr_list,
      self$params$data_arg(self$init_mats$.names()),
      self$random$data_arg(self$init_mats$.names(), .type_string = "r"),
      self$obj_fn$data_arg(self$init_mats$.names()
        , .existing_literals = existing_literals
      ),
      self$time_steps$data_arg(),
      list(values_adreport = as.integer(self$do_pred_sdreport))
    )
  }
  self$param_arg = function() {
    p = list(
      params = self$params$vector(),
      random = self$random$vector()
    )
    if (length(p$params) == 0L) p$params = 0
    p
  }
  self$random_arg = function() {
    if (length(self$random$vector()) == 0L) return(NULL)
    return("random")
  }
  self$make_ad_fun_arg = function(tmb_cpp = "macpan2") {
    list(
        data = self$data_arg(),
        parameters = self$param_arg(),
        random = self$random_arg(),
        DLL = tmb_cpp
    )
  }
  self$ad_fun = function(tmb_cpp = "macpan2") {
    do.call(TMB::MakeADFun, self$make_ad_fun_arg(tmb_cpp))
  }

  self$simulator = function(tmb_cpp = "macpan2", initialize_ad_fun = TRUE) {
    TMBSimulator(self, tmb_cpp = tmb_cpp, initialize_ad_fun = initialize_ad_fun)
  }

  self$add = TMBAdder(self)
  self$insert = TMBInserter(self)
  self$print = TMBPrinter(self)
  self$replace = TMBReplacer(self)

  return_object(
    valid$tmb_model$assert(self),
    "TMBModel"
  )
}

TMBCompartmentalSimulator = function(tmb_simulator, compartmental_model) {
  self = tmb_simulator
  self$compartmental_model = compartmental_model
  return_object(self, "TMBCompartmentalSimulator")
}

# SimulatorsList = function() {
#   self = Base()
#   self$.simulators = list()
#   self$simulators = function() self$.simulators
#   self$add = function(simulator) {
#     if (!any(vapply(self$.simulators, identical, logical, simulator))) {
#       self$.simulators = append(self$.simulators, simulator)
#     }
#   }
#   return_object(self, "SimulatorsList")
# }

## The copied method acts on the source when called by the target.
## This is useful when the target has the source as a composed object.
# copy_method = function(
#       method ## string giving the name of the method to transfer
#     , source ## source object to donate the method
#     , target ## target object to receive the method
#     , return = TRUE ## should the return value in the source be returned in the target?
#   ) {
#   force(method); force(source); force(target)
#   target[[method]] = function() {
#     named_args = as.list(environment())
#     dot_args = try(list(...), silent = TRUE)
#     if (inherits(dot_args, "try-error")) dot_args = list()
#     args = c(named_args, dot_args)
#     y = do.call(source[[method]], args)
#     if (return) return(y)
#   }
#   formals(target[[method]]) = formals(source[[method]])
# }
# copy_methods = function(methods, source, target, return = TRUE) {
#   for (method in methods) copy_method(method, source, target, return)
# }

TMBSimulationUtils = function() {
  self = Base()
  self$.simulation_formatter = function(r, .phases) {
    r = setNames(
      as.data.frame(r$values),
      c("matrix", "time", "row", "col", "value")
    )  ## get raw simulation output from TMB and supply column names (which don't exist on the TMB side)
    r$matrix = self$matrix_names()[r$matrix + 1L]  ## replace matrix indices with matrix names
    dn = self$tmb_model$init_mats$.dimnames ## get the row and column names of matrices with such names
    for (mat in names(dn)) {
      i = r$matrix == mat

      ## convert to 1-based indices for R users
      row_indices = as.integer(r[i,"row"]) + 1L
      col_indices = as.integer(r[i,"col"]) + 1L

      ## add row and column names if available
      r[i, "row"] = dn[[mat]][[1L]][row_indices]
      r[i, "col"] = dn[[mat]][[2L]][col_indices]

      ## if some of the row and column names are unavailable,
      ## replace with indices -- this is important for the use case
      ## where a named matrix changes shape/size, beacuse row and column
      ## names can be set for the initial shape/size
      missing_row_nms = is.na(r[i, "row"])
      missing_col_nms = is.na(r[i, "col"])
      r[i, "row"][missing_row_nms] = as.character(row_indices[missing_row_nms])
      r[i, "col"][missing_col_nms] = as.character(col_indices[missing_col_nms])
    }
    r$time = as.integer(r$time)
    num_t = self$tmb_model$time_steps$time_steps
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
  self$.find_problematic_expression = function(row) {
    expr_num_p_table_rows = self$tmb_model$data_arg()$expr_num_p_table_rows
    expr_num = min(which(row < cumsum(expr_num_p_table_rows)))
    deparse1(self$tmb_model$expr_list$expr_list()[[expr_num]])
  }
  self$.runner = function(..., .phases = c("before", "during", "after"), .method = c("report", "simulate")) {
    .method = match.arg(.method)
    fixed_params = as.numeric(unlist(list(...)))
    if (length(fixed_params) == 0L) {
      r = self$ad_fun()[[.method]]()
    } else {
      r = self$ad_fun()[[.method]](fixed_params)
    }
    if (r$error != 0L) {
      stop(
        "\nError thrown by the TMB engine at the following expression:\n",
        self$.find_problematic_expression(r$expr_row)
      )
    }
    self$.simulation_formatter(r, .phases)
  }
  return_object(self, "TMBSimulationFormatter")
}

#' TMB Simulator
#'
#' Construct an object with methods for simulating from and optimizing a
#' compartmental model made using \code{\link{TMBModel}}.
#'
#' @param tmb_model An object of class \code{\link{TMBModel}}.
#' @param tmb_cpp Name of a C++ program using TMB as the simulation engine.
#' @param initialize_ad_fun Should the TMB AD function be intialized? This
#' should usually be set to \code{TRUE} unless you want to hack the data
#' structure passed to TMB (which can be acquired using
#' `$tmb_model$make_ad_fun_arg()`) before passing it yourself to
#' `TMB::MakeADFun`. This is particularly useful if you want to modify
#' `tmb_cpp`.
#'
#' @return Object of class \code{TMBSimulator} with the following methods.
#'
#' ## Methods
#'
#' * `$report()`: Runs simulations and returns a data frame with the following
#' columns.
#'     * `matrix`: Name of the matrix with values returned.
#'     * `time`: Time step of the values.
#'     * `row`: Row in the `matrix` containing the `value`.
#'     * `col`: Column in the `matrix` containing the `value`.
#'     * `value`: Numerical value being reported.
#' * `$error_code()`: If the simulations result in an engine error then the
#' code associated with this error is returned, otherwise the code `0` is
#' returned.
#' * `$ad_fun()`: Return the underlying [TMB](https://github.com/kaskr/adcomp)
#' object.
#'
#' @importFrom MASS mvrnorm
#' @export
TMBSimulator = function(tmb_model, tmb_cpp = "macpan2", initialize_ad_fun = TRUE) {
  self = TMBSimulationUtils()

  ## Args
  self$tmb_model = tmb_model
  self$tmb_cpp = tmb_cpp

  ## Standard Methods
  self$matrix_names = function() self$tmb_model$init_mats$.names()
  self$ad_fun = function() self$tmb_model$ad_fun(self$tmb_cpp)

  self$objective = function(...) {
    fixed_params = as.numeric(unlist(list(...)))
    self$ad_fun()$fn(fixed_params)
  }
  self$gradient = function(...) {
    fixed_params = as.numeric(unlist(list(...)))
    self$ad_fun()$gr(fixed_params)
  }
  self$hessian = function(...) {
    fixed_params = as.numeric(unlist(list(...)))
    self$ad_fun()$he(fixed_params)
  }
  self$error_code = function(...) self$ad_fun()$report(...)$error
  self$sdreport = function() TMB::sdreport(self$ad_fun())
  self$cov.fixed = function() self$sdreport()$cov.fixed
  self$par.fixed = function() self$sdreport()$par.fixed
  self$report = function(..., .phases = c("before", "during", "after")) {
    self$.runner(..., .phases = .phases, .method = "report")
  }
  self$report_values = function(..., .phases = c("before", "during", "after")) {
    self$report(..., .phases = .phases)$value
  }
  self$report_ensemble = function(...
      , .phases = c("before", "during", "after")
      , .n = 100
      , .probs = c(0.025, 0.5, 0.975)
    ) {
    r = self$report(..., .phases = .phases)
    rr = (MASS::mvrnorm(.n, self$par.fixed(), self$cov.fixed())
      |> apply(1, self$report_values, .phases = .phases)
      |> apply(1, quantile, probs = .probs)
      |> t()
    )
    cbind(r, rr)
  }
  self$simulate = function(..., .phases = c("before", "during", "after")) {
    self$.runner(..., .phases = .phases, .method = "simulate")
  }
  self$matrix = function(..., matrix_name, time_step) {
    r = self$report(...)
    i = (r$matrix == as.character(matrix_name)) & (r$time == as.integer(time_step))
    rr = r[i, c("row", "col", "value")]
    if (!any(is.na(as.integer(rr$row)))) {
      return(matrix(rr$value, max(as.integer(rr$row)) + 1L))
    }
    matrix(rr$value, max(rr$row) + 1L)
  }

  ## Composition
  self$optimize = TMBOptimizer(self)
  self$optimization_history = TMBOptimizationHistory(self)
  self$print = TMBSimulatorPrinter(self)
  self$insert = TMBSimulatorInserter(self)
  self$add = TMBSimulatorAdder(self)
  self$replace = TMBSimulatorReplacer(self)
  self$current = TMBCurrentParams(self)
  self$get = TMBSimulatorGetters(self)

  initialize_cache(self, "ad_fun", "sdreport")
  if (initialize_ad_fun) {
    if (inherits(self$ad_fun(), "try-error")) {
      stop(
        "\nThe tmb_model object is malformed,",
        "\nwith the following explanation:\n",
        self$ad_fun()
      )
    }
  }
  return_object(self, "TMBSimulator")
}


