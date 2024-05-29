TMBModelSpec = function(
      before = list()
    , during = list()
    , after = list()
    , default = list()
    , integers = list()
    , must_save = character()
    , must_not_save = character()
    , sim_exprs = character()
    , state_update = c("euler", "rk4", "euler_multinomial", "hazard")
  ) {
  self = Base()
  self$change_model = get_change_model(before, during, after)
  self$state_update = get_state_update_type(match.arg(state_update), self$change_model)
  self$update_method = get_state_update_method(self$state_update, self$change_model)
  self$before = before
  self$during = during
  self$after = after
  self$default = default
  self$integers = integers
  self$must_save = must_save
  self$must_not_save = must_not_save
  self$sim_exprs = sim_exprs
  
  self$expr_list = function() {
    ExprList(
        self$update_method$before()
      , self$update_method$during()
      , self$update_method$after()
    )
  }
  self$unrendered_expr_list = function() {
    ExprList(
        self$before
      , self$during
      , self$after
    )
  }
  
  self$all_derived_vars = function() {
    self$expr_list()$all_derived_vars()
  }
  self$all_default_vars = function() {
    self$expr_list()$all_default_vars()
  }
  self$all_formula_vars = function() {
    self$expr_list()$all_formula_vars()
  }
  
  ## check for name ambiguity
  self$check_names = function() {
    tmb_model_names = c(names(self$all_integers()), names(self$all_matrices()))
    ambiguous = duplicated(tmb_model_names)
    if (any(ambiguous)) {
      msg(
          msg_hline()
        , msg_colon(
            msg(
                "The following names were used for one or more purposes."
              , "(either as names in the default or integers lists,"
              , "or as names of the elements of vectors in the default list)"
            )
          , msg_indent_break(unique(tmb_model_names[ambiguous]))
        )
      ) |> stop()
    }
    TRUE
  }
  ## convert each name of each named vector in the default list into
  ## an 'implied' integer vector for subsetting vectors in before, during,
  ## and after expressions by position name
  self$all_integers = function() {
    ## TODO: make smarter so that only used integer vectors
    ## are produced and maybe even check if an integer vector
    ## is being used in the wrong numeric vector
    implied_integers = implied_position_vectors(self$default)
    c(implied_integers, self$integers)
  }
  
  self$empty_matrices = function() {
    dv = setdiff(self$all_derived_vars(), names(self$default))
    rep(list(empty_matrix), length(dv)) |> setNames(dv)
  }
  self$all_matrices = function() c(self$default, self$empty_matrices())
  
  self$copy = function() {
    mp_tmb_model_spec(
        self$before, self$during, self$after
      , self$default, self$integers
      , self$must_save, self$must_not_save, self$sim_exprs
      , self$state_update
    )
  }
  self$change_update_method = function(state_update = c("euler", "rk4", "euler_multinomial", "hazard")) {
    if (self$state_update == "no") {
      warning("This model has not formalized the notion of a state variable, and so changing how the state variables are updated has no effect. Models with formalized state variables are specified with state flows using functions such as mp_per_capita_flow.")
    }
    mp_tmb_model_spec(
        self$before, self$during, self$after
      , self$default, self$integers
      , self$must_save, self$must_not_save, self$sim_exprs
      , state_update
    )
  }
  self$expand = function() {
    mp_tmb_model_spec(
        self$update_method$before()
      , self$update_method$during()
      , self$update_method$after()
      , self$default, self$integers
      , self$must_save, self$must_not_save, self$sim_exprs
      , self$state_update
    )
  }
  self$name_map = function(local_names) {
    macpan2:::map_names(
        self$all_formula_vars()
      , setNames(as.list(local_names), local_names)
    )
  }
  self$tmb_model = function(
        time_steps = 0
      , outputs = character()
      , default = list()
      , initialize_ad_fun = TRUE
  ) {
    self$check_names()
    mats = update_default(self$all_matrices(), default)
    mat_args = c(mats, mat_options$from_spec(
        mats
      , outputs
      , self$must_save
      , self$must_not_save
    ))
    TMBModel(
        init_mats = do.call(MatsList, mat_args)
      , expr_list = self$expr_list()
      , engine_methods = EngineMethods(
          int_vecs = do.call(IntVecs, self$all_integers())
        )
      , time_steps = Time(as.integer(time_steps))
    )
  }
  self$simulator_fresh = function(
        time_steps = 0
      , outputs = character()
      , default = list()
      , initialize_ad_fun = TRUE
    ) {
    m = self$tmb_model(time_steps, outputs, default, initialize_ad_fun)
    m$simulator(outputs = outputs, initialize_ad_fun = initialize_ad_fun)
  }
  self$simulator_cached = memoise(self$simulator_fresh)
  return_object(self, "TMBModelSpec")
}

mat_options = list(
  from_spec = function(initial_mats, outputs, must_save, must_not_save) {
    matrix_outputs = intersect(outputs, names(initial_mats))
    row_outputs = (outputs
      |> setdiff(matrix_outputs)
      |> intersect(initial_rownames(initial_mats))
    )
    check_outputs(outputs, matrix_outputs, row_outputs)
    .mats_to_return = (initial_mats
      |> lapply(names)
      |> Filter(f = is.character)
      |> Filter(f = \(x) any(x %in% row_outputs))
      |> names()
      |> c(matrix_outputs)
      |> unique()
    )
    .mats_to_save = (.mats_to_return
      |> union(must_save)
      |> setdiff(must_not_save)
    )
    nlist(.mats_to_return, .mats_to_save)
  }, 
  from_simulator = function(mats_to_return, mats_to_save) {
    list(.mats_to_return = mats_to_return, .mats_to_save = mats_to_save)
  }
)

check_outputs = function(outputs, matrix_outputs, row_outputs) {
  realized_outputs = c(matrix_outputs, row_outputs)
  outputs_not_realized = setdiff(outputs, realized_outputs)
  if (length(outputs_not_realized) > 0L) {
    msg = sprintf("The following outputs were requested but not available in the model:\n%s\nThey will be silently ignored.", paste0(outputs_not_realized, ", "))
    warning(msg)
  }
}

initial_rownames = function(initial_mats) {
  (initial_mats
    |> lapply(as.matrix)
    |> lapply(rownames)
    |> unlist(use.names = FALSE, recursive = TRUE)
    |> unique()
  )
}
update_default = function(mats, default) {
  mats[names(default)] = default
  mats
}

#' Specify a TMB Model
#' 
#' Specify a model in the TMB engine.
#' 
#' @param before List of formulas to be evaluated (in the order provided) before
#' the simulation loop begins. Each \code{\link{formula}} must have a left hand
#' side that gives the name of the matrix being updated, and a right hand side
#' giving an expression containing only the names of matrices in the model,
#' functions defined in the TMB engine, and numerical literals (e.g.
#' \code{3.14}). The available functions in the TMB engine  can be described in
#' \code{\link{engine_functions}}. Names can be provided for the components of
#' \code{before}, and these names do not have to be unique. These names are
#' used by the \code{sim_exprs} argument.
#' @param during List of formulas to be evaluated at every iteration of the
#' simulation loop, with the same rules as \code{before}.
#' @param after List of formulas to be evaluated after the simulation loop,
#' with the same rules as \code{before}.
#' @param default Named list of objects, each of which can be coerced into 
#' a \code{\link{numeric}} \code{\link{matrix}}. The names refer to 
#' variables that appear in \code{before}, \code{during}, and \code{after}.
#' @param integers Named list of vectors that can be coerced to integer
#' vectors. These integer vectors can be used by name in model formulas to
#' provide indexing of matrices and as grouping factors in 
#' \code{\link{group_sums}}.
#' @param must_save Character vector of the names of matrices that must have 
#' their values stored at every iteration of the simulation loop. For example,
#' a matrix that the user does not want to be returned but that impacts dynamics
#' with a time lag must be saved and therefore in this list.
#' @param must_not_save Character vector of the names of matrices that must
#' not have their values stored at every iteration of the simulation loop. For
#' example, the user may ask to return a very large matrix that would create
#' performance issues if stored at each iteration. The creator of the model
#' can mark such matrices making it impossible for the user of the model to
#' save their full simulation history.
#' @param sim_exprs Character vector of the names of \code{before}, 
#' \code{during}, and \code{after} expressions that must only be evaluated 
#' when simulations are being produced and not when the objective function is
#' being evaluated. For example, expressions that generate stochasticity should
#' be listed in \code{sim_exprs} because TMB objective functions must be
#' continuous.
#' @param state_update (experimental) Optional character vector for how to update the
#' state variables when it is relevant. Options include `"euler"`, `"rk4"`, 
#' and `"euler_multinomial"`.
#' @export
mp_tmb_model_spec = TMBModelSpec

#' @export
print.TMBModelSpec = function(x, ...) {
  spec_printer(x, include_defaults = TRUE)
}

spec_printer = function(x, include_defaults) {
  #e = ExprList(x$before, x$during, x$after)
  #e = x$expr_list()
  if (include_defaults) {
    cat("---------------------\n")
    msg("Default values:\n") |> cat()
    cat("---------------------\n")
    print(melt_default_matrix_list(x$default), row.names = FALSE)
    cat("\n")
  }
  exprs = c(x$before, x$during, x$after)
  schedule = c(length(x$before), length(x$during), length(x$after))
  model_steps_printer(exprs, schedule)
}
