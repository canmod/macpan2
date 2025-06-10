TMBModelSpec = function(
      before = list()
    , during = list()
    , after = list()
    , default = list()
    , inits = list()
    , integers = list()
    , must_save = character()
    , must_not_save = character()
    , sim_exprs = character()
    , state_update = c(
          "euler"
        , "rk4"
        , "discrete_stoch"
        , "hazard"
        , "rk4_old"
        , "euler_multinomial"
      )
  ) {
  default = c(default, inits)
  must_not_save = handle_saving_conflicts(must_save, must_not_save)
  self = Base()
  self$macpan2_version = packageVersion("macpan2")
  before = force_expr_list(before)
  during = force_expr_list(during)
  after = force_expr_list(after)
  self$change_model = get_change_model(before, during, after)
  self$state_update = get_state_update_type(match.arg(state_update), self$change_model)
  self$update_method = get_state_update_method(self$state_update, self$change_model)
  self$change_components = function() self$change_model$change_list
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
  
  self$all_derived_vars = function() {
    self$expr_list()$all_derived_vars()
  }
  self$all_default_vars = function() {
    self$expr_list()$all_default_vars()
  }
  self$all_formula_vars = function() {
    self$expr_list()$all_formula_vars()
  }
  self$all_default_mats = function() {
    setdiff(
        self$all_default_vars()
      , names(self$integers)
    )
  }
  ## matrices in self$default that are not used by any expressions.
  ## note that unused expressions are not always useless. for example
  ## observed data might be added to a spec, and these data will not
  ## be used by the simulations but could be used to compute the
  ## objective function in a calibrator.
  self$all_unused_defaults = function() {
    setdiff(names(self$default), self$all_default_mats())
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
  
  self$name_map = function(local_names) {
    map_names(
      self$all_formula_vars()
      , setNames(as.list(local_names), local_names)
    )
  }
  
  self$copy = function() {
    mp_tmb_model_spec(
        before = self$before, during = self$during, after = self$after
      , default = self$default, integers = self$integers
      , must_save = self$must_save, must_not_save = self$must_not_save
      , sim_exprs = self$sim_exprs
      , state_update = self$state_update
    )
  }
  self$change_update_method = function(
      state_update = c("euler", "rk4", "discrete_stoch", "hazard", "rk4_old", "euler_multinomial")
    ) {
    
    if (self$state_update == "no") {
      msg_space(
          "This model has not formalized the notion of a state variable,"
        , "and so changing how the state variables are updated has no effect."
        , "Models with formalized state variables are specified with state"
        , "flows using functions such as mp_per_capita_flow."
      ) |> warning()
    }
    mp_tmb_model_spec(
        before = self$before, during = self$during, after = self$after
      , default = self$default, integers = self$integers
      , must_save = self$must_save, must_not_save = self$must_not_save
      , sim_exprs = self$sim_exprs, state_update = state_update
    )
  }
  self$expand = function() {
    mp_tmb_model_spec(
        before = self$update_method$before()
      , during = self$update_method$during()
      , after = self$update_method$after()
      , default = self$default
      , integers = self$integers
      , must_save = self$must_save
      , must_not_save = self$must_not_save
      , sim_exprs = self$sim_exprs
      , state_update = self$state_update
    )
  }
  self$name_map = function(local_names) {
    map_names(
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
    time_args = must_save_time_args(
      c(
          self$update_method$before()
        , self$update_method$during()
        , self$update_method$after()
      )
    )
    mats = update_default(self$all_matrices(), default)
    mat_args = c(mats, mat_options$from_spec(
        mats
      , outputs
      , c(self$must_save, time_args)
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

handle_saving_conflicts = function(must_save, must_not_save) {
  ## must_save takes precedence over must_not_save
  problems = intersect(must_save, must_not_save)
  if (length(problems) != 0L) {
    msg = sprintf(
        "The following matrices were removed from the must_not_save list\nbecause they are also on the must_save list, which takes precedence:\n      %s\n"
      , paste0(problems, collapse = ", ")
    )
    getOption("macpan2_saving_conflict_msg_fn")(msg)
    must_not_save = setdiff(must_not_save, problems)
  }
  return(must_not_save)
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

must_save_time_args = function(formulas) {
  time_dep_funcs = getOption("macpan2_time_dep_funcs")
  parse_expr = make_expr_parser(finalizer = finalizer_char)
  time_args = (formulas
     |> lapply(rhs)
     |> lapply(parse_expr)
     |> lapply(\(x) x$x[x$i[x$x %in% time_dep_funcs] + 1])
     |> unlist()
  )
  return(time_args)
}

#' Create TMB Model Specification
#' 
#' Specify a simulation model in the TMB engine. A detailed explanation of this
#' function is covered in `vignette("quickstart")`.
#' 
#' @param before List of formulas to be evaluated (in the order provided)
#' before the simulation loop begins. These formulas must be standard 
#' two-sided R \code{\link{formula}} objects. See `details` below for the 
#' rules for these formulas.
#' @param during List of formulas or calls to flow functions (e.g., 
#' \code{\link{mp_per_capita_flow}}) to be evaluated at every iteration of the
#' simulation loop.
#' @param after List of formulas to be evaluated (in the order provided)
#' before the simulation loop begins. These formulas must be standard 
#' two-sided R \code{\link{formula}} objects. See `details` below for the 
#' rules for these formulas.
#' @param default Named list of objects, each of which can be coerced into 
#' a \code{\link{numeric}} \code{\link{matrix}}. The names refer to 
#' quantities that appear in \code{before}, \code{during}, and \code{after}.
#' Each quantity that is used in \code{before}, \code{during}, or \code{after}
#' before it is defined must be given a numerical value in this \code{default}
#' list.
#' @param inits Named list of initial values of state variables.  These 
#' initial values can be added to the `default` list with identical results,
#' but adding them to `inits` is better practice because it makes it clear
#' that they are initial values that will change as the state updates.
#' @param integers Named list of vectors that can be coerced to integer
#' vectors. These integer vectors can be used by name in model formulas to
#' provide indexing of matrices and as grouping factors in 
#' \code{\link{group_sums}}.
#' @param must_save Character vector of the names of variables that must have 
#' their values stored at every iteration of the simulation loop. For example,
#' a variable that you do not want to be returned, but that impacts 
#' dynamics with a time lag, must be saved and therefore must be in this list.
#' @param must_not_save Character vector of the names of variables that must
#' not have their values stored at every iteration of the simulation loop. For
#' example, the user may ask to return a very large matrix that would create
#' performance issues if stored at each iteration. The creator of the model
#' can mark such variables making it impossible for the user of the model to
#' save their full simulation history.
#' @param sim_exprs Character vector of the names of \code{before}, 
#' \code{during}, and \code{after} expressions that must only be evaluated 
#' when simulations are being produced and not when the objective function is
#' being evaluated. For example, expressions that generate stochasticity should
#' be listed in \code{sim_exprs} because TMB objective functions must be
#' continuous.
#' @param state_update Optional character vector for how to update the state 
#' variables when it is relevant. Options include `"euler"` (the default), 
#' `"rk4"`, and `"discrete_stoch"`.
#' 
#' @details
#' Expressions in the `before`, `during`, and `after` lists can be standard 
#' R \code{\link{formula}} objects for defining variables in the model. These
#' formulas must have a left hand side that gives the name of the (possibly 
#' matrix-valued) variable being updated, and a right hand side giving an 
#' expression containing only (1) the names of quantities in the model, (2) 
#' numerical literals (e.g., \code{3.14}), or (3) functions defined in the TMB 
#' engine (described in \code{\link{engine_functions}}). For example, the
#' expression `N ~ S + I + R` updates the value of `N` to be the sum of the
#' variables `S`, `I`, and `R`.
#' 
#' Names can be provided for the components of the `before`, `during`, and
#' `after` lists, and these names do not have to be unique. These names are 
#' used by the \code{sim_exprs} argument.
#' 
#' @examples
#' ## A simple SI model.
#' spec = mp_tmb_model_spec(
#'     during = mp_per_capita_flow("S", "I", "beta * I / N", "infection")
#'   , default = list(N = 100, S = 99, I = 1, beta = 0.2)
#' )
#' (spec 
#'   |> mp_simulator(time_steps = 5L, output = "infection") 
#'   |> mp_trajectory()
#' )
#' 
#' ## The `~` can be used for flexibly defining dynamical variables.
#' spec2 = mp_tmb_model_spec(
#'     during = list(
#'           force_of_infection ~ beta * I / N
#'         , mp_per_capita_flow("S", "I", "force_of_infection", "infection")
#'     )
#'   , default = list(N = 100, S = 99, I = 1, beta = 0.2)
#' )
#' (spec2
#'   |> mp_simulator(time_steps = 5L, output = "force_of_infection") 
#'   |> mp_trajectory()
#' )
#' 
#' ## The `before` argument can be used to pre-compute quantities before
#' ## the simulation loop begins. Here we compute `S` from `N` and `I`,
#' ## instead of specifying `S` in the `default` list. The potential
#' ## benefit here is that one could make `I` a parameter to be fitted,
#' ## while ensuring consistent values for `S`.
#' spec3 = mp_tmb_model_spec(
#'     before = S ~ N - I
#'   , during = mp_per_capita_flow("S", "I", "beta * I / N", "infection")
#'   , default = list(N = 100, I = 1, beta = 0.2)
#' )
#' (spec3 
#'   |> mp_simulator(time_steps = 5L, output = "infection") 
#'   |> mp_trajectory()
#' )
#' 
#' @concept create-model-spec
#' @export
mp_tmb_model_spec = TMBModelSpec

#' @export
print.TMBModelSpec = function(x, ...) mp_print_spec(x)

defaults_printer = function(x) {
  if (length(x$default) > 0L) {
    cat("---------------------\n")
    msg("Default values:\n") |> cat()
    print(
        melt_default_matrix_list(x$default, simplify_as_scalars = TRUE)
      , row.names = FALSE
    )
    cat("---------------------\n")
    cat("\n")
  } else {
    cat("---------------------\n")
    msg("No default values\n") |> cat()
    cat("---------------------\n")
  }
}

spec_printer = function(x, include_defaults) {
  if (include_defaults) defaults_printer(x)
  exprs = c(x$before, x$during, x$after)
  schedule = c(length(x$before), length(x$during), length(x$after))
  model_steps_printer(exprs, schedule)
  more_help = c(
      "Discover more about model specifications here:\n"
    , "https://canmod.github.io/macpan2/reference#specifications \n"
  )
  # cat(more_help)
}

#' Print Model Specification
#' 
#' @param model A model produced by \code{\link{mp_tmb_model_spec}}.
#' 
#' @export
mp_print_spec = function(model) spec_printer(model, include_defaults = TRUE)

#' @describeIn mp_print_spec Print just the expressions executed before the
#' simulation loop.
#' @export
mp_print_before = function(model) {
  model_steps_printer(
      model$before
    , c(length(model$before), 0L, 0L)
  )
}

#' @describeIn mp_print_spec Print just the expressions executed during each
#' iteration of the simulation loop.
#' @export
mp_print_during = function(model) {
  model_steps_printer(
      model$during
    , c(0L, length(model$during), 0L)
  )
}

#' @describeIn mp_print_spec Print just the expressions executed after the
#' simulation loop.
#' @export
mp_print_after = function(model) {
  model_steps_printer(
      model$after
    , c(0L, 0L, length(model$after))
  )
}

#' Version Update
#' 
#' Update a model specification so that it is compatible with the 
#' installed version of `macpan2`.
#' 
#' @param spec Object produced by \code{\link{mp_tmb_model_spec}} or another 
#' function that produces the same type of object.
#' 
#' @export
mp_version_update = function(spec) {
  if (!inherits(spec, "TMBModelSpec")) {
    stop("The spec argument must be a `macpan2` model specification.")
  }
  mp_tmb_model_spec(
      before = spec$before, during = spec$during, after = spec$after
    , default = spec$default, integers = spec$integers
    , must_save = spec$must_save, must_not_save = spec$must_not_save
    , sim_exprs = spec$sim_exprs
    , state_update = spec$state_update
  )
}

#' Read Serialized Model Specification
#' 
#' Uses \code{\link{readRDS}} to read in a saved model specification
#' created using a function like \code{\link{mp_tmb_model_spec}}, and
#' updates this specification using \code{\link{mp_version_update}} so that
#' it is compatible with the installed version of `macpan2`. To save
#' a model specification, just use the base `R` function \code{\link{saveRDS}}.
#' 
#' @param filename Path to a saved model specification object.
#' 
#' @export
mp_read_rds = function(filename) {
  obj = filename |> readRDS()
  if (!inherits(obj, "TMBModelSpec")) {
    stop("Found an object that is not a `macpan2` model specification.")
  }
  mp_version_update(obj)
}
