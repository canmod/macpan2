

handle_abs_rate_args = function(rate, rate_name = NULL, flow_name = NULL) {
  if (!is.null(rate_name)) {
    warning(
        "The rate_name argument is deprecated; "
      , "please use 'flow_name' instead"
    )
    if (!is.null(flow_name)) {
      stop(
          "You used both the 'rate_name' and 'flow_name' arguments. "
        , "Please only use 'flow_name', as 'rate_name' is deprecated."
      )
    }
    flow_name = rate_name
  }
  if (!is_two_sided(rate)) {
    if (is_one_sided(rate)) {
      rate = rhs_char(rate)
      if (is.null(rate_name)) {
        if (is_char_symbol(rate)) {
          rate_name = rate
        } else {
          stop(
              "rate_name must be specified when rate is a "
            , "one_sided formula and rate gives an expression "
            , "involving functions or operations."
          )
        }
      }
    }
    rate = two_sided(rate_name, rate)
  }
  rate
}
handle_rate_args = function(rate, abs_rate, flow_name) {
  if (!is.null(abs_rate)) {
    warning(
        "The abs_rate argument is deprecated; "
      , "please use 'flow_name' instead"
    )
    if (!is.null(flow_name)) {
      stop(
          "You used both the 'abs_rate' and 'flow_name' arguments. "
        , "Please only use 'flow_name', as 'abs_rate' is deprecated."
      )
    }
    flow_name = abs_rate
  }
  if (!is_two_sided(rate)) {
    if (is_one_sided(rate)) rate = rhs_char(rate)
    if (is.null(flow_name)) {
      stop(
          "flow_name must be specified when rate is a one_sided formula "
        , "or character string."
      )
    }
    rate = two_sided(flow_name, rate)
  }
  return(rate)
}


##' filter out expressions from expr_list that are not 'iterable'
##' 
##' @param expr_list list of expressions that will be checked for non-iterable
##' functions or expressions with state variables
##' @param states list of state variables associated with the expr_list
##' @param is_first is this the first iteration, and therefore ok to retain
##' non-iterables in the output expr_list
##' @noRd
only_iterable = function(expr_list, states, is_first = FALSE) {
  if (is_first) return(expr_list)
  non_iterable_funcs = getOption("macpan2_non_iterable_funcs")
  parse_expr = make_expr_parser(finalizer = finalizer_char)
  no_iterable_funcs = (expr_list
     |> lapply(rhs)
     |> lapply(parse_expr)
     |> vapply(
         \(x) !any(non_iterable_funcs %in% x$x)
       , logical(1L)
      )
  )
  no_state_var_assignments = (expr_list
     |> lapply(lhs)
     |> lapply(parse_expr)
     |> vapply(
         \(x) !any(states %in% x$x)
       , logical(1L)
      )
  )
  is_iterable = no_iterable_funcs & no_state_var_assignments
  expr_list[is_iterable]
}



## two class types:
##   1. ChangeModel (set of flows) (e.g. SimpleChangeModel) (change_models.R)
##   2. UpdateMethod (Euler, RK4, EulerMultinomial, TimeDerivative) (e.g. RK4UpdateMethod) (update_methods.R)
##
## one data structure type:
##   1. ChangeComponent (Flow, Birth, ...) (e.g. PerCapitaFlow) (change_components.R)


## Abstract ChangeModel -- functions for specifying what kinds of expressions 
## should go in what phases of the simulation. more finely grained than just
## before-during-after, and makes absolute flow and state updates first
## class citizens. importantly this is not what creates the final expression
## lists -- something that is done by UpdateMethod
ChangeModel = function() {
  self = Base()
  
  # lists of formula expressions to be added to a `before` list
  self$before_loop = function() list()
  
  # lists of formula expressions to be added to a `during` list
  self$once_start = function() list()
  self$before_flows = function() list()
  self$update_flows = function() list()
  self$before_state = function() list()
  self$update_state = function() list()
  self$after_state = function() list()
  self$once_finish = function() list()
  
  # list of formula expressions to be added to an `after` list
  self$after_loop = function() list()
  
  self$empty_flow_frame = empty_frame("size", "change", "rate", "abs_rate")
  self$empty_change_frame = empty_frame("state", "change")
  
  # one row per flow
  #  size : aka name of the from compartment
  #  change : absolute flow rate name
  #  rate : string with expression for the per-capita flow rate
  #  abs_rate : string with expression for the absolute flow rate
  self$flow_frame = function() self$empty_flow_frame
  
  # one row per term in a state update expression
  #  state : name of the state being updated
  #  change : string with the term in the expression that updates that state
  self$change_frame = function() self$empty_change_frame
  
  # character vector of ChangeComponent class names used in the model
  self$change_classes = function() character()
  
  self$other_generated_formulas = function() list()
  
  # list of standard two-sided R formulas provided either as formulas by the 
  # user, or perhaps as a formula generator (not yet developed). the key here 
  # is that the user is responsible for name clashes. so developers should not 
  # use automatically generated variable names in these formulas.
  self$user_formulas = function() list()
  
  # character vector of all variable names that will be sent the the engine,
  # except any temporary variables used by the state update method (e.g.
  # coefficients in an RK4 update). the user should feel free to depend on 
  # these names in any calls to mp_tmb_update|insert|delete functions.
  self$all_user_aware_names = function() character()
  
  return_object(self, "ChangeModel")
}



## UpdateMethod -- functions for rendering the final before-during-after
## expression lists to be used in simulations
UpdateMethod = function(exising_global_names = character()) {
  self = Base()
  self$existing_global_names = exising_global_names
  self$before = function() list()
  self$during = function() list()
  self$after = function() list()
  return_object(self, "UpdateMethod")
}

## ChangeComponent -- functions for producing the change_frame and flow_frame,
## which describe model dynamics, and also other associated formulas
ChangeComponent = function() {
  self = Base()
  
  ## column - state: state variable being changed (i.e. updated at each step)
  ## column - change: signed absolute flow rates (variables or expressions that 
  ## don't involve any state variables)
  ## example:
  ## state, change
  ## S,     -infection
  ## I,     +infection
  ## I,     -recovery
  ## R,     +recovery
  self$change_frame = function() empty_frame("state", "change")
  
  ## column - size: variable (often a state variable or function of
  ## state variables) that gives the size of the population being drawn
  ## from in a flow (e.g. S is the size of an infection flow).
  ## column - change: unsigned absolute flow rate name.
  ## column - rate: per-capita flow rates (variables or expressions that 
  ## sometimes involve state variables).
  ## column - abs_rate: absolute flow rate expression
  ## example:
  ## size, change,    rate,          abs_rate
  ## S,    infection, beta * I / N,  S * (beta * I / N)
  ## I,    recovery,  gamma,         I * gamma
  ## N,    birth,     mu,            N * mu
  ## 
  ## one may wonder, as i have, why we need to separate the change column
  ## and the abs_rate column. because we sometimes need a symbol name that 
  ## can appear on the lhs of an expression for the absolute rate, and we
  ## sometimes directly specify the absolute rate through an expression 
  ## (although this is not the most comment case, it does come up with things
  ## like specifying absolute numbers of vaccines).
  self$flow_frame = function() empty_frame("size", "change", "rate", "abs_rate")
  
  self$other_generated_formulas = function() list()
  
  self$user_formulas = function() list()
  
  self$string = function() "Abstract change component"
  
  return_object(self, "ChangeComponent")
}


## Change Models

##' Simple Change Model
##' 
##' @param before List of two-sided formulas to be run in the before phase.
##' @param during List of two-sided formulas and/or `ChangeComponent` objects
##' to be used to create a `during` expression list after choosing an
##' `UpdateMethod`.
##' @param after List of two-sided formulas to be run in the after phase.
##' @examples
##' 
##' si = SimpleChangeModel(
##'   during = list(mp_per_capita_flow("S", "I", infection ~ beta * I / N))
##' ) |> EulerUpdateMethod()
##' si$during()
##' 
##' @noRd
SimpleChangeModel = function(before = list(), during = list(), after = list()) {
  self = ChangeModelDefaults()
  
  self$before = before
  self$during = during
  self$after = after
  
  self$change_list = lapply(self$during, to_change_component)
  #proposed_global_names = self$all_user_aware_names()
  
  ## find what formulas were provided by the user as raw formulas
  ## and make sure that they know that they should place them either
  ## at the beginning or the end of the during list
  head_indices = tail_indices = integer()
  is_formula = self$change_classes() == "Formula"
  
  if (all(is_formula)) {
    head_indices = seq_along(is_formula)
  }
  else if (length(is_formula) > 0L) {
    if (isTRUE(head(is_formula, 1L))) {
      first_non_formula = which.min(is_formula)
      head_indices = seq_len(first_non_formula - 1L)
    }
    if (isTRUE(tail(is_formula, 1L))) {
      tail_indices = length(is_formula) - ((which.min(rev(is_formula)) - 1L):1) + 1L
    }
    if (sum(is_formula) > (length(head_indices) + length(tail_indices))) {
      warning("Raw formula-valued expressions were inserted between flow-based expressions. These raw formulas will be ignored. Please collect raw formulas at the start or end of the during list")
    }
  }
  self$.before_flows = self$user_formulas()[head_indices]
  self$.after_state = self$user_formulas()[tail_indices]
  
  self$no_change_components = function() {
    all(self$change_classes() %in% c("Formula", "FormulaListHelper"))
  }
  
  self$before_flows = function() {
    unlist(self$.before_flows, recursive = FALSE, use.names = FALSE)
  }
  self$after_state = function() {
    unlist(self$.after_state, recursive = FALSE, use.names = FALSE)
  }
  
  ## this function should only be used for update methods that must
  ## be based on per-capita flows (e.g., Euler-multinomial, hazard).
  ## other update methods should work directly with the $flow_frame()
  self$update_flows = function() {
    frame = self$flow_frame()
    size_vars = setdiff(unique(frame$size), "")
    frame = frame[size_vars != "", , drop = FALSE]
    #if (any(size_vars == "")) stop("model includes flows coming from outside the system and so they cannot be used with update methods that cannot be expressed as a per-capita flow from somewhere in the system. please either use mp_euler or mp_rk4, or move absolute inflows to an ordinary formula component in the spec.")
    flow_list = list()
    formulas = sprintf("%s ~ %s", frame$change, frame$rate) |> lapply(as.formula)
    for (var in size_vars) flow_list[[var]] = formulas[frame$size == var]
    return(flow_list)
  }
  self$update_state = function() {
    frame = self$change_frame()
    exprs = tapply(frame$change, frame$state, paste0, collapse = " ", simplify = TRUE)[unique(frame$state)]
    sprintf("%s ~ %s", names(exprs), unname(exprs)) |> lapply(as.formula)
  }
  self$before_loop = function() self$before
  self$after_loop = function() self$after
  self$check()
  return_object(self, "SimpleChangeModel")
}

AllFormulaChangeModel = function(before = list(), during = list(), after = list()) {
  self = ChangeModelDefaults()
  self$before = before
  self$during = during
  self$after = after
  
  self$before_loop = function() self$before
  self$before_flows = function() self$during
  self$after_loop = function() self$after
  
  self$check()
  return_object(self, "AllFormulaChangeModel")
}


MockChangeModel = function() {
  self = ChangeModel()
  
  self$before_loop = function() list(S ~ N - I - V)
  self$before_flows = function() list(N ~ S + I + V)
  
  ## TODO: how to handle this method when we move to vector-valued 
  ## flow components and states
  ## TODO: make sure that in the scalar case at least the names are 
  ## unique in the output list
  self$update_flows = function() {
    list(
      # {capita} = list(
      #     {update_component_1} ~ {expression_giving_per_capita_flow_1}
      #   , ...
      #   , {update_component_n} ~ {expression_giving_per_capita_flow_n}
      # )
        N = list(birth ~ mu)
      , S = list(
            infection ~ beta * I / N
          , vax_uptake ~ rho * vax_supply
          , death_S ~ mu
        )
      , I = list(death_I ~ mu)
      , V = list(death_V ~ mu)
    )
  }
  
  self$update_state = function() list(
      S ~ birth - infection - vax_uptake - death_S
    , I ~ infection - recovery - death_I
    , V ~ vax_uptake - death_V
    , vax_supply ~ vax_production - vax_uptake - vax_expiry
  )
  
  self$default = function() list()
  self$integers = function() list()
  
  return_object(self, "ChangeModel")
}


##' Change How State Variables are Updated
##' 
##' These functions return a modified version of a model specification, such 
##' that the state variables are updated each time step according to different 
##' numerical methods.
##' 
##' By choosing one of these functions, one is able to convert 
##' 
##' To see the computations that update the state variables under these 
##' modified specifications, one may use the
##' \code{\link{mp_expand}} function (see examples).
##' 
##' The default update method for model specifications produced using
##' \code{\link{mp_tmb_model_spec}} is `mp_euler`. This update method
##' yields a difference-equation model where the state is updated once
##' per time-step using the absolute flow rate as the difference between
##' steps.
##' 
##' @param model Object with quantities that have been explicitly 
##' marked as state variables.
##' 
##' @examples
##' sir = mp_tmb_library("starter_models", "sir", package = "macpan2")
##' sir
##' sir |> mp_euler() |> mp_expand()
##' sir |> mp_rk4() |> mp_expand()
##' sir |> mp_discrete_stoch() |> mp_expand()
##' 
##' @name state_updates
NULL

##' @describeIn state_updates ODE solver using the Euler method, which is
##' equivalent to treating the model as a set of discrete-time difference 
##' equations. This is the default method used by 
##' \code{\link{mp_tmb_model_spec}}, but this default can be changed using
##' the functions described below.
##' @export
mp_euler = function(model) UseMethod("mp_euler")

##' @describeIn state_updates ODE solver using Runge-Kutta 4. Any formulas that
##' appear before model flows in the `during` list will only be updated
##' with RK4 if they do contain functions in 
##' `getOption("macpan2_non_iterable_funcs")` and if they do not make any
##' state variable assignments (i.e., the left-hand-side does not contain
##' state variables). Each formula that does not meet these conditions will 
##' be evaluated only once at each time-step before the other three RK4
##' iterations are taken. By default, the `time_var` function and functions
##' that generate random numbers (e.g., `rbinom`) are not iterable. Functions
##' that generate random numbers will only be called once with state update
##' methods that do not repeat expressions more than once per time-step 
##' (e.g., \code{\link{mp_euler}}), and so repeating these functions with RK4
##' could make it difficult to compare methods. If you really do want to 
##' regenerate random numbers at each RK4 iteration, you can do so by setting
##' the above option appropriately. The `time_var` function assumes that it
##' will only be called once per time-step, and so it should never be removed
##' from the list of non-iterable functions. Although in principle it could
##' make sense to update state variables manually, it currently causes us to
##' be confused. We therefore require that all state variable updates are set
##' explicitly (e.g., with \code{\link{mp_per_capita_flow}}).
##' @export
mp_rk4 = function(model) UseMethod("mp_rk4")

##' @describeIn state_updates Old version of `mp_rk4` that doesn't keep track
##' of absolute flows through each time-step. As a result this version is
##' more efficient but makes it more difficult to compute things like 
##' incidence over a time scale.
##' @export
mp_rk4_old = function(model) UseMethod("mp_rk4_old")

##' @describeIn state_updates Original and deprecated name for 
##' `mp_discrete_stoch`. In all new projects please use `mp_discrete_stoch`.
##' @export
mp_euler_multinomial = function(model) UseMethod("mp_euler_multinomial")

##' @describeIn state_updates Update state such that the probability of moving
##' from box `i` to box `j` in one time step is given by
##' `(1 - exp(-sum(r_i))) * (r_ij / r_i)`,
##' where `r_ij` is the per-capita rate of flow from box `i` to box `j`, and
##' `r_i` is the sum of all `r_ij` for a particular `i`.
##' These probabilities from box `i` are used together in a multinomial 
##' distribution that determines how many individuals go to each `j` box and 
##' how many stay in  `i`.
##' @export
mp_discrete_stoch = function(model) UseMethod("mp_discrete_stoch")

##' @describeIn state_updates Update state with hazard steps, which is equivalent
##' to taking the step given by the expected value of the Euler-multinomial
##' distribution.
##' @export
mp_hazard = function(model) UseMethod("mp_hazard")

##' @export
mp_euler.TMBModelSpec = function(model) model$change_update_method("euler")

##' @export
mp_rk4.TMBModelSpec = function(model) model$change_update_method("rk4")

##' @export
mp_rk4_old.TMBModelSpec = function(model) model$change_update_method("rk4_old")


##' @export
mp_euler_multinomial.TMBModelSpec = function(model) model$change_update_method("euler_multinomial")

##' @export
mp_discrete_stoch.TMBModelSpec = function(model) model$change_update_method("discrete_stoch")


##' @export
mp_hazard.TMBModelSpec = function(model) model$change_update_method("hazard")


#' Expand Model
#' 
#' Expand a structured model so that it is represented in an unstructured
#' format requiring a more verbose description. Currently,
#' this is only applicable for \code{\link{mp_tmb_model_spec}} objects
#' that have explicit flows  
#' (e.g. \code{\link{mp_per_capita_flow}}). For such models, `mp_expand`
#' produces a model with expression lists composed entirely of plain R
#' formulas.
#' 
#' @param model A model object.
#'
#' @examples
#' sir = mp_tmb_library("starter_models", "sir", package = "macpan2")
#' print(sir)
#' print(mp_expand(sir))
#' 
#' @export
mp_expand = function(model) UseMethod("mp_expand")

#' @export
mp_expand.TMBModelSpec = function(model) model$expand()


## Utilities
to_exogenous = function(flow_frame, rand_fn = NULL) {
  frame = flow_frame[flow_frame$size == "", , drop = FALSE]
  if (is.null(rand_fn)) {
    template = "%s ~ %s"
  } else {
    template = sprintf("%%s ~ %s(%%s)", rand_fn)
  }
   
  sprintf(template, frame$change, frame$abs_rate) |> lapply(as.formula)
}
to_exogenous_inputs = to_exogenous ## back-compat
flow_frame_to_absolute_flows = function(flow_frame) {
  char_vec = with(flow_frame, sprintf("%s ~ %s", change, abs_rate))
  lapply(char_vec, as.formula)
}
to_absolute_flows = function(per_capita_flows) {
  char_list = list()
  for (size_var in names(per_capita_flows)) {
    char_list = append(char_list, sprintf("%s ~ %s * %s"
      , vapply(per_capita_flows[[size_var]], lhs_char, character(1L))
      , size_var
      , vapply(per_capita_flows[[size_var]], rhs_char, character(1L))
    ))
  }
  lapply(char_list, as.formula)
}

get_state_update_type = function(state_update_type, change_model) {
  if (inherits(change_model, "AllFormulaChangeModel")) return("no")
  state_update_type
}
get_state_update_method = function(state_update, change_model) {
  if (inherits(change_model, "AllFormulaChangeModel")) {
    return(NoUpdateMethod(change_model))
  }
  cls_nm = sprintf("%sUpdateMethod", var_case_to_cls_case(state_update))
  if (state_update == "rk4") cls_nm = "RK4UpdateMethod"
  if (state_update == "rk4_old") cls_nm = "RK4OldUpdateMethod"
  get(cls_nm)(change_model)
}
get_change_model = function(before, during, after) {
  valid_before = all(vapply(before, is_two_sided, logical(1L)))
  if (!valid_before) stop("The before argument must be all two-sided formulas.")
  valid_after = all(vapply(after, is_two_sided, logical(1L)))
  if (!valid_after) stop("The after argument must be all two-sided formulas.")
  any_change_components = any(vapply(during, inherits, logical(1L), "ChangeComponent"))
  if (any_change_components) return(SimpleChangeModel(before, during, after))
  AllFormulaChangeModel(before, during, after)
}
force_expr_list = function(x) {
  is_change_component = function(x) inherits(x, "ChangeComponent")
  is_valid = function(x) isTRUE(is_two_sided(x) | is_change_component(x))
  if (is_two_sided(x)) return(list(x))
  if (is_change_component(x)) return(list(x))
  if (!is.list(x)) {
    ## TODO: msg should make more sense to humans
    stop("Argument must be a formula, change component, or a list of such objects.")
  }
  invalid = !vapply(x, is_valid, logical(1L))
  if (any(invalid)) {
    stop(
        "The expressions at the following positions are invalid: "
      , paste(which(invalid), collapse = ", ")
    )
  }
  return(x)
}

##' Update Methods
##' 
##' Convert a `ChangeModel` into an object that can return `before`, `during`,
##' and `after` expression lists.
##' 
##' @param change_model An object of class `ChangeModel`.
##' @name update_methods
##' @noRd

NoUpdateMethod = function(change_model) {
  self = UpdateMethod()
  self$change_model = change_model 
  self$before = function() self$change_model$before_loop()
  self$during = function() self$change_model$before_flows()
  self$after = function() self$change_model$after_loop()
  return_object(self, "NoUpdateMethod")
}

EulerUpdateMethod = function(change_model, existing_global_names = character()) {
  self = UpdateMethod()
  self$change_model = change_model
  
  ## euler method requires no additional names from the spec
  
  self$before = function() self$change_model$before_loop()
  self$during = function() {
    before_components = self$change_model$before_flows()
    components = flow_frame_to_absolute_flows(self$change_model$flow_frame())
    before_update = self$change_model$before_state()
    update = self$change_model$update_state()
    
    states = vapply(update, lhs_char, character(1L))
    rates = vapply(update, rhs_char, character(1L))
    update_char = sprintf("%s ~ %s %s", states, states, rates)
    new_update = lapply(update_char, as.formula)
    after_components = self$change_model$after_state()
    c(
        before_components, components, before_update
      , new_update, after_components
    )
  }
  
  self$after = function() self$change_model$after_loop()
  return_object(self, "EulerUpdateMethod")
}


RK4OldUpdateMethod = function(change_model) {
  self = UpdateMethod()
  self$change_model = change_model
  
  self$before = function() self$change_model$before_loop()
  self$during = function() {
    before_components = self$change_model$before_flows()
    components = flow_frame_to_absolute_flows(self$change_model$flow_frame())
    before_state = self$change_model$before_state()
    before = c(before_components, components, before_state)
    update = self$change_model$update_state()
    
    new_update = list()
    new_before = list()
    
    states = vapply(update, lhs_char, character(1L))
    rates = vapply(update, rhs_char, character(1L))
    
    existing_names = self$change_model$all_user_aware_names()
    local_step_names = list(
        k1 = sprintf("k1_%s", states)
      , k2 = sprintf("k2_%s", states)
      , k3 = sprintf("k3_%s", states)
      , k4 = sprintf("k4_%s", states)
    )
    step_names = map_names(existing_names, local_step_names)
    
    ## rk4 step 1
    k1_new_before = before
    k1_new_update = sprintf("%s ~ %s", step_names$k1, rates) |> lapply(as.formula)
    
    ## rk4 step 2
    state_replacements = sprintf("%s ~ (%s + (%s / 2))", states, states, step_names$k1) |> lapply(as.formula)
    k2_new_before = update_formulas(before, state_replacements)
    k2_new_update = sprintf("k2_%s ~ %s", states, rates) |> lapply(as.formula)
    
    ## rk4 step 3
    state_replacements = sprintf("%s ~ (%s + (%s / 2))", states, states, step_names$k2) |> lapply(as.formula)
    k3_new_before = update_formulas(before, state_replacements)
    k3_new_update = sprintf("k3_%s ~ %s", states, rates) |> lapply(as.formula)
    
    ## rk4 step 4
    state_replacements = sprintf("%s ~ (%s + %s)", states, states, step_names$k3) |> lapply(as.formula)
    k4_new_before = update_formulas(before, state_replacements)
    k4_new_update = sprintf("%s ~ %s", step_names$k4, rates) |> lapply(as.formula)
    
    ## final update step
    final_update = sprintf("%s ~ %s + (%s + 2 * %s + 2 * %s + %s)/6"
      , states, states, step_names$k1, step_names$k2, step_names$k3, step_names$k4
    ) |> lapply(as.formula) |> setNames(states)
    after_components = self$change_model$after_state()
    c(
        k1_new_before, k1_new_update
      , k2_new_before, k2_new_update
      , k3_new_before, k3_new_update
      , k4_new_before, k4_new_update
      , final_update, after_components
    )
  }
  self$after = function() self$change_model$after_loop()
  return_object(self, "EulerUpdateMethod")
}

RK4UpdateMethod = function(change_model) {
  self = UpdateMethod()
  self$change_model = change_model
  
  self$before = function() self$change_model$before_loop()
  self$during = function() {
    ## abbr func nms
    unlst = function(x) unlist(x, recursive = FALSE, use.names = FALSE)
    as_forms = function(x) lapply(x, as.formula)
    
    before_components = self$change_model$before_flows()
    flow_frame = self$change_model$flow_frame()
    components = flow_frame_to_absolute_flows(flow_frame)
    before_state = self$change_model$before_state()
    update_state = self$change_model$update_state()
    update_flows = self$change_model$update_flows() |> unlst()  ## line can be removed?
    
    new_update = list()
    new_before = list()
    
    states = vapply(update_state, lhs_char, character(1L))
    rates = vapply(update_state, rhs_char, character(1L))
    flows = flow_frame$change
    
    existing_names = self$change_model$all_user_aware_names()
    local_state_step_names = list(
        k1 = sprintf("k1_%s", states)
      , k2 = sprintf("k2_%s", states)
      , k3 = sprintf("k3_%s", states)
      , k4 = sprintf("k4_%s", states)
    )
    local_flow_step_names = list(
        k1 = sprintf("k1_%s", flows)
      , k2 = sprintf("k2_%s", flows)
      , k3 = sprintf("k3_%s", flows)
      , k4 = sprintf("k4_%s", flows)
    )
    state_step_names = map_names(existing_names, local_state_step_names)
    flow_step_names = map_names(existing_names, local_flow_step_names)
    
    rate_formulas = sprintf("%s ~ %s", state_step_names$k1, rates) |> as_forms()
    
    make_before = function(stage) {
      stage_flow_frame = within(flow_frame, change <- flow_step_names[[stage]])
      stage_components = flow_frame_to_absolute_flows(stage_flow_frame)
      is_k1 = stage == "k1"
      stage_before_components = only_iterable(before_components, states, is_k1)
      c(stage_before_components, stage_components)
    }
    
    ## rk4 step 1
    flow_replacements = sprintf("%s ~ %s", flows, flow_step_names$k1) |> as_forms()
    k1_new_before = make_before("k1")
    k1_new_update = update_formulas(rate_formulas, flow_replacements)
    
    ## rk4 step 2
    state_replacements = sprintf("%s ~ (%s + (%s / 2))", states, states, state_step_names$k1) |> as_forms()
    flow_replacements = sprintf("%s ~ %s", flows, flow_step_names$k2) |> as_forms()
    k2_new_before = update_formulas(make_before("k2"), state_replacements)
    k2_new_update = sprintf("%s ~ %s", state_step_names$k2, rates) |> as_forms() |> update_formulas(flow_replacements)
    
    ## rk4 step 3
    state_replacements = sprintf("%s ~ (%s + (%s / 2))", states, states, state_step_names$k2) |> as_forms()
    flow_replacements = sprintf("%s ~ %s", flows, flow_step_names$k3) |> as_forms()
    k3_new_before = update_formulas(make_before("k3"), state_replacements)
    k3_new_update = sprintf("%s ~ %s", state_step_names$k3, rates) |> as_forms() |> update_formulas(flow_replacements)
    
    ## rk4 step 4
    state_replacements = sprintf("%s ~ (%s + %s)", states, states, state_step_names$k3) |> as_forms()
    flow_replacements = sprintf("%s ~ %s", flows, flow_step_names$k4) |> as_forms()
    k4_new_before = update_formulas(make_before("k4"), state_replacements)
    k4_new_update = sprintf("%s ~ %s", state_step_names$k4, rates) |> as_forms() |> update_formulas(flow_replacements)
    
    ## final update step
    final_flow_update = sprintf("%s ~ (%s + 2 * %s + 2 * %s + %s)/6"
      , flows, flow_step_names$k1, flow_step_names$k2, flow_step_names$k3, flow_step_names$k4
    ) |> as_forms()
    final_state_update = sprintf("%s ~ %s %s", states, states, rates) |> as_forms() |> setNames(states)
    after_components = self$change_model$after_state()
    c(
        k1_new_before, k1_new_update
      , k2_new_before, k2_new_update
      , k3_new_before, k3_new_update
      , k4_new_before, k4_new_update
      , final_flow_update, final_state_update
      , after_components
    )
  }
  self$after = function() self$change_model$after_loop()
  return_object(self, "EulerUpdateMethod")
}

DiscreteStochUpdateMethod = function(change_model) {
  self = EulerMultinomialUpdateMethod(change_model)
  return_object(self, "DiscreteStochUpdateMethod")
}

EulerMultinomialUpdateMethod = function(change_model) {
  self = Base()
  self$change_model = change_model
  
  self$vec = function(expr_list, char_fun) {
    vec = vapply(expr_list, char_fun, character(1L))
    ## simple expressions are any non-formula strings (names of variables or state flows)
    ## expressions that are not simple contain math symbols (ex. +,-,*,/, etc.)
    ## BMB: allow non-ASCII alpha? "^[[:alpha:]0-9._]+$"
    simple_expr = all(grepl("^[a-zA-Z0-9._]+$", vec))
    scalar_expr = length(vec) == 1L
    
    if (simple_expr & scalar_expr) return(vec)
    if (!simple_expr | !scalar_expr) {
      vec = sprintf("(%s)", paste0(vec, collapse = ", "))
    }
    if (!scalar_expr) {
      vec = sprintf("c%s", vec)
    }
    return(vec)
  }
  
  self$before = function() self$change_model$before_loop()
  self$during = function() {
    before_components = c(
        self$change_model$before_flows()
      , to_exogenous(self$change_model$flow_frame(), rand_fn = "rpois")
    )
    flow_list = self$change_model$update_flows()
    components = list()
    for (size_var in names(flow_list)) {
      components[[size_var]] = sprintf("%s ~ reulermultinom(%s, %s)"
        , self$vec(flow_list[[size_var]], lhs_char)
        , size_var
        , self$vec(flow_list[[size_var]], rhs_char)
      )
    }
    ## BMB: add absolute flows here
    new_flow = lapply(components, as.formula)
    
    update = self$change_model$update_state()
    states = vapply(update, lhs_char, character(1L))
    rates = vapply(update, rhs_char, character(1L))
    update_char = sprintf("%s ~ %s %s", states, states, rates)
    new_update = lapply(update_char, as.formula)
    after_components = self$change_model$after_state()
    
    c(before_components, new_flow, new_update, after_components)
  }
  self$after = function() self$change_model$after_loop()
  return_object(self, "EulerMultinomialUpdateMethod")
}


HazardUpdateMethod = function(change_model) {
  self = EulerMultinomialUpdateMethod(change_model)
  self$during = function() {
    before_components = c(
        self$change_model$before_flows()
      , to_exogenous(self$change_model$flow_frame())
    )
    before_state = self$change_model$before_state()

    flow_list = self$change_model$update_flows()
    components = list()
    for (size_var in names(flow_list)) {
      components[[size_var]] = sprintf("%s ~ %s * (1 - exp(-sum(%s))) * proportions(%s, 0, %s)"
        , self$vec(flow_list[[size_var]], lhs_char)
        , size_var
        , self$vec(flow_list[[size_var]], rhs_char)
        , self$vec(flow_list[[size_var]], rhs_char)
        , getOption("macpan2_tol_hazard_div")
      )
    }
    new_flow = lapply(components, as.formula)
    
    update = self$change_model$update_state()
    states = vapply(update, lhs_char, character(1L))
    rates = vapply(update, rhs_char, character(1L))
    update_char = sprintf("%s ~ %s %s", states, states, rates)
    new_update = lapply(update_char, as.formula)
    
    after_components = self$change_model$after_state()
    
    c(before_components, before_state, new_flow, new_update, after_components)
  }
  return_object(self, "HazardUpdateMethodUpdateMethod")
}


# Change Components

#' Specify Flow Between Compartments
#' 
#' Specify different kinds of flows between compartments.
#' 
#' The examples below can be mixed and matched in `mp_tmb_model_spec()`
#' to produce compartmental models. The symbols used below must
#' be used in an appropriate context (e.g., if `N` is used for total population
#' size, then there must be an expression like `N ~ S + I + R` somewhere in
#' the model or for models with constant population size there must be a 
#' default variable, `N`, with a numerical value).
#' 
#' @param from String giving the name of the compartment from which the flow
#' originates.
#' @param to String giving the name of the compartment to which the flow is
#' going.
#' @param rate String giving the expression for the per-capita
#' flow rate. Alternatively, and for back compatibility, 
#' a two-sided formula with the left-hand-side giving the name of the absolute 
#' flow rate per time-step and the right-hand-side giving an expression for 
#' the per-capita rate of flow from `from` to `to`.
#' @param flow_name String giving the name for the absolute flow rate per
#' time-step. By default, during simulations, the absolute flow rate will be 
#' computed as `from * rate`. This default behaviour will simulate the 
#' compartmental model as discrete difference equations, but this can 
#' be changed to use other approaches such as ordinary differential equations
#' or stochastic models (see \code{\link{state_updates}}). If a formula is 
#' passed to `rate` (not recommended for better readability), then this 
#' `flow_rate` argument will be ignored. 
#' @param abs_rate Deprecated synonym for `flow_name`. Please use `flow_name`
#' in all future work.
#' 
#' @seealso [mp_absolute_flow()]
#' 
#' @examples
#' 
#' # infection by mass action
#' # https://github.com/canmod/macpan2/blob/main/inst/starter_models/si
#' mp_per_capita_flow("S", "I", "beta * I / N", "infection")
#' 
#' # recovery
#' # https://github.com/canmod/macpan2/blob/main/inst/starter_models/sir
#' mp_per_capita_flow("I", "R", "gamma", "recovery")
#' 
#' # disease progression with different severity
#' # https://github.com/canmod/macpan2/blob/main/inst/starter_models/macpan_base
#' mp_per_capita_flow("E", "I_mild", "alpha * phi"      , "progression_mild")
#' mp_per_capita_flow("E", "I_sev" , "alpha * (1 - phi)", "progression_sev")
#' 
#' # birth
#' # https://github.com/canmod/macpan2/blob/main/inst/starter_models/sir_demog
#' mp_per_capita_inflow("N", "S", "nu", "birth")
#' 
#' # death
#' # https://github.com/canmod/macpan2/blob/main/inst/starter_models/sir_demog
#' mp_per_capita_outflow("S", "mu", "death_S")
#' mp_per_capita_outflow("I", "mu", "death_I")
#' mp_per_capita_outflow("R", "mu", "death_R")
#' 
#' # vaccination 
#' # https://github.com/canmod/macpan2/blob/main/inst/starter_models/shiver
#' mp_per_capita_flow("S", "V", "((a * S)/(b + S))/S",  "vaccination")
#' 
#' # importation (experimental)
#' # mp_absolute_inflow("I", "delta", "importation")
#' 
#' @export
mp_per_capita_flow = function(from, to, rate, flow_name = NULL, abs_rate = NULL) {
  call_string = deparse(match.call())
  rate = handle_rate_args(rate, abs_rate, flow_name)
  PerCapitaFlow(from, to, rate, call_string)
}

#' @describeIn mp_per_capita_flow Only flow into the `to` compartment, and
#' do not flow out of the `from` compartment. The `from` compartment can even
#' be a function of a set of compartments, because it will not be updated. A 
#' common construction is `mp_per_capita_inflow("N", "S", "birth_rate", "birth")`
#' for adding a birth process, which involves the total population size, `N`,
#' rather than a single compartment.
#' @export
mp_per_capita_inflow = function(from, to, rate, flow_name = NULL, abs_rate = NULL) {
  call_string = deparse(match.call())
  rate = handle_rate_args(rate, abs_rate, flow_name)
  PerCapitaInflow(from, to, rate, call_string)
}

#' @describeIn mp_per_capita_flow Only flow into the `to` compartment
#' For adding a birth or immigration process
#' @param flow_name String giving the name of the flow
#' @export
mp_inflow = function(to, rate, flow_name  = NULL, abs_rate = NULL) {
  call_string = deparse(match.call())
  rate = handle_rate_args(rate, abs_rate, flow_name)
  AbsoluteInflow(to, rate, call_string)
}

#' @describeIn mp_per_capita_flow Only flow into the `to` compartment
#' For adding an absolute removal process that goes to 'nowhere': dangerous!
#' @export
mp_outflow = function(from, rate, flow_name = NULL, abs_rate = NULL) {
  call_string = deparse(match.call())
  rate = handle_rate_args(rate, abs_rate, flow_name)
  AbsoluteOutflow(from, rate, call_string)
}

#' @describeIn mp_per_capita_flow Only flow out of the `from` compartment,
#' without going anywhere. This is useful for removing individuals from the 
#' system (e.g., death). To keep track of the total number of dead individuals
#' one can use `mp_per_capita_flow` and set `to` to be a compartment for
#' these individuals (e.g., `to = "D"`).
#' @export
mp_per_capita_outflow = function(from, rate, flow_name = NULL, abs_rate = NULL) {
  call_string = deparse(match.call())
  rate = handle_rate_args(rate, abs_rate, flow_name)
  PerCapitaOutflow(from, rate, call_string)
}




#' Specify Absolute Flow Between Compartments (Experimental)
#' 
#' An experimental alternative to \code{\link{mp_per_capita_flow}} that 
#' allows users to specify flows using absolute rates instead of 
#' per-capita rates.
#' 
#' @param from String giving the name of the compartment from which the flow
#' originates.
#' @param to String giving the name of the compartment to which the flow is
#' going.
#' @param rate String giving the expression for the absolute
#' flow rate per time-step.
#' @param flow_name String giving the name for the variable that 
#' will store the `rate`.
#' @param rate_name Deprecated synonym for `flow_name`. Please use `flow_name`
#' in all future work.
#' 
#' @seealso [mp_per_capita_flow()]
#' 
#' @export
mp_absolute_flow = function(from, to, rate, flow_name = NULL, rate_name = NULL) {
  call_string = deparse(match.call())
  rate = handle_abs_rate_args(rate, rate_name, flow_name)
  AbsoluteFlow(from, to, rate, call_string)
}

PerCapitaOutflow = function(from, rate, call_string) {
  self = PerCapitaFlow(from, NULL, rate, call_string)
  self$change_frame = function() {
    data.frame(
        state = self$from
      , change = sprintf("-%s", lhs_char(self$rate))
    )
  }
  return_object(self, "PerCapitaOutflow")
}

PerCapitaInflow = function(from, to, rate, call_string) {
  self = PerCapitaFlow(from, to, rate, call_string)
  self$change_frame = function() {
    data.frame(
        state = self$to
      , change = sprintf("+%s", lhs_char(self$rate))
    )
  }
  return_object(self, "PerCapitaInflow")
}

AbsoluteInflow = function(to, rate, call_string) {
  self = AbsoluteFlow(NULL, to, rate, call_string)
  self$change_frame = function() {
    data.frame(
        state = self$to
      , change = sprintf("+%s", lhs_char(self$rate))
    )
  }
  self$string = function() self$call_string
  return_object(self, "AbsoluteInflow")
}

AbsoluteOutflow = function(from, rate, call_string) {
  self = AbsoluteFlow(from, NULL, rate, call_string)
  self$change_frame = function() {
    data.frame(
        state = self$from
      , change = sprintf("-%s", lhs_char(self$rate))
    )
  }
  self$string = function() self$call_string
  return_object(self, "AbsoluteOutflow")
}

AbsoluteFlow = function(from, to, rate, call_string) {
  self = ChangeComponent()
  self$from = from
  self$to = to
  self$rate = rate
  self$call_string = call_string
  self$change_frame = function() {
    data.frame(
        state = c(self$from, self$to)
      , change = sprintf("%s%s", c("-", "+"), lhs_char(self$rate))
    )
  }
  self$flow_frame = function() {
    abs_rate = rhs_char(self$rate)
    data.frame(
        ## BMB: not sure if this is right? there is no 'size'
        size = "" ## self$from %||% ""
      , change = lhs_char(self$rate)
      
      ## this is the main problem with absolute flows, because it has a 
      ## `size` variable (e.g., a `from` variable) in the denominator.
      ## this issue should only arise for update methods that are more 
      ## naturally expressed for per-capita flows (e.g., Euler-multinomial
      ## and hazard)
      ## BMB: why does there need to be a 'from' involved here at all?
      ##  is this only an issue because we need a Poisson-type stochastic
      ##  flow to go with the Euler-multinomial?
      , rate = sprintf("%s", abs_rate)
      , abs_rate = abs_rate
    )
  }
  return_object(self, "AbsoluteFlow")
}

PerCapitaFlow = function(from, to, rate, call_string) {
  self = ChangeComponent()
  self$from = from
  self$to = to
  self$rate = rate ## formula with flow_name ~ per_capita_rate
  self$call_string = call_string
  self$change_frame = function() {
    data.frame(
        state = c(self$from, self$to)
      , change = sprintf("%s%s", c("-", "+"), lhs_char(self$rate))
    )
  }
  self$flow_frame = function() {
    per_capita = rhs_char(self$rate)
    data.frame(
        size = self$from
      , change = lhs_char(self$rate)
      , rate = per_capita
      , abs_rate = sprintf("%s * (%s)", self$from, per_capita)
    )
  }
  self$string = function() self$call_string
  return_object(self, "PerCapitaFlow")
}


##' Formula
##' 
##' Wrap a two-sided formula so that it can be used as a change component.
##' Developer use only.
##' 
##' @noRd
Formula = function(formula) {
  if (is_one_sided(formula)) stop("Raw R formulas in an expression list must be two-sided")
  self = ChangeComponent()
  self$formula = formula
  self$user_formulas = function() list(self$formula)
  return_object(self, "Formula")
}


#' @noRd
to_change_component = function(x) UseMethod("to_change_component")

#' @export
to_change_component.ChangeComponent = function(x) x

#' @export
to_change_component.formula = function(x) Formula(x)


#' @describeIn mp_expand Confusingly, `mp_reduce` and `mp_expand` are synonyms.
#' Please use `mp_expand` in new projects, as `mp_reduce` is available for 
#' back-compatibility only.
#' @export
mp_reduce = function(model) UseMethod("mp_reduce")

#' @export
mp_reduce.TMBModelSpec = function(model) model$expand()
