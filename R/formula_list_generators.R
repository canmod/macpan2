


handle_rate_args = function(rate, abs_rate = NULL) {
  if (!is_two_sided(rate)) {
    if (is_one_sided(rate)) rate = rhs_char(rate)
      if (is.null(abs_rate)) stop("abs_rate must be specified when rate is a one_sided formula.")
    rate = two_sided(abs_rate, rate)
  }
  return(rate)
}

only_iterable = function(expr_list, states) {
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
  
  self$before_loop = function() list()
  self$once_start = function() list()
  self$before_flows = function() list()
  self$update_flows = function() list()
  self$before_state = function() list()
  self$update_state = function() list()
  self$after_state = function() list()
  self$once_finish = function() list()
  self$after_loop = function() list()
  
  # one row per flow
  #  size : aka name of the from compartment
  #  change : absolute flow rate name
  #  rate : string with expression for the per-capita flow rate
  self$flow_frame = function() empty_frame("size", "change", "rate")
  
  # one row per term in a state update expression
  #  state : name of the state being updated
  #  change : string with the term in the expression that updates that state
  self$change_frame = function() empty_frame("state", "change")
  
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
  ## column - change: unsigned absolute flow rates.
  ## column - rate: per-capita flow rates (variables or expresions that 
  ## sometimes involve state variables).
  ## example:
  ## size, change,    rate
  ## S,    infection, beta * I / N
  ## I,    recovery,  gamma
  ## N,    birth,     mu
  self$flow_frame = function() empty_frame("size", "change", "rate")
  
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
  
  self$update_flows = function() {
    frame = self$flow_frame()
    size_vars = unique(frame$size)
    flow_list = list()
    formulas = sprintf("%s ~ %s", frame$change, frame$rate) |> lapply(as.formula)
    for (var in size_vars) {
      flow_list[[var]] = formulas[frame$size == var]
    }
    flow_list
  }
  self$update_state = function() {
    frame = self$change_frame()
    exprs = tapply(frame$change, frame$state, paste0, collapse = " ", simplify = TRUE)[unique(frame$state)]
    sprintf("%s ~ %s", names(exprs), unname(exprs)) |> lapply(as.formula)
  }
  self$before_loop = function() self$before
  self$after_loop = function() self$after
  
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


##' State Updates (experimental)
##' 
##' Use these functions to update a model spec so that the state variables
##' are updated according to different numerical methods.
##' 
##' @param model Object with quantities that have been explicitly 
##' marked as state variables.
##' @export
mp_euler = function(model) UseMethod("mp_euler")

##' @describeIn mp_euler ODE solver using Runge-Kutta 4. Any formulas that
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
##' be confused. We therefore require that all state variables updates are set
##' explicitly (e.g., with \code{\link{mp_per_capita_flow}}) if any are explicit.
##' @export
mp_rk4 = function(model) UseMethod("mp_rk4")

##' @describeIn mp_euler Update state with process error given by the 
##' Euler-multinomial distribution. 
##' @export
mp_euler_multinomial = function(model) UseMethod("mp_euler_multinomial")

##' @describeIn mp_euler Update state with hazard steps, which is equivalent
##' to taking the step given by the expected value of the Euler-multinomial
##' distribution.
##' @export
mp_hazard = function(model) UseMethod("mp_hazard")

##' @export
mp_euler.TMBModelSpec = function(model) model$change_update_method("euler")

##' @export
mp_rk4.TMBModelSpec = function(model) model$change_update_method("rk4")

##' @export
mp_euler_multinomial.TMBModelSpec = function(model) model$change_update_method("euler_multinomial")

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
  if (is_two_sided(x)) return(list(x))
  if (inherits(x, "ChangeComponent")) return(list(x))
  if (!is.list(x)) {
    ## TODO: should make more sense!
    stop("Argument must be a formula, change component, or a list of such objects.")
  }
  ## TODO: check that we have a list of valid components
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
  
  ## nb: euler method requires no additional names from the spec
  #self$existing_global_names = existing_global_names
  
  
  self$before = function() self$change_model$before_loop()
  self$during = function() {
    before_components = self$change_model$before_flows()
    components = to_absolute_flows(self$change_model$update_flows())
    before_update = self$change_model$before_state()
    update = self$change_model$update_state()
    
    states = vapply(update, lhs_char, character(1L))
    rates = vapply(update, rhs_char, character(1L))
    update_char = sprintf("%s ~ %s %s", states, states, rates)
    new_update = lapply(update_char, as.formula)
    after_components = self$change_model$after_state()
    c(before_components, components, before_update, new_update, after_components)
  }
  self$after = function() self$change_model$after_loop()
  return_object(self, "EulerUpdateMethod")
}


RK4UpdateMethod = function(change_model) {
  self = UpdateMethod()
  self$change_model = change_model
  
  self$before = function() self$change_model$before_loop()
  self$during = function() {
    before_components = self$change_model$before_flows()
    components = to_absolute_flows(self$change_model$update_flows())
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


EulerMultinomialUpdateMethod = function(change_model) {
  self = Base()
  self$change_model = change_model
  
  self$vec = function(expr_list, char_fun) {
    vec = vapply(expr_list, char_fun, character(1L))
    # simple expressions are any non-formula strings (names of variables or state flows)
    # expressions that are not simple contain math symbols (ex. +,-,*,/, etc.)
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
    before_components = self$change_model$before_flows()
    flow_list = self$change_model$update_flows()
    components = list()
    for (size_var in names(flow_list)) {
      components[[size_var]] = sprintf("%s ~ reulermultinom(%s, %s)"
        , self$vec(flow_list[[size_var]], lhs_char)
        , size_var
        , self$vec(flow_list[[size_var]], rhs_char)
      )
    }
    new_flow = lapply(components, as.formula)
    
    update = self$change_model$update_state()
    states = vapply(update, lhs_char, character(1L))
    rates = vapply(update, rhs_char, character(1L))
    update_char = sprintf("%s ~ %s %s", states, states, rates)
    new_update = lapply(update_char, as.formula)
    
    c(before_components, new_flow, new_update)
  }
  self$after = function() self$change_model$after_loop()
  return_object(self, "EulerMultinomialUpdateMethod")
}


HazardUpdateMethod = function(change_model) {
  self = EulerMultinomialUpdateMethod(change_model)
  self$during = function() {
    before_components = self$change_model$before_flows()
    before_state = self$change_model$before_state()

    flow_list = self$change_model$update_flows()
    components = list()
    for (size_var in names(flow_list)) {
      components[[size_var]] = sprintf("%s ~ %s * (1 - exp(-sum(%s))) * proportions(%s, 0, 1e-8)"
        , self$vec(flow_list[[size_var]], lhs_char)
        , size_var
        , self$vec(flow_list[[size_var]], rhs_char)
        , self$vec(flow_list[[size_var]], rhs_char)
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

#' Per-Capita Flow (Experimental)
#' 
#' @param from String giving the name of the compartment from which the flow
#' originates.
#' @param to String giving the name of the compartment to which the flow is
#' going.
#' @param rate String giving the expression for the per-capita flow rate.
#' Alternatively, for back compatibility, a two-sided formula with the
#' left-hand-side giving the name of the absolute flow rate per unit time-step 
#' and the right-hand-side giving an expression for the per-capita rate of 
#' flow from `from` to `to`.
#' @param abs_rate String giving the name for the absolute flow rate,
#' which will be computed as `from * rate`. If a formula is passed to
#' `rate` (not recommended), then this `abs_rate` argument will be ignored.
#' 
#' @export
mp_per_capita_flow = function(from, to, rate, abs_rate = NULL) {
  call_string = deparse(match.call())
  rate = handle_rate_args(rate, abs_rate)
  PerCapitaFlow(from, to, rate, call_string)
}

#' @describeIn mp_per_capita_flow Only flow into the `to` compartment, and
#' do not flow out of the `from` compartment.
#' @export
mp_per_capita_inflow = function(from, to, rate, abs_rate = NULL) {
  call_string = deparse(match.call())
  rate = handle_rate_args(rate, abs_rate)
  PerCapitaInflow(from, to, rate, call_string)
}


PerCapitaInflow = function(from, to, rate, call_string) {
  self = PerCapitaFlow(from, to, rate, call_string)
  self$change_frame = function() {
    data.frame(
        state = self$to
      , change = sprintf("%s%s", "+", lhs_char(self$rate))
    )
  }
  return_object(self, "PerCapitaInflow")
}

PerCapitaFlow = function(from, to, rate, call_string) {
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
    data.frame(
        size = self$from
      , change = lhs_char(self$rate)
      , rate = rhs_char(self$rate)
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


#' Reduce Model
#' 
#' Reduce a model by removing any model structure 
#' (e.g. \code{\link{mp_per_capita_flow}}), so that expression lists
#' are plain R formulas.
#' 
#' @param model A model object.
#'
#' @export
mp_reduce = function(model) UseMethod("mp_reduce")

#' @export
mp_reduce.TMBModelSpec = function(model) model$expand()
