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

ChangeModelDefaults = function() {
  self = ChangeModel()
  
  self$flow_frame = function() {
    ## TODO: check for change_list
    (self$change_list
      |> method_apply("flow_frame")
      |> bind_rows()
    )
  }
  self$change_frame = function() {
    (self$change_list
      |> method_apply("change_frame")
      |> bind_rows()
    )
  }
  self$change_classes = function() {
    (self$change_list
      |> lapply(class)
      |> vapply(getElement, character(1L), 1L)
    )
  }
  self$user_formulas = function() {
    method_apply(self$change_list, "user_formulas")
  }
  self$all_user_aware_names = function() {
    quasi_during_exprs = c(
      unlist(self$flow_frame(), use.names = FALSE, recursive = FALSE), 
      unlist(self$change_frame(), use.names = FALSE, recursive = FALSE)
    ) |> unique() |> sprintf(fmt = " ~ %s") |> lapply(as.formula)
    user_formulas = unlist(self$user_formulas()
      , recursive = FALSE
      , use.names = FALSE
    )
    exprs = c(
        self$before_loop()
      , quasi_during_exprs
      , user_formulas
      , self$after_loop()
    )
    (exprs
      |> lapply(formula_components, side = "both")
      |> lapply(getElement, "variables")
      |> unlist(use.names = FALSE, recursive = FALSE)
      |> unique()
    )
  }
  return_object(self, "ChangeModelDefaults")
}

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
