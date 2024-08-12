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
