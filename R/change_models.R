
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
