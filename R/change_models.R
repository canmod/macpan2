
ChangeModelDefaults = function(delta_t) {
  self = ChangeModel()
  
  self$delta_t = delta_t
  
  self$change_list = list()
  
  self$flow_frame = function() {
    cl = self$change_list
    if (length(cl) == 0L) return(self$empty_flow_frame)
    (cl
      |> method_apply("flow_frame")
      |> bind_rows()
    )
  }
  self$change_frame = function() {
    cl = self$change_list
    if (length(cl) == 0L) return(self$empty_change_frame)
    (cl
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
  self$duplicated_change_names = function() {
    frame = self$flow_frame()
    dups = frame$change[duplicated(frame$change)]
    return(dups)
  }
  self$check = function() {
    dups = self$duplicated_change_names()
    if (length(dups) > 0L) {
      stop(
          "The following names are duplicates:\n   "
        , paste(dups, collapse = "   \n")
      )
    }
    NULL
  }
  return_object(self, "ChangeModelDefaults")
}
