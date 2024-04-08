##' @param formula A formula with index names.
##' @param by Like in `mp_join`
##' @param index_list Outer list associated with names of vectors in the
##' engine, and inner list of subset indices of these vectors.
##' @param existing_global_names
##' @noRd
StructuredFormula = function(formula, by, index_list, existing_global_names = character()) {
  self = Base()
  
  self$formula = formula
  self$by = by
  self$index_list = index_list
  self$existing_global_names = existing_global_names
  
  self$global_names = function() {
    macpan2:::map_names(self$existing_global_names, self$local_names())
  }
  self$global_names_vector = function() {
    c(
      self$existing_global_names, 
      unlist(self$global_names(), recursive = TRUE, use.names = FALSE)
    )
  }
  
  ## TODO: name conflict avoidance. probably need to separate lhs and rhs integers
  ## 
  self$lhs_var = function() macpan2:::formula_vars(self$formula, side = "left")
  self$rhs_vars = function() macpan2:::formula_vars(self$formula, side = "right")
  self$indexes_in_formula = function() {
    mp_lookup_in_list(
        self$index_list
      , c(self$lhs_var(), self$rhs_vars())
    )
  }
  self$vector_names = function() attr(self$indexes_in_formula(), "vectors")
  self$ledger = function() {
    if (is.null(self$by)) {
      ## default to all shared columns -- makes the smallest result set
      ## and also makes it easy to specify indexes that are already aligned
      by = (self$indexes_in_formula()
        |> lapply(names)
        |> Reduce(f = intersect)
        |> to_name()
      )
    } else {
      by = self$by
    }
    do.call(mp_join, c(self$indexes_in_formula(), list(by = by)))
  }
  self$lhs_integers = function() {
    ledger = self$ledger()
    pfuns = ledger$positions_for[self$lhs_var()]
    local_names = sprintf("%s_groups", self$lhs_var())
    (pfuns
      |> lapply(\(x) x(zero_based = TRUE))
      |> setNames(local_names)
    )
  }
  self$rhs_integers = function() {
    ledger = self$ledger()
    pfuns = ledger$positions_for[self$rhs_vars()]
    vector_nms = self$vector_names()
    lhs_nm = abbreviate(self$lhs_var())
    rhs_nms = abbreviate(self$rhs_vars())
    #vec_nms = vector_nms[rhs_nms]
    from_nms = sprintf("%s_", rhs_nms)
    #from_nms[vec_nms == rhs_nms] = ""
    local_names = sprintf("%sto_%s", from_nms, lhs_nm)
    (pfuns
      |> lapply(\(x) x(zero_based = TRUE))
      |> setNames(local_names)
    )
  }
  self$integers = function() {
    c(self$lhs_integers(), self$rhs_integers())
  }
  self$updated_formula = function() {
    global_nms = self$global_names()
    vector_nms = self$vector_names()
    lhs_nm = self$lhs_var()
    rhs_nms = self$rhs_vars()
    vec_nms = vector_nms[rhs_nms]
    replacers = sprintf(
        "%s ~ %s[%s]"
      , rhs_nms
      , vec_nms
      , global_nms$rhs_integers
    ) |> lapply(as.formula)
    formula = macpan2:::update_formula(self$formula, replacers)
    
    sprintf("%s ~ group_sums(%s, %s, %s)"
      , lhs_nm
      , macpan2:::rhs_char(formula)
      , global_nms$lhs_integers
      , lhs_nm
    ) |> as.formula()
  }
  
  self$local_names = function() {
    macpan2:::make_names_list(self
      , c("lhs_integers", "rhs_integers")
      , prefix_with_meth_nms = FALSE
    )
  }
  
  return_object(self, "StructuredFormula")
}



StructuredFormulaSpec = function(formula, by = NULL) {
  self = Base()
  self$formula = formula
  self$by = by
  
  self$render = function(
        index_list
      , existing_global_names = character()
    ) {
    
    ## this if structure is to handle the case where
    ## mp_by has optional by-conditions for each index/table.
    ## in this case the missing by-condition(s) is(are) 
    ## assumed to mean that the tables/indices mentioned are 
    ## joined by all common columns.
    by = self$by
    # for (i in seq_along(self$by)) {
    #   if (inherits(self$by, "By")) {
    #     if (is.null(self$by$by_x) | is.null(self$by$by_y)) {
    #       if (is.null(self$by$by_x)) {
    #         x_nms = index_list[[self$by$name_x]] |> to_names()
    #       } else {
    #         x_nms = self$by$by_x
    #       }
    #       if (is.null(self$by$by_y)) {
    #         y_nms = index_list[[self$by$name_y]] |> to_names()
    #       } else {
    #         y_nms = self$by$by_y
    #       }
    #       by = c(x_nms, y_nms) |> unique() |> to_name()
    #     }
    #   }
    # }
    
    StructuredFormula(self$formula, by, index_list, existing_global_names)
  }
  return_object(self, "StructuredFormulaSpec")
}


##' Structured Formula
##' 
##' @param formula An unstructured formula to be turned into a structured one.
##' @param by See \code{\link{mp_join}}.
##'
##' @export
mp_struc = StructuredFormulaSpec
