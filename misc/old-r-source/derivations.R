# Derivation Grouping
#
# Group together derivations into those with identical filtering and
# grouping specs. This class avoids filtering Partitions repeatedly in the
# same way, which is helpful for product models e.g. several derivations
# are age-stratified in the same way.
#
# @param model Object of class Model
DerivationGrouping = function(model) {
  self = Base()
  self$model = model

  ## number of derivations in the model
  self$n_derivations = function() length(self$model$def$derivations())

  ## list of spec lists, which are lists containing any of the following fields:
  ## "filter_partition", "filter_names", "group_partition", "group_names"
  self$specs = function() {
    get_fields = function(x) {
      fields = c("filter_partition", "filter_names", "group_partition", "group_names")
      l = setNames(vector(mode = "list", length = length(fields)), fields)
      for (f in fields) l[[f]] = x[[f]]
      l
    }
    lapply(self$model$def$derivations(), get_fields)
  }

  ## list of unique specs
  self$unique_specs = function() unique(self$specs())

  ## integer vector of length self$n_derivations, giving the id for each
  ## derivation into self$unique_specs()
  self$spec_map = function() {
    l = integer(self$n_derivations())
    s = self$specs()
    us = self$unique_specs()
    for (i in seq_along(s)) for (j in seq_along(us)) {
        if (identical(s[[i]], us[[j]])) l[i] = j
    }
    l
  }

  ## list of unique partitions
  self$unique_var_lists = function() {
     all_vars = self$model$variables$all()
     specs = self$unique_specs()
     l = list()
     for (i in seq_along(specs)) {
       if (is.null(specs[[i]]$filter_partition)) {
         filtered_vars = all_vars
       } else {
         filtered_vars = all_vars$filter(
           specs[[i]]$filter_names,
           .wrt = specs[[i]]$filter_partition
         )
       }
       l[[i]] = list()
       if (is.null(specs[[i]]$group_partition)) {
         grp_nm = specs[[i]]$filter_partition
         if (is.null(grp_nm)) grp_nm = "all"
         l[[i]][[grp_nm]] = filtered_vars
       } else {
         for (g in specs[[i]]$group_names) {
            l[[i]][[g]] = filtered_vars$filter(g
              , .wrt = specs[[i]]$group_partition
              , .comparison_function = all_consistent
            )
         }
       }
     }
     l
  }

  ## get the variable list partitions by derivation_id and looking up the
  ## result in self$unique_var_lists() using the self$spec_map(). the result
  ## is a list with one element per grouping giving the partition for that
  ## group
  self$var_list = function(derivation_id) {
    self$unique_var_lists()[[self$spec_map()[derivation_id]]]
  }

  initialize_cache(self, "n_derivations", "specs", "unique_specs", "spec_map"
    , "unique_var_lists"
  )
  return_object(self, "DerivationGrouping")
}

Derivation = function(derivation_id, model) {
  self = Base()
  self$model = model
  self$derivation_grouping = DerivationGrouping(model)
  self$derivation_id = derivation_id
  self$component_list = function() {
    list(
      state = self$model$labels$state(),
      flow = self$model$labels$flow()
    )
  }
  self$get = function() self$model$def$derivations()[[self$derivation_id]]
  self$output_partition = function() {
    p = self$get()$output_partition
    if (is.null(p)) p = self$model$settings$name()
    p
  }
  self$input_partition = function() {
    p = self$get()$input_partition
    if (is.null(p)) p = self$model$settings$name()
    p
  }
  self$do_lookup_output_names = function() {
    self$output_partition() != self$model$settings$name()
  }
  self$output_labels = function() {
    if (!self$do_lookup_output_names()) return(self$get()$output_names)
    l = self$get()$output_names
    for (i in seq_along(self$var_list)) {
      l[i] = self$var_list()[[i]]$filter(l[i], .wrt = self$output_partition())$labels()
    }
    l
  }
  self$arguments = function() {
    args = self$get()$arguments
    if (is.null(args)) args = character()
    l = self$var_list()
    if (length(args) == 0L) {
      for (i in seq_along(l)) l[[i]] = args
    } else {
      for (i in seq_along(l)) {
        l[[i]] = l[[i]]$filter(args, .wrt = self$input_partition())$labels()
      }
    }
    l
  }
  self$argument_dots = function() {
    arg_dots = self$get()$argument_dots
    if (is.null(arg_dots)) arg_dots = character()
    l = self$var_list()
    if (length(arg_dots) == 0L) {
      for (i in seq_along(l)) l[[i]] = arg_dots
    } else {
      for (i in seq_along(l)) {
        l[[i]] = l[[i]]$filter(arg_dots, .wrt = self$input_partition())$labels()
      }
    }
    l
  }
  self$all_arguments = function() {
    mapply(c, self$arguments(), self$argument_dots(), SIMPLIFY = FALSE)
  }
  self$var_list = function() self$derivation_grouping$var_list(self$derivation_id)
  self$math = function() {
    args = c(self$get()$arguments, self$get()$argument_dots)
    dots_true = !is.null(self$get()$argument_dots)
    MathExpressionFromStrings(self$get()$expression, args, dots_true)$symbolic$evaluate
  }
  self$expr_list = function() {
    l = setNames(
      vector(mode = "list", length = length(self$output_labels())),
      sprintf("derivation_%s_%s", self$derivation_id, self$output_labels())
    )
    for (i in seq_along(l)) {
      l[[i]] = do.call(self$math(), as.list(self$all_arguments()[[i]]))
      l[[i]] = (two_sided(self$output_labels()[i], l[[i]])
        |> to_special_vecs(
            self$component_list()
          , c("state", "flow", self$model$labels$other())
          , c(state = "state", flow = "flow")
        )
        |> to_assign()
      )
    }
    l
  }
  initialize_cache(self, "get", "output_partition", "input_partition"
    , "do_lookup_output_names", "output_labels", "arguments", "argument_dots"
    , "all_arguments", "var_list", "math", "expr_list"
  )
  return_object(self, "Derivation")
}

#' @export
Derivations = function(model) {
  self = Base()
  self$model = model
  self$derivation_grouping = DerivationGrouping(model)
  self$derivations = lapply(1:self$derivation_grouping$n_derivations(), Derivation, self$model)
  self$.expr_list_by_phase = function(phase) {
    l = list()
    for (i in 1:self$derivation_grouping$n_derivations()) {
      if (self$derivations[[i]]$get()$simulation_phase == phase) {
        l = append(l, self$derivations[[i]]$expr_list())
      }
    }
    l
  }
  self$before = function() self$.expr_list_by_phase("before")
  self$during_pre_update = function() self$.expr_list_by_phase("during_pre_update")
  self$during_update = function() self$.expr_list_by_phase("during_update")
  self$during_post_update = function() self$.expr_list_by_phase("during_post_update")
  self$during = function() c(self$during_pre_update(), self$during_update(), self$during_post_update())
  self$after = function() self$.expr_list_by_phase("after")
  return_object(self, "Derivations")
}
