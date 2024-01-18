#' @export
Quantities = function(state_variables, flow_rates, labelling_descriptors) {
  self = Base()
  self$names = union(state_variables$names(), flow_rates$names())
  self$name = to_name(self$names)
  self$state_variables = state_variables$expand(self$name)
  self$flow_rates = flow_rates$expand(self$name)
  self$labelling_descriptors = labelling_descriptors
  self$labelled_subset = function(vector, name, ...) {

  }
  self$from = function(..., filter_by) {
    ## cases:
    if (missing(filter_by) & length(list(...)) == 0L) s = self$state_variables
    if (!missing(filter_by) & length(list(...)) == 0L)
    if (!missing(filter_by)) s = s$filter(..., .wrt = filter_by)
    labelled_frame(s
      , label_by = self$labelling_descriptors
      , label_name = "from"
    )
  }
  self$to = function(..., filter_by) {
    s = self$state_variables
    if (!missing(filter_by)) s = s$filter(..., .wrt = filter_by)
    labelled_frame(s
      , label_by = self$labelling_descriptors
      , label_name = "to"
    )
  }
  self$infectious = function(..., filter_by) {

  }
  self$flow = function(..., filter_by) {
    f = self$flow_rates
    if (!missing(filter_by)) f = f$filter(..., .wrt = filter_by)
    labelled_frame(f
      , label_by = self$labelling_descriptors
      , label_name = "flow"
    )
  }
  self$join = join_partitions
  self$join3 = function(x, y, z, by1 = "", by2 = "") {
    self$join(x, y, by1) |> self$join(z, by2)
  }
  self$flow_mechanism = function(x, y, z, by1 = "", by2 = "", type = "per_capita") {
    z = self$join3(x, y, z, by1, by2)[, c("from", "to", "flow"), drop = FALSE]
    z$type = type
    z
  }
  return_object(self, "Quantities")
}

#' @export
print.Quantities = function(x, ...) {
  print(union_vars(x$state_variables, x$flow_rates))
}

quantities = function(..., .labelling_descriptors) {
  l = list(...)
  if (missing(.labelling_descriptors)) .labelling_descriptors = names(l)
  data.frame(...) |> Quantities(.labelling_descriptors)
}

# A = function(x) {
#   self = Base()
#   self$x = x
#   self$f = function(y) self$x * y
#   return_object(self, "A")
# }
# B = function(x, y_default) {
#   self = A(x)
#   formals(self$f)$y = y_default
#   return_object(self, "B")
# }
# a = A(100)
# b = B(100, 300)
# a$f(300)
# b$f()
