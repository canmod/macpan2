Dependencies = function(container, ...) {
  self = Base()
  self$container = container
  self$component_classes = list(...)
  self$rev_deps = function() {
    l = list()
    for (component in names(self$component_classes)) {
      l[[component]] = list(simple = character(0L), custom = character(0L))
      contents = ls(self$container[[component]])
      for (d in setdiff(names(self$component_classes), component)) {
        custom = sprintf("refresh_%s", component) %in% ls(self$container[[d]])
        right_cls = inherits(self$container[[d]][[component]], self$component_classes[[component]])
        containment = component %in% ls(self$container[[d]])
        if (custom) {
          l[[component]]$custom = append(l[[component]]$custom, d)
        } else if (right_cls & containment) {
          l[[component]]$simple = append(l[[component]]$simple, d)
        }
      }
    }
    l
  }
  self$deps = function() {
    l = list()
    for (component in ls(self$component_classes)) {
      l[[component]] = character(0L)
      object = self$container[[component]]
      for (d in ls(self$component_classes)) {
        containment = d %in% ls(object)
        right_cls = inherits(object[[d]], self$component_classes[[d]])
        custom_meth = sprintf("refresh_%s", d) %in% ls(object)
        if ((containment & right_cls) | custom_meth) {
          l[[component]] = append(l[[component]], d)
        }
      }
    }
    l
  }
  return_object(self, "Dependencies")
}

Refresher = function(dependencies) {
  self = Base()
  self$dependencies = dependencies
  components = names(self$dependencies$component_classes)
  for (component in components) ComponentRefresher(self, component)
  return_object(self, "Refresher")
}

ComponentRefresher = function(refresher, component) {
  self = Base()
  self$refresher = refresher
  self$component = component
  self$refresher[[self$component]] = function(x) {
    custom_refresher_name = sprintf("refresh_%s", self$component)
    deps = self$refresher$dependencies$deps()[[self$component]]
    rev_deps = self$refresher$dependencies$rev_deps()[[self$component]]
    container = self$refresher$dependencies$container

    ## update of the component
    container[[self$component]] = x

    ## place the updated component inside all other components
    ## that have a 'simple' dependency on the focal component
    for (d in rev_deps$simple) container[[d]][[self$component]] = x

    ## place the updated component inside all other components
    ## have a dependency on the focal component, such that this
    ## dependency is induced by a custom refresh method that is
    ## called `refresh_{focal_component}`
    for (d in rev_deps$custom) container[[d]][[custom_refresher_name]](x)

    ## update all components that the focal component depends on
    ## (sounds expensive but we are only passing by reference)
    for (d in deps) container$refresh[[d]](container[[d]])
  }
  return_object(self, "ComponentRefresher")
}
