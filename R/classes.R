# Initialization Utilities ---------------------------

# clean the environment of a method (or methods) so that
# they contain a single object -- self -- which is the
# environment defining an object
clean_method_environment = function(e) {
  # remove everything from the initializing
  # environment except self. this is a convenience
  # so that you don't need to worry about cleaning
  # up intermediate results from the class definition
  if (!is.environment(e)) stop("e must be an environment")
  if (identical(e, globalenv())) stop("e cannot be the global environment")
  if (environmentIsLocked(e)) stop("e is locked")
  if (!"self" %in% ls(e)) stop("e must contain self")
  if (!is.environment(e$self)) stop("self must be an environment")
  if (identical(e$self, globalenv())) stop("self cannot be the global environment")
  if (environmentIsLocked(e$self)) stop("self is locked")
  all_objects = ls(envir = e)  # clean objects starting with a dot as well?
  objects_to_keep = "self"
  objects_to_clean = setdiff(all_objects, objects_to_keep)
  rm(list = objects_to_clean, envir = e)
}

# S3 generic for checking the validity of a constructed
# object. should either return nothing or trigger an error.
# typically these S3 methods will make use of object$valid
# methods
validate_object = function(object) {
  UseMethod("validate_object")
}

# this should be the final function called in a class
# definition. think of it like return(...)
return_object = function(self, class) {
  clean_method_environment(parent.frame())
  object = structure(self, class = c(class, unique(class(self))))
  validate_object(object)
  return(object)
}

inherit_from = function(parent, traits) {
  self = parent()
  for (trait in traits) {
    trait_object = trait()
    trait()$forward_all(self)
  }
  return(self)
}

implements = function(interface) {
  object = Implementation()
  object$.implements(interface)
  return(object)
}

# Basal Classes ---------------
# Base: Testable: Implementation
#                 Trait
#                 Interface

# Inherit from Base if you want to start
# from an empty class
Base = function() {
  self = new.env(parent = emptyenv())
  structure(self, class = "Base")
}

# Inherit from Testable if you would like
# your class to provide a validity check.
# Validity checking often requires differentiating
# between public versus private members, as well
# as fields versus methods.
Testable = function() {
  self = Base()
  self$valid = function() TRUE
  self$.field_names = function() {
    not_function = function(x) !is.function(x)
    names(which(unlist(eapply(self, not_function))))
  }
  self$.method_names = function() {
    names(which(unlist(eapply(self, is.function))))
  }
  self$.public = function(names) names[!startsWith(names, ".")]
  self$.private = function(names) names[startsWith(names, ".")]
  self$.public_method_names = function() {
    self$.public(self$.method_names())
  }
  self$.private_method_names = function() {
    self$.private(self$.method_names())
  }
  self$.public_field_names = function() {
    self$.public(self$.method_names())
  }
  self$.private_field_names = function() {
    self$.private(self$.method_names())
  }
  return_object(self, "Testable")
}

# Inherit from Trait if you want to use
# your class to forward public methods to
# other classes without direct inheritance
Trait = function() {
  self = Testable()
  self$.forwardable_method_names = function() {
    setdiff(self$.public_method_names(), c("forward_all", "forward_method"))
  }
  self$forward_method = function(focal_object, method) {
    # forward the method from the trait (i.e. self) to focal_object
    focal_object[[method]] = self[[method]]
    # make sure that the environment of the forwarded method
    # contains a single object called self, which is the
    # focal_object itself
    environment(focal_object[[method]]) = list2env(list(self = focal_object))
  }
  self$forward_all = function(focal_object) {
    methods = self$.forwardable_method_names()
    for (method in methods) {
      self$forward_method(focal_object, method)
    }
  }
  self$valid = function() {
    if (length(self$.field_names()) != 0L)
    return("traits cannot contain data, only methods")
    TRUE
  }
  return_object(self, "Trait")
}

# Inherit from Interface if you want to
# define an abstract class that defines the
# argument signatures and return value types
# of methods.
Interface = function() {
  self = Testable()
  self$.stopifnot_valid_interface = function() {
    method_nms = self$.public_method_names()
    for (nm in method_nms) {
      ## assumption: arguments in the pairlist are of type `name` if they
      ## have no default -- this seems fragile but i can't seem to find the
      ## canonical/explicit approach here
      if (any(vapply(formals(self[[nm]]), is.name, logical(1L)))) {
        stop("all public methods in interfaces must have defaults")
      }
    }
  }
  return_object(self, "Interface")
}

# Inherit from Implementation (using the
# implements utility function) if you want
# your class to implement an interface
Implementation = function() {
  self = Testable()
  self$interface = Interface()
  self$.implements = function(interface_class) {
    self$interface = interface_class()
  }
  self$.stopifnot_valid_implementation = function() {
    interface = self$interface
    method_nms = interface$.public_method_names()
    for (nm in method_nms) {
      if (!identical(formals(self[[nm]]), formals(interface[[nm]]))) {
        stop("method arguments do not have identical signatures")
      }
      self_return_type = typeof(try(self[[nm]](), silent = TRUE))
      interface_return_type = typeof(try(interface[[nm]](), silent = TRUE))
      if (!identical(self_return_type, interface_return_type)) {
        stop("default return value must be of the same type")
      }
    }
  }
  return_object(self, "Implementation")
}

# S3 Methods -----------------------
# these are like python-style __dunder__ methods
# such as __str__, __eq__

print.Base = function(x, ...) {
  str(x)
  print(lsf.str(envir = x))
  invisible(x)
}

`$.Base` = function(x, name) {
  ## make sure that you get an error if
  ## a name cannot be found in self
  get(name, envir = x)
}

## Base objects are not testable ...
validate_object.Base = function(object) {}
## ... but Testable objects obviously are
validate_object.Testable = function(object) {
  # S4-style validity checking
  v = try(object$valid())
  if (!isTRUE(v)) {
    msg = try(as.character(v))
    if (inherits(msg, "try-error")) {
      stop("object construction has failed, and a reason is unavailable")
    }
    stop(v)
  }
}
## Implementation and Interface objects get some special
## validity checking before the standard stuff that gets
## checked using the $valid method of the object.
validate_object.Implementation = function(object) {
  object$.stopifnot_valid_implementation()
  validate_object.Testable(object)  ## assess object using $valid method
}
validate_object.Interface = function(object) {
  object$.stopifnot_valid_interface()
  validate_object.Testable(object)  ## assess object using $valid method
}
