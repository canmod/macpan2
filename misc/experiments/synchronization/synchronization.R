library(macpan2)
library(oor)

CachedMethod = function(object, method) {
  self = Base()
  self$object = object
  self$method = method

  self$invalidate = function() self$.valid = FALSE
  self$get = function() {
    if (!isTRUE(self$.valid)) {
      mc = match.call()
      mc[[1L]] = self$method
      self$.cache = eval(mc)
      self$.valid = TRUE
    }
    return(self$.cache)
  }

  self$invalidate()
  formals(self$get) = formals(method)

  return_object(self, "CachedMethod")
}

initialize_cache = function(object, ...) {
  self = Base()
  self$object = object

  ## list of methods that will get a cache
  self$method_names = unlist(lapply(list(...), as.character), recursive = TRUE)

  ## create a cache for each method
  for (nm in self$method_names) self[[nm]] = CachedMethod(object, object[[nm]])

  ## initialize the cache
  self$object$cache = return_object(self, "MethodsCache")

  ## replace the methods in the object with ones that look in the cache instead
  for (nm in self$method_names) {

    ## replace function definition
    self$object[[nm]] = function(nm) {
      force(nm)
      function() {
        mc = match.call()
        mc[[1L]] = self[[nm]]$get
        eval(mc)
      }
    }()

    ## set the argument list of the method to be identical
    ## to that of the get method in the cache
    formals(self$object[[nm]]) = formals(self[[nm]]$get)
  }
}

A = function(x) {
  self = Base()
  self$x = x
  self$y = function() self$x + 1
  initialize_cache(self, "y")
  return_object(self, "A")
}

a = A(1)
a$x = 200
a$cache$y$invalidate()
a$cache$y$get()
a$y()


A = function(x) {
  self = Base()
  self$x = x
  self$y = CachedMethod(self, function() {
    self$x + 1
  })
  self$update_x = function(x) {
    self$x = x
    self$y$invalidate()
  }
  return_object(self, "A")
}

a = A(2)
a$y$get()

a$x = 20
a$y$get()

a$update_x(20)
a$y$get()




library(macpan2)
library(oor)

MethodsCache = function(...) {
  self = Base()
  return_object(self, "MethodsCache")
}

cache_methods = function(self, ...) {
  self$methods_cache = MethodsCache(...)
  method_names = unlist(lapply(list(...), as.character), recursive = TRUE)
  self$.valid = setNames(logical(length(method_names)), method_names)
  for (m in method_names) {

  }

  self$invalidate = function() self$.valid = FALSE
  self$get = function() {
    if (!isTRUE(self$.valid)) {
      mc = match.call()
      mc[[1L]] = self$method
      self$.cache = eval(mc)
      self$.valid = TRUE
    }
    return(self$.cache)
  }

  self$invalidate()
  formals(self$get) = formals(method)

  return_object(self, "CachedMethod")
}



A = function(x) {
  self = Base()
  self$x = x
  self$y = CachedMethod(self, function() {
    self$x + 1
  })
  self$update_x = function(x) {
    self$x = x
    self$y$invalidate()
  }
  return_object(self, "A")
}

a = A(2)
a$y$get()

a$x = 20
a$y$get()

a$update_x(20)
a$y$get()




cc = FALSE

xx = 1
yy = 2

ff = function() {
  zz = xx + 1
  zz * 2
}

append(body(ff), expression(xx = 52), 2)
## modify the body of the function by placing cache checking lines at the beginning
