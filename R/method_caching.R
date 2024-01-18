## Internal classes for caching the return value of a no-op method.

CachedMethod = function(object, method, method_name) {
  self = Base()
  self$object = object
  self$method = method
  self$method_name = method_name

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

  ## replace function definition
  self$object[[self$method_name]] = function() {
    mc = match.call()
    mc[[1L]] = self$get
    eval(mc)
  }

  ## set the argument list of the method to be identical
  ## to that of the get method in the cache
  formals(self$object[[self$method_name]]) = formals(self$get)

  return_object(self, "CachedMethod")
}

initialize_cache = function(object, ...) {
  self = Base()
  self$object = object

  ## list of methods that will get a cache
  self$method_names = unlist(lapply(list(...), as.character), recursive = TRUE)

  ## create a cache for each method
  for (nm in self$method_names) self[[nm]] = CachedMethod(object, object[[nm]], nm)

  self$invalidate = function() {
    for (nm in self$method_names) self[[nm]]$invalidate()
  }

  ## initialize the cache
  self$object$cache = return_object(self, "MethodsCache")
}

CacheList = function(...) {
  self = Base()
  self$cache_objects = list(...)
  self$invalidate = function() {
    for (i in seq_along(self$cache_objects)) {
      self$cache_objects[[i]]$invalidate()
    }
  }
  return_object(self, "CacheList")
}
