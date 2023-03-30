#' Collection
#'
#' Named collection of components.
#'
#' Much like a \code{\link{list}} but designed to work with \code{\link{Files}}.
#' Typically one produces a `Collection` object by freezing a
#' \code{\link{Files}} object so that changes to the files that are
#' represented by the object will not affect the object.
#'
#' @param ... Named components.
#'
#' @return Object of class \code{Collection} with the following methods.
#'
#' ## Methods
#'
#' * `$get(component_name)`: Return components by name.
#' * `$freeze()`: Return the object itself so that the `$freeze()` method
#' in \code{\link{Files}} objects feel idempotent.
#'
#' @export
Collection = function(...) {
  #browser()
  self = Base()
  self$.components = nlist(...)
  self$get = function(component_name) self$.components[[component_name]]
  self$freeze = function() self
  return_object(self, "Collection")
}

#' Files
#'
#' Construct objects for representing files on disk.
#'
#' Files can be accessed by reading them into R objects in memory on demand,
#' and caching the objects. The cache is used to return file contents unless
#' the file is updated, in which case the file is accessed again and
#' corresponding object re-cached.
#'
#' @param directory Path to a directory containing the files.
#' @param ... One or more \code{\link{reader_spec}}(s) that point to files
#' within the \code{directory} and define what \code{\link{Reader}} should
#' be used to access the file.
#'
#' @return Object of class \code{Collection} with the following methods.
#'
#' ## Methods
#'
#' * `$get(component_name)`: Return components by name.
#' * `$freeze()`: Convert to a \code{\link{Collection}} object, which is
#' equivalent to a \code{Files} object in that the \code{$get} method returns
#' the same objects. The difference is that a \code{Files} object
#' will update what it returns if the associated files change on disk, whereas
#' \code{Collection} is intended to be (by convention) immutable (hence the
#' verb 'freeze'). The 'by convention' bit means that a user is free to change
#' the contents of the private `.components` field, but this would violate
#' the convention.
#'
#' @export
Files = function(directory, ...) {
  #browser()
  self = Base()
  self$.directory = normalizePath(directory)
  self$.readers = list(...)
  for (i in seq_along(self$.readers)) {
    self$.readers[[i]] = self$.readers[[i]](self$.directory)
  }
  self$.readers = do.call(c, self$.readers)
  self$.component_names = names(self$.readers)
  self$.access_times = setNames(
    rep(list(Sys.time()), length(self$.readers)),
    self$.component_names
  )
  self$.components = setNames(
    vector("list", length(self$.readers)),
    self$.component_names
  )
  # read data and store it, bumping the access time
  self$.fetch = function(component_name) {
    self$.access_times[[component_name]] = Sys.time()
    self$.components[[component_name]] =
      self$.readers[[component_name]]$read()
  }
  # read data, store it, return it, bumping the access time
  self$.read = function(component_name) {
    self$.fetch(component_name)
    self$.components[[component_name]]
  }
  # fill the components fields
  self$.components = setNames(
    lapply(names(self$.readers), self$.read),
    self$.component_names
  )
  # fetch data only if it was last accessed before it changed
  self$.pull = function(component_name) {
    access_time = self$.access_times[[component_name]]
    modification_time = file.mtime(self$.readers[[component_name]]$file)
    if (modification_time > access_time) self$.fetch(component_name)
  }
  # pull data (i.e. fetch it only if necessary) and return it
  self$get = function(component_name) {
    self$.pull(component_name)
    self$.components[[component_name]]
  }
  self$freeze = function() do.call(Collection, self$.components)
  return_object(self, "Files")
}

#' Reader Spec
#'
#' Bundle a file path with a \code{\link{Reader}}.
#'
#' Typically this function is used when defining an object that inherits
#' from \code{\link{Files}}.
#'
#' @param file Path to a single file.
#' @param reader One of the functions documented in \code{\link{Reader}}s.
#'
#' @return A function that takes a path in which to file \code{file} and
#' returns a named list with one element giving an instantiated
#' \code{\link{Reader}} object with name given by the file name with the
#' extension removed. The path may optionally be broken into path components,
#' which are assembled using \code{\link{file.path}}.
#'
#' @export
reader_spec = function(file, reader) {
  component_name = tools::file_path_sans_ext(file)
  function(...) setNames(list(reader(..., file)), component_name)
}


if (FALSE) {

f = Files("inst/starter_models/seir"
  , reader_spec("derivations.json", JSONReader)
  , reader_spec("flows.csv", CSVReader)
  , reader_spec("settings.json", JSONReader)
  , reader_spec("variables.csv", CSVReader)
)
f$get("variables")
f$.fetch("derivations")
names(f$.readers)
reader_spec("derivations.json", JSONReader)("inst", "starter_models", "seir")$derivations$read()

a = function(x) function(a) list(a = x)
b = function(x) function(b) list(b = x)
ff = function(...) {
  lapply()
}
ff(a(10), b(20))
}
