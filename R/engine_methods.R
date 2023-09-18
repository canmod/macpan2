#' Engine Methods
#'
#' List of methods for pre-processing matrices before returning or
#' assigning them. One benefit of these methods is that they can make use
#' of user-supplied integer vectors that are stored in C++ as integers. Another
#' benefit can be readability but this is subjective.
#'
#' @param exprs A named `list` of formulas that can be matched to one of the
#' method prototypes (TODO: describe these).
#' @param int_vecs An \code{\link{IntVecs}} object containing integer vectors
#' that can be used by the methods.
#'
#' @export
EngineMethods = function(exprs = list(), int_vecs = IntVecs()) {
  self = Base()

  # Args
  self$exprs = exprs
  self$int_vecs = int_vecs

  # Composition
  self$method_types = MethodTypes()
  self$meth_list = MethListFromEngineMethods(self)

  # Refreshments
  self$refresh_init_mats = function(init_mats) {
    for (meth_nm in names(self$meth_list)) {
      self$meth_list$methods[[meth_nm]]$init_mats = init_mats
    }
  }
  return_object(self, "EngineMethods")
}

#' Construct a Method List from Engine Methods
#'
#' Alternative constuctor for `MethList`.
#'
#' The engine interacts with `MethList` objects, but the user interacts
#' with `EngineMethods` objects. This allows developers to convert between
#' the two.
#'
#' @param engine_methods An object of class EngineMethods
#' @nord
MethListFromEngineMethods = function(engine_methods) {
  l = list()
  for (nm in names(engine_methods$exprs)) {
    l[[nm]] = engine_methods$method_types$make_method(engine_methods$exprs[[nm]], nm)
    l[[nm]]$int_vecs = engine_methods$int_vecs
  }
  do.call(MethList, l)
}


#' Method List
#'
#' @param ... List of \code{\link{Method}} objects.
#' @export
MethList = function(...) {
  self = Base()

  # Args
  self$methods = list(...)

  # Static
  self$.null_data_arg = setNames(
    rep(list(integer()), 5),
    sprintf("meth_%s",
      c("type_id", "n_mats", "n_int_vecs", "mat_id", "int_vec_id")
    )
  )

  self$data_arg = function() {
    l = self$.null_data_arg
    new_args = method_apply(self$methods, "data_arg")
    for (a in names(l)) {
      l[[a]] = c(l[[a]], unlist(
        lapply(new_args, `[[`, a),
        recursive = FALSE
        , use.names = FALSE
      ))
    }
    l
  }

  return_object(self, "MethList")
}

#' @export
names.MethList = function(x) {
  vapply(x$methods, getElement, character(1L), "name")
}

#' Engine Method Class
#'
#' Engine methods allow users to interact with the engine
#' by specifying a unique \code{name}. This name is used to reference recipes
#' for computing matrices from other matrices and constants.
#'
#' @param name Method name.
#' @param mat_args Character vector of names of matrices that will be used by
#' the method to produce an output matrix.
#' @param int_vec_args Character vector of names of constants that will be used
#' by the method to produce an output matrix.
#' @export
Method = function(name, mat_args, int_vec_args, init_mats = MatsList(), int_vecs = IntVecs()) {
  self = Base()

  # Args
  self$name = name
  self$mat_args = mat_args
  self$int_vec_args = int_vec_args
  self$init_mats = init_mats
  self$int_vecs = int_vecs

  # Static
  ## abstract -- instantiate with implementation classes (e.g. MethodRowIndexer)
  self$meth_type_id = NA_integer_

  # Private
  self$.mat_ids = function() match(self$mat_args, names(self$init_mats)) - 1L
  self$.int_vec_ids = function() match(self$int_vec_args, names(self$int_vecs)) - 1L
  self$.check_args = function() {
    missing_mats = self$mat_args[!self$mat_args %in% names(self$init_mats)]
    missing_int_vecs = self$int_vec_args[!self$int_vec_args %in% names(self$int_vecs)]
    if (length(missing_mats) != 0L) {
      mixup = missing_mats[missing_mats %in% names(self$int_vecs)]
      if (length(mixup) != 0L) {
        stop(
            "\nThe ", self$name, " method had the following integer vectors passed"
          , "\nto arguments that require matrices:\n"
          , paste(mixup, collapse = "\n")
        )
      }
      stop(
          "\nThe ", self$name, " method could not find the following matrices:\n"
        , paste(missing_mats, collapse = "\n")
      )
    }
    if (length(missing_int_vecs) != 0L) {
      mixup = missing_int_vecs[missing_int_vecs %in% names(self$init_mats)]
      if (length(mixup) != 0L) {
        stop(
            "\nThe ", self$name, " method had the following matrices passed"
          , "\nto arguments that require integer vectors:\n"
          , paste(mixup, collapse = "\n")
        )
      }
      stop(
          "\nThe ", self$name, " method coult not find the following integer vectors:\n"
        , paste(missing_int_vecs, collapse = "\n")
      )
    }
  }

  # Standard Methods
  self$data_arg = function() {
    self$.check_args()
    list(
      ## these must be length-1 integer vectors
        meth_type_id = self$meth_type_id
      , meth_n_mats = length(self$mat_args)
      , meth_n_int_vecs = length(self$int_vec_args)
      ## these must be length-meth_n_mats and length-meth_n_const respectively
      , meth_mat_id = self$.mat_ids()
      , meth_int_vec_id = self$.int_vec_ids()
    )
  }

  return_object(self, "Method")
}
