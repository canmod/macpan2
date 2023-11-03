#' Index
#'
#' Make an object to represent descriptions of model components and their
#' dimensions of variation. \code{Index} objects generalize and wrap
#' \code{\link{data.frame}}s. Each row of this \code{\link{data.frame}}-like
#' object is an entry in the index, and each column provides a description of
#' the entries. For example, the following \code{Index} object describes the
#' state variables of an age-structured SIR model. Each row corresponds to a
#' state variable and each state variable is described by columns `Epi` and
#' `Age`.
#' ```{r, echo = FALSE}
#' sir = mp_index(Epi = c("S", "I", "R"))
#' age = mp_index(Age = c("young", "old"))
#' mp_cartesian(sir, age)
#' ```
#'
#' @param partition A data frame (or data frame-like) object containing
#'   definitions of the index. This object can be a \code{\link{data.frame}},
#'   \code{\link{Partition}}, or another \code{\link{Index}} object.
#' @param labelling_names A \code{\link{character}} vector of the names of the
#'   index that will be used to label the model components (i.e. rows) being
#'   described. The \code{labelling_names} cannot have duplicates and must
#'   contain at least one name. The index given by the \code{labelling_names}
#'   must uniquely identify each row.
#' @param reference_index (Advanced) An optional partition to use when
#'        computing subset indices.
#' @param x \code{Index} object.
#' @param ... For consistency with existing S3 methods.
#'
#' @seealso [mp_index()]
#'
#' @examples
#' sir = data.frame(
#'   Epi   = c("S",     "I",     "R",     "D"   ),
#'   Vital = c("alive", "alive", "alive", "dead")
#' ) |> Index(labelling_names = "Epi")

#' age = data.frame(
#'   Age = c("young", "old")
#' ) |> Index()
#' state_index = mp_cartesian(sir, age)
#' labels(state_index)
#' (state_index
#'   |> mp_subset(Vital = "alive")
#'   |> labels()
#' )
#'
#' @export
Index = function(partition, labelling_names = names(partition), reference_index = NULL) {
  UseMethod("Index")
}

#' @describeIn Index Create a \code{Index} object from a
#' \code{\link{Partition}} object. These \code{\link{Partition}} objects
#' wrap \code{\link{data.frame}}s. These data frames must follow certain
#' restrictions.
#' @export
Index.Partition = function(partition
    , labelling_names = names(partition)
    , reference_index = NULL
  ) {
  self = Base()

  ## Args
  self$partition = partition
  self$labelling_names = to_names(labelling_names)

  ## Private Arg
  self$.reference_index = reference_index

  ## Getters and Setters
  self$reference_index = function() {
    if (is.null(self$.reference_index)) return(self)
    self$.reference_index
  }
  self$set_reference_index = function(index) {
    self$.reference_index = index
  }

  ## Standard Methods
  self$labels = function() self$partition$select(self$labelling_names)$labels()
  self$partial_labels = function(...) self$partition$partial_labels(...)
  self$reference_labels = function() {
    self$reference_index()$partial_labels(self$labelling_names)
  }
  self$reference_positions = function(zero_based = FALSE) {
    i = match(self$reference_labels(), self$labels())
    if (zero_based) i = i - 1L
    i
  }
  self$positions = function(zero_based = FALSE) {
    i = match(self$labels(), self$reference_labels())
    if (zero_based) i = i - 1L
    i
  }

  return_object(self, "Index")
}

#' @describeIn Index Create a \code{Index} object from a
#' \code{\link{data.frame}} object that follows certain restrictions
#' documented in the help page for \code{\link{Partition}}s.
#' @export
Index.data.frame = function(partition, labelling_names = names(partition), reference_index = NULL) {
  partition |> Partition() |> Index(labelling_names, reference_index)
}

#' @describeIn Index Create another \code{Index} object from an
#' existing \code{Index} object, possibly with new \code{labelling_names}.
#' @export
Index.Index = function(partition, labelling_names = names(partition), reference_index = NULL) {
  partition$partition |> Index(labelling_names, reference_index)
}

#' @describeIn Index Print a \code{Index} object.
#' @export
print.Index = function(x, ...) print(x$partition)

#' @describeIn Index Get the names of the index
#' (i.e. columns) of a \code{Index} object.
#' @export
names.Index = function(x) x$partition$names()

#' @export
as.data.frame.Index = function(x, row.names = NULL, optional = FALSE, ...) {
  x$partition$frame()
}

#' @export
labelling_names = function(x) UseMethod("labelling_names")

#' @describeIn Index Retrieve the \code{labelling_names} of
#' a \code{Index} object. These \code{labelling_names}
#' are the names of the index that are used to label the model
#' components.
#' @export
labelling_names.Index = function(x) x$labelling_names

#' @describeIn Index Convert a \code{Index} object into
#' a character vector giving labels associated with each model component
#' (i.e. row) being described.
#' @export
to_labels.Index = function(x) x$labels()

#' @describeIn Index Convert a \code{Index} object into
#' a character vector giving labels associated with each model component
#' (i.e. row) being described.
#' @export
labels.Index = function(x, ...) x$labels()


#' Create Index Object
#'
#' Create an \code{\link{Index}} object from a set of
#' character vectors. This function is analogous to the
#' \code{\link{data.frame}} function for creating data frames.
#'
#' @param ... Character vectors to combine into an \code{\link{Index}}.
#' object.
#' @param labelling_names See \code{\link{Index}}.
#'
#' @seealso [Index()]
#' @examples
#' mp_index(
#'   Epi = c("S", "I", "S", "I"),
#'   Age = c("young", "young", "old", "old")
#' )
#'
#' @export
mp_index = function(..., labelling_names) UseMethod("mp_index")

#' @export
mp_index.character = function(..., labelling_names) {
  f = data.frame(...)
  if (missing(labelling_names)) labelling_names = names(f)
  Index(f, to_names(labelling_names))
}

#' @export
mp_index.data.frame = function(..., labelling_names) {
  f = list(...)[[1L]]
  if (missing(labelling_names)) labelling_names = names(f)
  Index(f, to_names(labelling_names))
}
