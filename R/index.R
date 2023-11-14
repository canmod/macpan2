#' Model Component Index
#'
#' Make an index object to represent descriptions of model components and their
#' dimensions of variation. These objects generalize and wrap
#' \code{\link{data.frame}}s. Each row of this \code{\link{data.frame}}-like
#' object is an entry in the index, and each column provides a description of
#' the entries. Each of these entries describes an element of a model component.
#' For example, the following index describes the
#' state variables of an age-structured SIR model. Each row corresponds to a
#' state variable and each state variable is described by columns `Epi` and
#' `Age`.
#' ```{r, echo = FALSE}
#' sir = mp_index(Epi = c("S", "I", "R"))
#' age = mp_index(Age = c("young", "old"))
#' prod = mp_cartesian(sir, age)
#' prod
#' ```
#' Each index can produce labels for the elements by dot-concatenating the
#' values in each row. The labels of the state variables in this
#' age-structured SIR example model are as follows.
#' ```{r, echo = FALSE}
#' labels(prod)
#' ```
#' These labels can be used to create 'multidimensional' names for the elements
#' of vectors. Here is the above example expressed in vector form.
#' ```{r, echo = FALSE}
#' v = Vector(prod)
#' v$set_numbers(Epi = c(S = 1000))$set_numbers(Epi = c(I = 1), Age = "old")
#' ```
#' This example vector could be stored as a 3-by-2 matrix. But other examples
#' cannot, making this indexing approach more general. For example, consider
#' the following index.
#' ```{r, echo = FALSE}
#' symp = mp_index(
#'  Epi = c("S", "I", "I", "R"),
#'  Symptoms = c("", "mild", "severe", "")
#' )
#' symp
#' ```
#' This index has an associated indexed vector that cannot be expressed
#' as a matrix.
#' ```{r, echo = FALSE}
#' mp_vector(symp)$set_numbers(Epi = c(S = 1000))$set_numbers(Epi = c(I = 1), Symptoms = "severe")
#' ```
#' Dots are not allowed in the index so that the labels can be inverted to
#' reproduce the original index (provided that the column names can be
#' retrieved). See the details below for more restrictions.
#'
#' This function is analogous to the
#' \code{\link{data.frame}} function for creating data frames.
#'
#' All values in the index must contain only letters, numbers, and underscores,
#' and blank empty string entries are also allowed. No value can be missing.
#' These restrictions ensure that the dot-concatenation of each row can be
#' unambiguously inverted and that these dot-concatenations produce
#' syntactically valid names (see \code{\link{make.names}}). These
#' dot-concatenations are used to provide labels for the model components.
#'
#' By convention, it is recommended to use CamalCase for the columns of
#' indexes and either snake_case (aging_rate) or single uppercase
#' letters (e.g. S, I). This helps when reading code that contains
#' references to both column names and values in an index.
#'
#' @param ... Character vectors to combine to produce an index. Alternatively,
#' any number of data frames of character-valued columns. If data frames are
#' supplied, their rows will be binded and the result converted to an index
#' if possible.
#' @param labelling_column_names A \code{\link{character}} vector of the names
#' of the index that will be used to label the model components (i.e. rows)
#' being described. The \code{labelling_column_names} cannot have duplicates
#' and must contain at least one name. The index given by the
#' \code{labelling_column_names} must uniquely identify each row.
#'
#' @examples
#' state = mp_index(
#'   Epi = c("S", "I", "S", "I"),
#'   Age = c("young", "young", "old", "old")
#' )
#' print(state)
#' labels(state)
#' mp_cartesian(state, mp_index(City = c("hamilton", "toronto")))
#' @family indexes
#' @export
mp_index = function(..., labelling_column_names) UseMethod("mp_index")


#' Index
#'
#' @param partition A data frame (or data frame-like) object containing
#'   definitions of the index. This object can be a \code{\link{data.frame}},
#'   \code{\link{Partition}}, or another \code{\link{Index}} object.
#' @param labelling_column_names A \code{\link{character}} vector of the names of the
#'   index that will be used to label the model components (i.e. rows) being
#'   described. The \code{labelling_column_names} cannot have duplicates and must
#'   contain at least one name. The index given by the \code{labelling_column_names}
#'   must uniquely identify each row.
#' @param reference_index (Advanced) An optional partition to use when
#'        computing subset indices.
#' @param x \code{Index} object.
#' @param ... For consistency with existing S3 methods.
#'
#' @seealso [mp_index()]
#' @noRd
#' @keywords internal
#' @export
Index = function(partition, labelling_column_names = names(partition), reference_index = NULL) {
  UseMethod("Index")
}

#' @export
Index.Partition = function(partition
    , labelling_column_names = names(partition)
    , reference_index = NULL
  ) {
  self = Base()

  ## Args
  self$partition = partition
  self$labelling_column_names = to_names(labelling_column_names)

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
  self$reset_reference_index = function() self$.reference_index = NULL

  ## Standard Methods
  self$labels = function() self$partition$select(self$labelling_column_names)$labels()
  self$partial_labels = function(...) self$partition$partial_labels(...)
  self$reference_labels = function() {
    self$reference_index()$partial_labels(self$labelling_column_names)
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

#' @export
Index.data.frame = function(partition, labelling_column_names = names(partition), reference_index = NULL) {
  partition |> Partition() |> Index(labelling_column_names, reference_index)
}

#' @export
Index.Index = function(partition, labelling_column_names = names(partition), reference_index = NULL) {
  partition$partition |> Index(labelling_column_names, reference_index)
}

#' @describeIn mp_index Print an index.
#' @export
print.Index = function(x, ...) print(x$partition)

#' @describeIn mp_index Get the names of the columns of an index.
#' @export
names.Index = function(x) x$partition$names()

#' @export
as.data.frame.Index = function(x, row.names = NULL, optional = FALSE, ...) {
  x$partition$frame()
}

#' @export
labelling_column_names = function(x) UseMethod("labelling_column_names")

#' @describeIn mp_index Retrieve the \code{labelling_column_names} of
#' an index. These are the names of the columns that are used to label
#' the model components.
#' @export
labelling_column_names.Index = function(x) x$labelling_column_names

#' @describeIn mp_index Convert an index into
#' a character vector giving labels associated with each model component
#' (i.e. row) being described.
#' @export
to_labels.Index = function(x) x$labels()

#' @describeIn mp_index Convert an index into
#' a character vector giving labels associated with each model component
#' (i.e. row) being described.
#' @export
labels.Index = function(x, ...) x$labels()


#' @export
mp_index.character = function(..., labelling_column_names) {
  f = data.frame(...)
  if (missing(labelling_column_names)) labelling_column_names = names(f)
  Index(f, to_names(labelling_column_names))
}

#' @export
mp_index.data.frame = function(..., labelling_column_names) {
  f = list(...) |> bind_rows()
  if (missing(labelling_column_names)) labelling_column_names = names(f)
  Index(f, to_names(labelling_column_names))
}
