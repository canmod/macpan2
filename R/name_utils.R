#' Names and Labels
#' 
#' This page describes functions for giving names and labels to
#' entities in structured models.
#' 
#' # Context
#' 
#' A goal of `macpan2` is to provide a mechanism for representing structured 
#' compartmental models. An example of such a model is to have each compartment
#' in an SEIR model split into a set of spatial locations and into a set of 
#' age groups. It is crucial but difficult to assign meaningful and consistent 
#' names to the compartments, flow rates, transmission rates, contact rates, 
#' sub-population sizes, and other parameters determining these quantities. 
#' Such names should convey how the different quantities relate to one another.
#' For example, the names should make clear that the rate of flow between two 
#' compartments is specific to, say, the age group and location of those 
#' compartments. The naming system should facilitate identifying model 
#' quantities and sets of quantities. For example, in a spatially structured
#' model we might want to refer to all states in a particular location 
#' (e.g. Toronto) and a specific state within that location (e.g. susceptible 
#' individuals in Toronto).
#' 
#' Model entities (e.g. states, flow rates, transmission rates), can be 
#' described using a data frame of string-valued columns. The rows of these data
#' frames represent the entities being represented.  The columns of the data 
#' frame represent different ways to describe the rows.
#' 
#' ```{r}
#' EpiSympVax = data.frame(
#'   Epi = c(rep(c("S", "E", "I", "I", "R", "beta"), 2), "alpha", "gamma", "gamma", "infectiousness", "infectiousness", ""),
#'   Symp = c(rep(c("", "", "mild", "severe", "", ""), 2), "", "mild", "severe", "mild", "severe", ""),
#'   Vax = c(rep(c("unvax", "vax"), each = 6), "", "", "", "", "", "dose_rate")
#' )
#' EpiSympVax
#' ```
#' 
#' Non-empty values in each cell must contain only letters, numbers, underscores,
#' and must start with a letter. Empty values are zero-length strings that can be
#' used to indicate that some partitions are not applicable to some variables. The
#' purpose for these restrictions is to facilitate the construction of strings and
#' character vectors that summarize different aspects of the data frame.
#' When taken together, these summaries can be inverted to restore the full
#' labelled partition and so they represent zero information loss. This equivalence
#' allows us to go back-and-forth between the two representations without loosing
#' information, but perhaps gaining convenience.
#' 
#' There are three types of summaries: the names, the name, and the labels. The
#' names of a data frame are the names of the string-valued columns.
#' 
#' ```{r}
#' to_names(EpiSympVax)
#' ```
#' 
#' The name of a data frame is the dot-concatenation of the names.
#' 
#' ```{r}
#' to_name(EpiSympVax)
#' ```
#' 
#' The labels of a data frame is the row-wise dot-concatenation of the 
#' string-valued columns.
#' 
#' ```{r}
#' to_labels(EpiSympVax)
#' ```
#' 
#' These labels give a unique single character string for referring to each
#' variable. With only the labels and one of either the names or the name, one may
#' recover the labelled partition. The labels provide convenient names for the
#' variables -- i.e. rownames.
#' By convention we use [UpperCamelCase](https://en.wikipedia.org/wiki/Camel_case)
#' for partition names and a modified form of
#' [snake_case](https://en.wikipedia.org/wiki/Snake_case) for variable labels. Our
#' modification of snake case allows for single uppercase letters in order to
#' accommodate the convention in epidemiology for using single uppercase letters to
#' refer to state variables. For example, `S`, `I`, and `R`, as well as `I_mild`
#' and `I_severe`, would be consistent with our modified snake case style.
#' 
#' @name names_and_labels
NULL


#' @param x Object from which to extract its name, names, labels, name-pairs,
#' or values. Not all types of objects will work for all functions.
#' @return Character vector (or numeric vector in the case of `to_values`)
#' that describes `x`.
#'
#' @describeIn names_and_labels Extract a vector for describing the rows
#' of a data frame or values of a numeric vector.
#' @export
to_labels = function(x) UseMethod("to_labels")

#' @export
to_labels.character = function(x) valid_dotted$assert(x)

#' @export
to_labels.Partition = function(x) x$labels()

#' @export
to_labels.numeric = function(x) {
  nms = names(x)
  if (!is.null(nms)) return(nms)
  dn = dimnames(x)
  if (!is.null(dn)) {
    if (!any(vapply(dn, is.null, logical(1L)))) {
      nms = try(to_labels(do.call(expand.grid, dn)))
      if (!inherits(nms, "try-error")) return(nms)
    }
  }
  nms
}

#' @export
to_labels.data.frame = function(x) {
  i = vapply(x, is.character, logical(1L))
  StringDataFromFrame(x[, i, drop = FALSE])$dot()$labels()$value()
}

#' @export
to_labels.StringData = function(x) x$dot()$labels()$value()

#' @export
to_labels.Scalar = function(x) x$dot()$value()

#' @export
to_labels.Vector = function(x) x$dot()$value()

#' @export
to_labels.Labels = function(x) x$dot()$value()

#' @export
to_labels.Index = function(x) x$labels()

#' @describeIn names_and_labels Extract a character vector for describing the 
#' character-valued columns in a data frame or the flattened structure of
#' a numeric vector.
#' Names obey the following restrictions:  (1) they cannot have dots, (2) all 
#' values must start with a letter, (3) all characters must be letters, 
#' numbers, or underscore.
#' @export
to_names = function(x) UseMethod("to_names")

#' @export
to_names.data.frame = function(x) to_name(x) |> to_names()

#' @export
to_names.NULL = function(x) character(0L)

#' @export
to_names.character = function(x) {
  if (length(x) == 1L) {
    x = StringDottedScalar(x)
  } else if (length(x) > 1L) {
    x = StringUndottedVector(x)
  } else {
    stop("an empty character vector cannot be turned into names")
  }
  to_names(x)
}

#' @export
to_names.Partition = function(x) x$names()

#' @export
to_names.Index = function(x) x$labelling_column_names

#' @export
to_names.StringData = function(x) x$undot()$names()$value()

#' @export
to_names.Scalar = function(x) x$undot()$value()

#' @export
to_names.Names = function(x) x$undot()$value()


#' @describeIn names_and_labels Extract a string (i.e. length-1 character
#' vector) for describing the character-valued columns in a data frame or the 
#' flattened structure of a numeric vector. The name of an object is the 
#' dot-concatenation of its names.
#' @export
to_name = function(x) UseMethod("to_name")

#' @export
to_name.data.frame = function(x) {
  i = vapply(x, is.character, logical(1L))
  to_name(names(x)[i])
}

#' @export
to_name.character = function(x) {
  if (length(x) == 1L) {
    x = StringDottedScalar(x)
  } else if (length(x) > 1L) {
    x = StringUndottedVector(x)
  } else {
    stop("an empty character vector cannot be turned into a name")
  }
  to_name(x)
}

#' @export
to_name.Partition = function(x) x$name()

#' @export
to_name.Index = function(x) to_names(x) |> to_name()

#' @export
to_name.StringData = function(x) x$dot()$names()$value()

#' @export
to_name.Scalar = function(x) x$dot()$value()

#' @export
to_name.Names = function(x) x$dot()$value()


#' @describeIn names_and_labels A character
#' vector with all possible pairwise dot-concatenations of a set of names.
#' @export
to_name_pairs = function(x) UseMethod("to_name_pairs")

#' @export
to_name_pairs.character = function(x) {
  x = to_names(x)
  pairings = triangle_indices(length(x))
  data.frame(
      x = x[pairings$i]
    , y = x[pairings$j]
  ) |> to_labels()
}

#' @export
to_name_pairs.data.frame = function(x) {
  to_names(x) |> to_name_pairs()
}

#' To String
#'
#' Convert an object to a string.
#'
#' @param x Object to convert to a string.
#' @returns A length-1 character vector.
#' @export
to_string = function(x) UseMethod("to_string")

#' @export
to_string.default = function(x) toString(x)

#' @export
to_string.formula = function(x, ...) formula_as_character(x)

#' @export
to_string.ChangeComponent = function(x, ...) x$string()


#' To Positions
#' 
#' Return position vector of indices corresponding to the
#' input object.
#' 
#' @seealso [mp_positions()]
#' 
#' @param x An object of a class that can be converted to a
#' position vector.
#' 
#' @export
to_positions = function(x) UseMethod("to_positions")

#' @export
to_positions.character = function(x) setNames(seq_along(x) - 1L, x)

#' @export
to_positions.numeric = function(x) as.integer(x)

# take a list of numeric objects and return a list of 
# length-1 integer vectors giving the position of each 
# name in each object that has names
implied_position_vectors = function(numeric_list) {
  (numeric_list
    |> lapply(names) 
    |> Filter(f = is.character)
    |> lapply(to_positions)
    |> unname()
    |> unique()
    |> unlist()
    |> as.list()
  )
}

#' @describeIn names_and_labels Extract the \code{\link{numeric}} column from a
#' data frame with only a single numerical column. This data frame might have
#' more than one column, but only one of them can be numeric. This function
#' will also turn numeric \code{\link{matrix}} and \code{\link{array}} objects
#' with \code{\link{dimnames}} into a flattened numeric vector with labels
#' produced by appropriately dot-concatenating the \code{\link{dimnames}}.
#' 
#' @export
to_values = function(x) UseMethod("to_values")

#' @export
to_values.data.frame = function(x) {
  value_col = which(vapply(x, is.numeric, logical(1L)))
  if (length(value_col) != 1L) {
    stop("Must be exactly one value column to create a vector with values.")
  }
  setNames(x[[value_col]], to_labels(x))
}

#' @export
to_values.numeric = function(x) setNames(as.vector(x), to_labels(x))

list_to_labels = function(...) unlist(lapply(list(...), to_labels), use.names = FALSE)
list_to_names = function(...) unlist(lapply(list(...), to_names), use.names = FALSE)

names_set_op = function(x, y, op = union) op(to_names(x), to_names(y))
name_set_op = function(x, y, op = union) to_name(names_set_op(x, y, op))

frame_to_part = function(frame) {
  # TODO: assert frameness
  if (ncol(frame) == 1L) {
    y = StringDataFromDotted(unique(frame[[1L]]), names(frame))$undot()
  } else {
    y = StringDataFromFrame(unique(frame))
  }
  y
}

#frame_to_part = memoise(frame_to_part)
#

to_matrix_with_rownames = function(x, nms) {
  x = as.matrix(x)
  if (!all(rownames(x) %in% nms)) {
    stop("Matrix rownames are not all in supplied rownames.")
  }
  missing_nms = nms[!nms %in% rownames(x)]
  if (length(missing_nms) > 0L) {
    y = matrix(NA
      , length(missing_nms)
      , ncol(x)
      , dimnames = list(missing_nms, colnames(x))
    )
    x = rbind(x, y)
  }
  x[nms, , drop = FALSE]
}

labelled_zero_vector = function(labels) {
  (rep(0, length(labels))
   |> setNames(labels)
   |> dput()
  )
}

undot_anything = function(x) {
  (x
   |> as.character()
   |> strsplit("\\.")
   |> unlist(use.names = FALSE)
  )
}


wrap_colon_terms = function(x) {
  i = which(grepl(":", x))
  x[i] = sprintf("(%s)", x[i])
  x
}

n_dots = function(x) nchar(x) - nchar(gsub(".", "", x, fixed = TRUE))
make_n_dots = function(n) lapply(n, rep, x = ".") |> vapply(paste0, character(1L), collapse = "")
extrapolate_dots = function(x, string_with_all_dots) {
  required_n_dots = n_dots(string_with_all_dots[[1L]]) - n_dots(x)
  sprintf("%s%s", x, make_n_dots(required_n_dots))
}


match_if_appropriate = function(x, table) {
  if (is.numeric(x)) {
    if (!is.finite(x)) return(x)
    return(as.integer(round(x)))
  }
  x = match(x, table)
  if (any(is.na(x))) stop("cannot find names")
  x
}

name_prefix = function(x, prefix) {
  names(x) = sprintf("%s_%s", prefix, names(x))
  x
}

make_names_list = function(obj, meth_nms) {
  l = list()
  for (nm in meth_nms) l[[nm]] = sprintf("%s_%s", nm, names(obj[[nm]]()))
  l
}

make_nested_names = function(x) {
  nms_x = names(x)
  nested_nms = lapply(x, names)
  null_loc = which(sapply(nested_nms,is.null))
  if (length(null_loc) > 0) {
    nested_nms = nested_nms[-null_loc]
    nms_x = nms_x[-null_loc]
  }
  nms = mapply(sprintf, nested_nms, nms_x, MoreArgs = list(fmt = "%s_%s"))
  make.unique(nms, sep = "_")
}

# @param x vector with names
# @param ... additional arguments giving unnamed character vectors with 
# argument names corresponding to required names in x, and character vectors 
# corresponding to valid synonyms for those names.
rename_synonyms = function(x, ...) {
  synonym_list = list(...)
  nms = names(x)
  for (true_name in names(synonym_list)) {
    synonyms = synonym_list[[true_name]]
    candidates = synonyms %in% nms
    if (any(candidates)) {
      if (sum(candidates) > 1L) stop(paste0("Multiple synonyms in names: ", synonyms[candidates], "\n"))
      nms[synonyms[candidates] == nms] = true_name
    }
  }
  names(x) = nms
  return(x)
}
