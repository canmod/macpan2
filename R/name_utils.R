#' To Labels
#'
#' Convert objects to labels, which are vectors that might be dotted.
#'
#' @param x Object to convert to labels.
#' @return Character vector that can be used as labels.
#'
#' @export
to_labels = function(x) UseMethod("to_labels")

#' @export
to_labels.character = function(x) valid_dotted$assert(x)

#' @export
to_labels.Partition = function(x) x$labels()


#' @export
to_labels.data.frame = function(x) StringDataFromFrame(x)$dot()$labels()$value()

#' @export
to_labels.StringData = function(x) x$dot()$labels()$value()

#' @export
to_labels.Scalar = function(x) x$dot()$value()

#' @export
to_labels.Vector = function(x) x$dot()$value()

#' @export
to_labels.Labels = function(x) x$dot()$value()

#' To Names
#'
#' Convert objects to names, which are character vectors with the following
#' restrictions:  (1) they cannot have dots, (2) all values must start with
#' a letter, (3) all characters must be letters, numbers, or underscore.
#'
#' @param x Object to convert to names.
#' @return Character vector that can be used as names.
#'
#' @export
to_names = function(x) UseMethod("to_names")

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


#' To Name
#'
#' Convert objects to a name, which is a scalar string that can be dotted.
#'
#' @param x Object to convert to labels.
#' @return Character string that can be used as a name.
#'
#' @export
to_name = function(x) UseMethod("to_name")

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
to_positions.numeric = as.integer

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
