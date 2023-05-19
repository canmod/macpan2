#' To Labels
#'
#' Convert objects to labels, which are vectors that can be dotted.
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
#' a letter, (3) all characters must be letters, numbers, or underscores.
#'
#' @param x Object to convert to names.
#' @return Character vector that can be used as names.
#'
#' @export
to_names = function(x) UseMethod("to_names")

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
to_name.StringData = function(x) x$dot()$names()$value()

#' @export
to_name.Scalar = function(x) x$dot()$value()

#' @export
to_name.Names = function(x) x$dot()$value()

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