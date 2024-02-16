
StructuredVector = function(x, ...) UseMethod("StructuredVector")


StructuredVector.data.frame = function(x, index = NULL, values_name = "values", ...) {
  nms = setdiff(names(x), values_name)
  if (is.null(index)) index = mp_index(x[, nms, drop = FALSE])
  bad_names = !nms %in% names(index)
  if (any(bad_names)) {
    msg_break(
      msg_colon(
        msg(
          "The following names were found in the values data",
          "but not in the model index"
        ),
        msg_indent(nms[bad_names])
      ),
      msg_colon(
        "The only names in the index are the following",
        msg_indent(names(index))
      )
    ) |> stop()
  }

  index_names = !names(x) %in% values_name
  test_index = try(Index(x[, index_names, drop = FALSE]))
  if (inherits(test_index, "try-error")) stop("Incompatible values data frame")
  bad_labels = !test_index$labels() %in% index$partial_labels(test_index$labelling_column_names)
  if (any(bad_labels)) {
    msg_colon(
      msg(
        "The following labels found in the values data",
        "are being ignored because they are not in the model index"
      ),
      msg_indent(test_index$labels()[bad_labels])
    ) |> warning()
  }

  f = merge(index$partition$frame(), x, all.x = TRUE, sort = FALSE) ## left join
  index_names = !names(f) %in% values_name  ## redefine index_names after merge
  mangled_order = Index(
    f[, index_names, drop = FALSE],
    labelling_column_names = index$labelling_column_names
  )$labels()
  sorted_positions = match(index$labels(), mangled_order)
  values = f[[values_name]][sorted_positions]
  values[is.na(values)] = 0  ## that's the way we roll
  f = f[sorted_positions, index_names, drop = FALSE]
  index = Index(f, labelling_column_names = index$labelling_column_names)
  v = StructuredVector(index)
  v$set_all_numbers(values)
}


StructuredVector.numeric = function(x, index, ...) {
  v = StructuredVector(index)
  index_name = to_name(index$labelling_column_names)
  names(x) = extrapolate_dots(names(x), index_name)
  args = setNames(list(x), index_name)
  do.call(v$set_numbers, args)
}


StructuredVector.StructuredVector = function(x, index, ...) {
  StructuredVector(x$numbers(), index, ...)
}


#' @importFrom utils write.csv write.table
StructuredVector.Index = function(x, ...) {
  self = Base()
  self$index = x
  self$.numbers = zero_vector(self$index$labels())
  self$numbers = function(...) {
    l = list(...)
    ## check if all numbers
    if (length(l) == 0L) return(self$.numbers)
    i = mp_subset(self$index, ...)$labels()
    self$.numbers[i]
  }
  self$set_all_numbers = function(values) {
    self$.numbers = setNames(values, names(self$.numbers))
    self
  }
  self$set_numbers = function(...) {
    ## generate list with: value, filter_cond, select_cond, select_nm
    l = process_grouping_dots(...)

    args = c(list(self$index), l$filter_cond, l$select_cond)
    filter = do.call(mp_subset, args)
    replacement_values = setNames(
      l$value[filter$partition$partial_labels(l$select_nm)],
      filter$labels()
    )

    self$.numbers[names(replacement_values)] = replacement_values
    self
  }
  self$labels_frame = function() {
    data.frame(
      labels = self$index$labels(),
      values = unname(self$numbers())
    )
  }
  self$frame = function() {
    f = self$index$partition$frame()
    f$values = self$numbers()
    f
  }
  self$csv = function(file) {
    write.csv(self$frame(), file, row.names = FALSE, quote = FALSE)
  }
  self$length = function() length(self$.numbers)
  self$clone = function() {
    new = StructuredVector(self$index)
    new$set_all_numbers(self$numbers())
  }
  return_object(self, "StructuredVector")
}


process_grouping_dots = function(...) {
  l = list(...)
  replacement = Filter(is.numeric, l)
  stopifnot(length(replacement) == 1L)
  filter_cond = Filter(is.character, l)
  value = replacement[[1L]]
  select_cond = lapply(replacement, names)
  select_nm = names(select_cond)
  nlist(value, filter_cond, select_cond, select_nm)
}

#' @export
length.StructuredVector = function(x) x$length()

#' @export
print.StructuredVector = function(x, ...) print(x$numbers())

#' @export
names.StructuredVector = function(x) x$numbers() |> names()

#' @export
as.matrix.StructuredVector = function(x, ...) {
  x$numbers() |> as.matrix()
}


#' Structured Vectors
#'
#' This documentation was originally in [mp_index()] and should be cleaned up
#' See issue #131. Also this is an experimental feature.
#'
#' @param x An index.
#' @param ... Passed on to S3 methods.
#'
#' @examples
#' state = mp_index(
#'   Epi = c("S", "I", "S", "I"),
#'   Age = c("young", "young", "old", "old")
#' )
#' state_vector = (state
#'   |> mp_structured_vector()
#'   |> mp_set_numbers(Epi = c(S = 1000))
#'   |> mp_set_numbers(Epi = c(I = 1), Age = "old")
#' )
#' print(state_vector)
#'
#' @export
mp_structured_vector = function(x, ...) UseMethod("mp_structured_vector")

#' @export
mp_structured_vector.StructuredVector = function(x, ...) x

#' @export
mp_structured_vector.Index = StructuredVector.Index

#' @export
mp_structured_vector.data.frame = StructuredVector.data.frame

#' @export
mp_structured_vector.numeric = StructuredVector.numeric

#' @export
mp_structured_vector.character = function(x, ...) zero_vector(x)

#' @export
mp_structured_vector.Link = function(x, dimension_name, ...) {
  mp_structured_vector(x$labels_for[[dimension_name]]())
}

#' @param vector An index.
#' @describeIn mp_structured_vector Update numerical values of a structured
#' vector. TODO: details on syntax.
#' @export
mp_set_numbers = function(vector, ...) vector$clone()$set_numbers(...)


## FIXME: Not used??
VectorList = function() {
  self = Base()
  self$list = list() |> setNames(as.character())
  self$add = function(new_vec, ...) {
      new_vecs = list(...)
    if (missing(new_vec)) {
      valid$named_list$check(new_vecs)
    } else if(length(new_vecs) == 0L) {
      new_nm = deparse1(substitute(new_vec))
      new_vecs = setNames(list(new_vec), new_nm)
    } else {
      stop("If supplying more than one vector, please name them with {name} = {vector}")
    }

    for (nm in names(new_vecs)) {
      if (nm %in% names(self$list)) {
        msg(
          "StructuredVector", nm, "is already in the list.",
          "Overwriting the existing one."
        ) |> message()
      }
      if (inherits(new_vecs[[nm]], "Index")) {
        new_vecs[[nm]] = mp_structured_vector(new_vecs[[nm]])
      }
      self$list[[nm]] = new_vecs[[nm]]
    }
  }
  return_object(self, "VectorList")
}

