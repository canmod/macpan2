valid_char = ValidityMessager(is.character)
valid_undotted_chars = function(x) {
  #browser()
  if (length(x) == 0L) return(is.character(x))
  grepl("^([A-Za-z]{1}[A-Za-z0-9_]*|)$", x)
}
valid_dotted_chars = function(x) {
  #browser()
  if (length(x) == 0L) return(is.character(x))
  grepl("^([A-Za-z.]{1}[A-Za-z0-9_.]*|)$", x)
}

valid_undotted = ValidityMessager(
  All(
    is.character,
    TestPipeline(Summarizer(valid_undotted_chars, all), TestTrue())
  ),
  "vector contained invalid strings"
)
valid_dotted = ValidityMessager(
  All(
    is.character,
    TestPipeline(Summarizer(valid_dotted_chars, all), TestTrue())
  )
)
valid_scalar = ValidityMessager(
  All(
    TestPipeline(Summarizer(length), TestRange(1L, 1L)),
    TestPipeline(Summarizer(dim, is.null), TestTrue())
  )
)
valid_vector = ValidityMessager(
  TestPipeline(Summarizer(dim, is.null), TestTrue())
)
valid_matrix = ValidityMessager(
  TestPipeline(Summarizer(dim, length), TestRange(2L, 2L))
)

test_is = local({
  a = c("Dotted", "Undotted")
  b = c("Scalar", "Vector", "Matrix")
  case_list = c(a, b)
  case_pairs = expand.grid(a, b)
  c(
    String = Is("String"),
    setNames(
      lapply(case_list, function(x) All(Is("String"), Is(x))),
      case_list
    ),
    setNames(
      apply(case_pairs, 1L, function(x) All(Is("String"), Is(x[1]), Is(x[2]))),
      apply(case_pairs, 1L, function(x) paste(x, collapse = ""))
    ),
    UndottedData = All(Is("StringData"), Is("Undotted")),
    DottedData = All(Is("StringData"), Is("Dotted"))
  )
})

valid_labels_dv = ValidityMessager(test_is$DottedVector)
valid_names_ds = ValidityMessager(test_is$DottedScalar)
valid_labels_um = ValidityMessager(test_is$UndottedMatrix)
valid_names_uv = ValidityMessager(test_is$UndottedVector)

#' Comparison Functions
#'
#' @param x \code{\link{character}} object
#' @param y \code{\link{character}} object
#'
#' @importFrom memoise memoise
#' @name comparison
NULL

#' @export
#' @describeIn comparison Is it true that all corresponding elements of \code{x}
#' and \code{y} are equal, have the same shape, and have no missing values?
all_equal = function(x, y) isTRUE(all(x == y))

#' @describeIn comparison Is it true that all corresponding elements of \code{x}
#'  and \code{y} are either equal or at least one is a blank string, have the
#'  same shape, and have no missing values?
#' @export
all_consistent = function(x, y) isTRUE(all((x == y) | (x == "") | (y == "")))

#' @describeIn comparison Complement of \code{all_equal}.
#' @export
not_all_equal = function(x, y) !all_equal(x, y)

#' @describeIn comparison Do not know yet. Currently unused; should we remove?
#' @export
all_not_equal = function(x, y) isTRUE(all(x != y))

## used in $filter() of StringUndottedMatrix
character_comparison = function(x, y, comparison_function) {
  z = logical(nrow(x))
  for (i in seq_row(x)) {
    for (j in seq_row(y)) {
      z[i] = comparison_function(x[i, , drop = TRUE], y[j, , drop = TRUE])
      if (z[i]) break
    }
  }
  z
}

character_mat_scal_comparison = function(x, y, comparison_function) {
  z = logical(nrow(x))
  for (i in seq_row(x)) {
    z[i] = comparison_function(x[i, , drop = TRUE], y)
    if (z[i]) break
  }
  z
}

## these functions are bottlenecks that
## get repeatedly called for the same inputs.
## so we use memoisation to solve this performance issue
## https://en.wikipedia.org/wiki/memoization
character_comparison = memoise(character_comparison)
# all_equal = memoise(all_equal)
# all_consistent = memoise(all_consistent)
# not_all_equal = memoise(not_all_equal)
# all_not_equal = memoise(all_not_equal)

seq_row = function(x) seq_len(nrow(x))
seq_col = function(x) seq_len(ncol(x))

## TODO: ensure liskov substitution for the following pairs:
## DottedScalar and UndottedVector
## DottedVector and UndottedMatrix
## DottedData and UndottedData
##
## note that the following need no substitution requirement:
## UndottedScalar, DottedMatrix

String = function(x) {
  self = Base()
  self$.value = valid_char$assert(x)
  self$value = function() self$.value
  return_object(self, "String")
}

#' @export
print.String = function(x, ...) {
  cat("String object with the following $value():\n")
  print(x$value())
}
StringDotted = function(x) {
  self = String(valid_dotted$assert(x))
  self$dot = function() self
  return_object(self, "Dotted")
}
StringUndotted = function(x) {
  self = String(valid_undotted$assert(x))
  self$undot = function() self
  return_object(self, "Undotted")
}
StringDottedScalar = function(...) {
  self = StringDotted(valid_scalar$assert(c(...)))
  self$regenerate = function(value) StringDottedScalar(value)
  self$value_combiner = function(value_list) {
    StringDottedVector(do.call(c, value_list))
  }
  self$undot = function() {
    v = self$value()
    if (!identical(v, "")) {
      d = read.table(text = v
        , sep = "."
        , na.strings = character()
        , colClasses = "character"
      )
      v = unlist(d, use.names = FALSE)
    }
    StringUndottedVector(v)
  }
  self$tuple_length = function() {
    length(self$undot()$value())
  }
  return_object(self, c("Scalar", "Names"))
}
StringUndottedScalar = function(...) {
  self = StringUndotted(valid_scalar$assert(c(...)))
  self$regenerate = function(value) StringUndottedScalar(value)
  self$value_combiner = function(value_list) {
    StringUndottedVector(do.call(c, value_list))
  }
  self$dot = function() {
    warning("Undotted scalars cannot be dotted")
    self
  }
  return_object(self, "Scalar")
}
StringDottedVector = function(...) {
  self = StringDotted(valid_vector$assert(c(...)))
  self$regenerate = function(value) StringDottedVector(value)
  self$undot = function() {
    v = self$value()
    if (!identical(v, "")) {
      d = read.table(text = v
        , sep = "."
        , na.strings = character()
        , colClasses = "character"
      )
      m = as.matrix(d)
      rownames(m) = colnames(m) = NULL
    } else {
      m = matrix(v)
    }
    StringUndottedMatrix(m)
  }
  self$which_in = function(other, comparison_function) {
    self$undot()$which_in(other$undot(), comparison_function)
  }
  self$which_not_in = function(other, comparison_function) {
    self$undot()$which_not_in(other$undot(), comparison_function)
  }
  self$which_dup = function() {
    duplicated(self$value())
  }
  self$unique = function() {
    self$subset(!self$which_dup())
  }
  self$subset = function(i) {
    StringDottedVector(self$value()[i])
  }
  self$tuple_length = function() {
    ncol(self$undot()$value())
  }
  return_object(self, c("Vector", "Labels"))
}
StringUndottedVector = function(...) {
  self = StringUndotted(valid_vector$assert(c(...)))
  self$regenerate = function(value) StringUndottedVector(value)
  self$dot = function() {
    s = paste0(as.character(self$value()), collapse = ".")
    StringDottedScalar(s)
  }
  self$tuple_length = function() {
    length(self$value())
  }
  return_object(self, c("Vector", "Names"))
}
StringDottedMatrix = function(...) {
  self = StringDotted(valid_matrix$assert(rbind(...)))
  self$regenerate = function(value) StringDottedMatrix(value)
  self$undot = function() {
    stop("not implemented, but why not?")
  }
  return_object(self, "Matrix")
}
StringUndottedMatrix = function(...) {
  self = StringUndotted(valid_matrix$assert(rbind(...)))
  self$regenerate = function(value) StringUndottedMatrix(value)
  self$dot = function() {
    paste_with_dot = function(x) paste0(x, collapse = ".")
    v = apply(self$value(), 1L, paste_with_dot)
    StringDottedVector(v)
  }
  self$which_in = function(other, comparison_function) {
    character_comparison(self$value()
      , other$undot()$value()
      , comparison_function
    )
  }
  self$which_not_in = function(other, comparison_function) {
    x = self$value()
    y = other$value()
    w = logical(nrow(y))
    z = logical(nrow(x))
    for (i in seq_row(x)) {
      for (j in seq_row(y)) {
        w[j] = comparison_function(x[i,,drop = TRUE], y[j,,drop = TRUE])
      }
      if(all(w==TRUE)) z[i]=TRUE
    }
    z
  }
  self$order_by = function(other, comparison_function) {
    x = self$value()
    y = other$value()
    z = integer(nrow(y))
    for (i in seq_row(y)) {
      for (j in seq_row(x)) {
        if (comparison_function(x[j,,drop = TRUE], y[i,,drop = TRUE])) {
          if (z[i] == 0L) {
            z[i] = j
          } else {
            stop("Lack of uniqueness")
          }
        }
      }
    }
    z
  }
  self$which_dup = function() {
    self$dot()$which_dup()
  }
  self$unique = function() {
    self$subset(!self$which_dup())
  }
  self$subset = function(i) {
    StringUndottedMatrix(self$value()[i,,drop = FALSE])
  }
  self$tuple_length = function() {
    ncol(self$value())
  }
  return_object(self, c("Matrix", "Labels"))
}

#' @export
c.String = function(...) {
  l = list(...)
  r = list()
  for (i in seq_along(l)) {
    r[[i]] = l[[i]]$value()
  }
  l[[1L]]$value_combiner()
  l[[1L]]$regenerate(do.call(l[[1L]]$value_combiner, r))
}

StringData = function(labels, names) {
  self = Base()
  if (labels$tuple_length() != names$tuple_length()) {
    stop("Labels and names refer to different partition set dimensions")
  }
  self$.labels = labels
  self$.names = names
  self$labels = function() self$.labels
  self$names = function() self$.names
  self$frame = function() {
    setNames(as.data.frame(self$labels()$value()), self$names()$value())
  }
  self$unique = function() {
    self$.labels = self$.labels$unique()
    self
  }
  return_object(self, "StringData")
}

#' @export
c.StringData = function(...) {
  l = list(...)
  r = list()
  for (i in seq_along(l)) {
    r[[i]] = l[[i]]$labels()$value()
  }
  l[[1L]]$regenerate(do.call(rbind, r))
}

StringDottedData = function(labels, names) {

  self = StringData(valid_labels_dv$assert(labels), valid_names_ds$assert(names))
  if (any(duplicated(self$names()$undot()$value()))) {
    stop("String data cannot have duplicated names.")
  }
  self$undot = function() {
    StringUndottedData(self$labels()$undot(), self$names()$undot())
  }
  self$dot = function() self
  self$change_coordinates = function(...) {
    names = StringDottedScalar(as.character(c(...)))$undot()$value()
    self$undot()$change_coordinates(names)$dot()
  }
  self$filter = function(other, comparison_function) {
    self$undot()$filter(other$undot(), comparison_function)$dot()
  }
  self$filter_out = function(other, comparison_function) {
    self$undot()$filter_out(other$undot(), comparison_function)$dot()
  }
  self$filter_other = function(other, comparison_function) {
    self$undot()$filter_other(other$undot(), comparison_function)$dot()
  }
  self$regenerate = function(labels) StringDottedData(labels, self$names())
  self$expand = function(names) self$undot()$expand(names)$dot()
  #self$group = function(other, comparison_function) {
  #
  #}
  return_object(self, "Dotted")
}

StringUndottedData = function(labels, names) {
  self = StringData(valid_labels_um$assert(labels), valid_names_uv$assert(names))
  if (any(duplicated(self$names()$value()))) {
    stop("String data cannot have duplicated names.")
  }
  self$undot = function() self
  self$dot = function() {
    StringDottedData(self$labels()$dot(), self$names()$dot())
  }
  self$change_coordinates = function(...) {
    names = as.character(unlist(list(...), use.names = FALSE))
    all_names = self$names()$value()
    components_to_keep = match(names, all_names)
    if (any(is.na(components_to_keep))) {
      stop("Some names are not found in the string data")
    }
    StringUndottedData(
      labels = StringUndottedMatrix(
        self$labels()$value()[, components_to_keep, drop = FALSE]
      ),
      names = StringUndottedVector(
        self$names()$value()[components_to_keep]
      )
    )
  }
  self$filter = function(other, comparison_function) {
    other = other$undot()
    coordinates = other$names()$value()
    x = self$change_coordinates(coordinates)
    y = other$change_coordinates(coordinates)
    z = x$labels()$which_in(y$labels(), comparison_function)
    StringUndottedData(
      labels = self$labels()$subset(z),
      names = self$names()
    )
  }
  self$filter_out = function(other, comparison_function) {
    other = other$undot()
    coordinates = other$names()$value()
    x = self$change_coordinates(coordinates)
    y = other$change_coordinates(coordinates)
    z = x$labels()$which_not_in(y$labels(), comparison_function)
    StringUndottedData(
      labels = self$labels()$subset(z),
      names = self$names()
    )
  }
  self$ordered_unique_filter = function(other, comparison_function) {
    other = other$undot()
    coordinates = other$names()$value()
    x = self$change_coordinates(coordinates)
    y = other$change_coordinates(coordinates)
    z = x$labels()$order_by(y$labels(), comparison_function)
    StringUndottedData(
      labels = self$labels()$subset(z),
      names = self$names()
    )
  }
  self$filter_other = function(other, comparison_function) {
    this = self$undot()
    coordinates = this$names()$value()
    x = other$change_coordinates(coordinates)
    y = this$change_coordinates(coordinates)
    z = x$labels()$which_in(y$labels(), comparison_function)
    StringUndottedData(
      labels = other$labels()$subset(z),
      names = other$names()
    )
  }
  self$filter_groups = function(other_list, comparison_function) {
    lapply(other_list, self$filter, comparison_function)
  }
  self$group = function(other, comparison_function) {
    other = other$undot()
    lapply(other$split_rows(), self$filter, comparison_function)
  }
  self$split_rows = function() {
    d = self$frame()
    lapply(split(d, seq_row(d)), StringDataFromFrame)
  }
  self$regenerate = function(labels) StringUndottedData(labels, self$names())
  self$expand = function(name) {
    new_names = setdiff(
      StringDottedScalar(name)$undot()$value(),
      self$names()$value()
    )
    new_list = setNames(rep(list(""), length(new_names)), new_names)
    StringDataFromFrame(as.data.frame(c(self$frame(), new_list)))
  }
  return_object(self, "Undotted")
}

#' String Data
#'
#' Create objects for representing names and labels in a compartmental
#' model.
#'
#' @examples
#' path = system.file("starter_models", "seir_symp_vax", package = "macpan2")
#' model = ModelFiles(path)
#' vars = StringDataFromFrame(model$variables())
#' vars
#' vars$dot()
#'
#'
#' @name StringData
NULL


#' @param data Data frame with names given by column names and labels by
#' the elements of the columns.
#' @describeIn StringData Construct object from a data frame without any
#' dots in either the names or the values.
#' @export
StringDataFromFrame = function(data) {
  m = as.matrix(data)
  rownames(m) = colnames(m) = NULL
  StringUndottedData(
    labels = StringUndottedMatrix(m),
    names = StringUndottedVector(names(data))
  )
}

#' @param labels Character vector with (dot-separated) partition labels.
#' @param name Character scalar with (dot-separated) partition name.
#' @describeIn StringData Construct object from a character scalar with
#' (dot-separated) partition names and a character vector with (dot-separated)
#' partition labels.
#' @export
StringDataFromDotted = function(labels, name) {
  StringDottedData(
    labels = StringDottedVector(labels),
    names = StringDottedScalar(name)
  )
}

#' @param x \code{StringData} object
#' @param ... Not used but present for S3 method consistency.
#' @describeIn StringData Print out a `StringData` object.
#' @export
print.StringData = function(x, ...) {
  cat("String data object with the following $frame():\n")
  print(x$frame())
}
