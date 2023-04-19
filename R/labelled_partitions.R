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

frame_to_part = function(frame) {
  # TODO: assert frameness
  if (ncol(frame) == 1L) {
    y = StringDataFromDotted(unique(frame[[1L]]), names(frame))$undot()
  } else {
    y = StringDataFromFrame(unique(frame))
  }
  y
}

if (FALSE) {
  ee = list(
    us = macpan2:::StringUndottedScalar("d"),
    ds = macpan2:::StringDottedScalar("d.g.d"),
    uv = macpan2:::StringUndottedVector("d", "g", "d"),
    dv = macpan2:::StringDottedVector("d.g.d", "e.g.e"),
    um = macpan2:::StringUndottedMatrix(matrix(c("a", "b", "c", "d"), 2, 2)),
    dm = macpan2:::StringDottedMatrix(matrix(c("a.b", "b.b", "c.e", "d.d"), 2, 2))
  )
  lapply(ee, class)
}

#' Partition
#'
#' Create object for manipulating partitions, which are sets of
#' labels for representing and naming model entities.
#'
#' @param frame Data frame representing the partition.
#'
#' @return Object of class \code{Partition} with the following methods.
#'
#' ## Methods
#'
#' * `$frame()` -- The `Partition` object as a data frame.
#' * `$dotted()` -- The `Partition` object as a data frame with one united column.
#' * `$names()` -- The names of the `Partition` (i.e. the column names).
#' * `$name()` -- The name of the `Partition` (i.e. the dot-concatenated column names).
#' * `$labels()` -- The labels of the `Partition` (i.e. the row-wise dot-concatenated columns).
#' * `$filter(..., .wrt, .comparison_function)` -- Filter by keeping only a subset of labels.
#' * `$filter_out(..., .wrt, .comparison_function)` -- Filter by removing a subset of labels.
#' * `$filter_ordered(..., .wrt, .comparison_function = all_equal)` -- Filter and order by labels.
#'    * `...` -- Labels to filter
#'    * `.wrt` -- The filtering labels are with respect to a particular `Partition`, and
#'    `.wrt` is the name of this `Partition`.
#'    * `.comparison_function` -- Boolean function to decide if each filtering label is
#'    equal to each label in the `Partition`.
#' * `$select(...)` -- Create a new `Partition` with a subset of names. The rows of the new
#' `Partition` are de-duplicated.
#' * `$select_out(...)` -- Create a new `Partition` without a subset of names.
#'    * `...` -- Names to keep in the resulting `Partition`.
#'
#' @export
Partition = function(frame) {
  self = Base()
  self$products = Products(self)
  self$.partition = frame_to_part(frame)
  self$frame = function() self$.partition$frame()
  self$dotted = function() self$.partition$dot()$frame()
  self$names = function() names(self$frame())
  self$name = function() names(self$dotted())
  self$labels = function() self$dotted()[[1L]]
  self$partial_labels = function(...) {
    self$.partition$change_coordinates(...)$dot()$frame()[[1L]]
  }
  self$filter = function(..., .wrt, .comparison_function = all_equal) {
    if (missing(.wrt)) {
      .wrt = self$names()
      if (length(.wrt) != 1L) .wrt = list_to_names(...)[[1L]]
    }
    filterer = StringDataFromDotted(
      labels = list_to_labels(...), names = to_name(.wrt)
    )
    Partition(self$.partition$filter(filterer, .comparison_function)$frame())
  }
  self$filter_out = function(..., .wrt, .comparison_function = not_all_equal) {
    if (missing(.wrt)) {
      .wrt = self$names()
      if (length(.wrt) != 1L) .wrt = list_to_names(...)[[1L]]
    }
    filterer = StringDataFromDotted(
      labels = list_to_labels(...), names = to_name(.wrt)
    )
    Partition(self$.partition$filter_out(filterer, .comparison_function)$frame())
  }
  self$filter_ordered = function(..., .wrt, .comparison_function = all_equal) {
    # step 1: process case of missing .wrt argument
    if (missing(.wrt)) {
      .wrt = self$names()
      if (length(.wrt) != 1L) .wrt = list_to_names(...)[[1L]]
    }

    # step 2: construct the StringData object to use as the filter
    filterer = StringDataFromDotted(
      labels = list_to_labels(...), names = to_name(.wrt)
    )

    # step 3: apply the filter to the StringData object in self$.partition
    Partition(self$.partition$ordered_unique_filter(filterer, .comparison_function)$frame())
  }
  self$select = function(...) {
    Partition(unique(self$.partition$change_coordinates(...)$frame()))
  }
  self$select_out = function(...) {
    self$select(setdiff(self$names(), unlist(list(...), recursive = TRUE)))
  }
  self$expand = function(name) {
    Partition(self$.partition$expand(name)$frame())
  }
  self$union = function(other) {
    new_names = StringUndottedVector(union(self$names(), other$names()))$dot()$value()
    x = self$.partition$expand(new_names)$frame()
    y = other$.partition$expand(new_names)$frame()
    Partition(rbind(x, y))
  }
  return_object(self, "Partition")
}

#' @export
print.Partition = function(x, ...) print(x$frame())

#' Union of Variables
#'
#' Take the union of a set of variable lists, each of which is represented
#' by a \code{\link{Partition}} object.
#'
#' @param ... \code{\link{Partition}} objects to combine.
#'
#' @export
union_vars = function(...) {
  l = list(...)
  y = l[[1L]]
  for (i in 2:length(l)) {
    y = y$union(l[[i]])
  }
  y
}

NumericPartition = function(frame, numeric_vector) {
  if (nrow(frame) != length(numeric_vector)) stop("Inconsitent numeric partition.")
  self = Base()
  self$partition = Partition(frame)
  self$vector = setNames(
    macpan2:::valid$num_vec$assert(numeric_vector),
    self$partition$select_out("Matrix")$labels()
  )
  self$filter_vector = function(..., .wrt, .comparison_function = all_equal) {
    l = self$partition$filter(...
      , .wrt = .wrt
      , .comparison_function = .comparison_function
    )$select_out("Matrix")$labels()
    self$vector[l]
  }
  self$matrix = function(name, row_part, col_part) {
    row_col_names = character(0L)
    if (row_part != "") row_part = row_col_names = to_names(row_part)
    if (col_part != "") {
      col_part = to_names(col_part)
      if (length(intersect(col_part, row_part)) != 0L) {
        stop("Row and column partitions must be mutually exclusive.")
      }
      row_col_names = c(row_col_names, col_part)
    }
    all_names = self$partition$select_out("Matrix")$names()
    if (!all(row_col_names %in% all_names)) {
      stop("Matrix row and column labels must use all label components.")
    }
    if (!all(all_names %in% row_col_names)) {
      stop("Some row/column labels not found in the partition.")
    }
    v = self$filter_vector("state", .wrt = "Matrix")
    if (isTRUE(row_part == "")) {
      rn = ""
      i = 1
    } else {
      r = self$partition$filter("state", .wrt = "Matrix")$partial_labels(row_part)
      rn = unique(r)
      i = match(r, rn)
    }
    if (isTRUE(col_part == "")) {
      cn = ""
      j = 1
    } else {
      c = self$partition$filter("state", .wrt = "Matrix")$partial_labels(col_part)
      cn = unique(c)
      j = match(c, cn)
    }
    m = matrix(0, length(rn), length(cn))
    m[cbind(i, j)] = v
    dimnames(m) = list(rn, cn)
    m
  }
  return_object(self, "NumericPartition")
}



if (interactive()) {
  model_dirs = list.files(system.file("starter_models", package = "macpan2"), full.names = TRUE)
  models = setNames(lapply(model_dirs, ModelFiles), basename(model_dirs))
  pp = Partition(models$seir_symp_vax$variables())
  qq = pp$filter("S", "E", "I", "R", .wrt = "Epi")$filter("unvax", .wrt = "Vax")
  Partition(pp$select("Epi", "Vax")$dotted())
  pp$frame()
  #pp$filter(qq$select("Epi", "Vax"), "foi.unvax" , .wrt = "Epi.Vax")
  qq = pp$select("Epi")$filter("S", .wrt = "Epi")
  pp$filter(qq)
  pp$filter("S", "I", .wrt = "Epi")$filter("unstructured", "component", .wrt = "SympStruc")
  pp$filter("I.component", .wrt = "Epi.SympStruc")
  pp$name()
  pp$names()
  pp$labels()
  pp$frame()
  pp$dotted()

  pp$filter("S", .wrt = "Epi", .comparison_function = not_all_equal)
  pp$filter_out("S", .wrt = "Epi")
  seir = Partition(models$seir$variables())
  vax = Partition(models$vax$variables())

  models$seir$settings()$required_partitions
  models$seir$settings()$state_variables

  m = Model(models$seir_symp_vax)
  m$variables()
  m$flow_variables()
  m$state_variables()
  m$flows()
  m$flows_expanded()
  m$derivations()
}

if (FALSE) {
  make_expression = function(model, expr_id, grp_id) {
    v = model$variables()
    e = model$derivations()[[expr_id]]
    if (!is.null(e$filter_partition)) {
      v = v$filter(e$filter_names, .wrt = e$filter_partition)
    }
    if (!is.null(e$group_partition)) {
      g = v$filter(e$group_names[grp_id], .wrt = e$group_partition)
      o = v$filter(e$output_names[grp_id], .wrt = e$output_partition)
    } else {
      g = v
      o = v$filter(e$output_names[1], .wrt = e$output_partition)
    }
    a = c(character(0L), e$arguments, e$argument_dots)
    a
  }
}

if (FALSE) {

i = 3
j = 1
ee = m$derivations()[[i]]
vv = m$variables()
gg = vv$filter(ee$group_names[j], .wrt = ee$group_partition)
oo = vv$filter(ee$output_names[j], .wrt = ee$output_partition)
##ii = gg$filter(ee$argument_dots, .wrt = ee$input_partition)
##ff = MathExpressionFromStrings(ee$expression, character(0L), include_dots = TRUE)
ii = gg$filter(ee$arguments, .wrt = ee$input_partition)
ff = MathExpressionFromStrings(ee$expression, ee$arguments)
do.call(ff$symbolic$evaluate, as.list(ii$labels()))
}
