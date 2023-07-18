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
#' * `$partial_labels()` -- TODO
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
#' * `$expand(name)` -- TODO
#' * `$union(other)` -- TODO
#'
#' ## Fields
#'
#' * `products`
#'
#' @export
Partition = function(frame) {
  self = Base()
  self$.partition = frame_to_part(frame)
  self$products = Products(self)
  self$frame = function() self$.partition$frame()
  self$dotted = function() self$.partition$dot()$frame()
  self$names = function() names(self$frame())
  self$name = function() names(self$dotted())
  self$labels = function() self$dotted()[[1L]]
  self$partial_labels = function(...) {
    new_names = list_to_names(...)
    self$.partition$change_coordinates(new_names)$dot()$frame()[[1L]]
  }
  self$.filter = function(..., .wrt, .comparison_function, .filter_type) {
    if (missing(.wrt)) .wrt = self$name()
    .wrt = name_set_op(self$name(), .wrt, intersect)
    nms = to_names(.wrt)
    labels = list_to_labels(...)
    if (is.null(labels)) {  ## no filtering names are supplied
      p = NullPartition(.wrt)
    } else if (!any(labels %in% self$select(nms)$labels())) {  ## no filtering names match
      p = NullPartition(.wrt)
    } else {  ##
      filterer = StringDataFromDotted(labels = labels, name = .wrt)
      p = Partition(self$.partition[[.filter_type]](filterer, .comparison_function)$frame())
    }
    return(p)
  }
  self$filter = function(..., .wrt, .comparison_function = all_equal) {
    self$.filter(...
      , .wrt = .wrt
      , .comparison_function = .comparison_function
      , .filter_type = "filter"
    )
  }
  self$filter_out = function(..., .wrt, .comparison_function = not_all_equal) {
    self$.filter(...
      , .wrt = .wrt
      , .comparison_function = .comparison_function
      , .filter_type = "filter"
    )
  }
  self$filter_ordered = function(..., .wrt, .comparison_function = all_equal) {
    self$.filter(...
      , .wrt = .wrt
      , .comparison_function = .comparison_function
      , .filter_type = "ordered_unique_filter"
    )
  }
  self$select = function(...) {
    Partition(unique(self$.partition$change_coordinates(...)$frame()))
  }
  self$select_out = function(...) {
    self$select(setdiff(self$names(), unlist(list(...), recursive = TRUE)))
  }
  self$expand = function(name) Partition(self$.expand(name))
  self$.expand = function(name) self$.partition$expand(name)$frame()
  self$union = function(other) {
    new_name = name_set_op(self$name(), other$name(), union)
    x = self$.expand(new_name)
    y = other$.expand(new_name)
    Partition(rbind(x, y))
  }
  return_object(self, "Partition")
}

NullPartition = function(...) {
  self = Base()
  self$.names = list_to_names(...)
  self$frame = function() empty_frame(self$.names)
  self$dotted = function() empty_frame(to_name(self$.names))
  self$names = function() self$.names
  self$name = function() to_name(self$.names)
  self$labels = function() character(0L)
  self$partial_labels = function(...) {
    stopifnot(all(list_to_names(...) %in% self$names()))
    self$labels()
  }
  self$filter = function(..., .wrt, .comparison_function = all_equal) self
  self$filter_out = function(..., .wrt, comparison_function = not_all_equal) self
  self$filter_ordered = function(..., .wrt, .comparison_function = all_equal) self
  self$select = function(...) {
    new_names = list_to_names(...)
    stopifnot(all(new_names %in% self$names()))
    NullPartition(new_names)
  }
  self$select_out = function(...) {
    new_names = name_set_op(self$name(), list_to_names(...), setdiff)
    NullPartition(new_names)
  }
  self$expand = function(name) NullPartition(names(self$.expand(name)))
  self$.expand = function(name) empty_frame(name_set_op(self$name(), name, union))
  self$union = function(other) {
    new_name = name_set_op(self$name(), other$name(), union)
    other$expand(new_name)
  }
  self = return_object(self, "Partition")
  return_object(self, "NullPartition")
}

#' @export
print.Partition = function(x, ...) print(x$frame())

empty_frame = function(...) {
  colnames = unlist(
    lapply(list(...), as.character),
    use.names = FALSE,
    recursive = TRUE
  )
  ncol = length(colnames)
  setNames(as.data.frame(matrix(character(), 0L, ncol)), colnames)
}

#' Union of Variables
#'
#' Take the union of a set of variable lists, each of which is represented
#' by a \code{\link{Partition}} object.
#'
#' @param ... \code{\link{Partition}} objects to combine.
#'
#' @export
union_vars = function(...) {
  not_null = function(x) !is.null(x)
  l = Filter(not_null, list(...))
  y = l[[1L]]
  if (length(l) > 1L) {
    for (i in 2:length(l)) {
      y = y$union(l[[i]])
    }
  }
  y
}

NumericPartition = function(frame, numeric_vector) {
  if (nrow(frame) != length(numeric_vector)) stop("Inconsitent numeric partition.")
  self = Base()
  self$partition = Partition(frame)
  self$vector = setNames(
    valid$num_vec$assert(numeric_vector),
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
    v = self$filter_vector(name, .wrt = "Matrix")
    if (isTRUE(row_part == "")) {
      rn = ""
      i = 1
    } else {
      r = self$partition$filter(name, .wrt = "Matrix")$partial_labels(row_part)
      rn = unique(r)
      i = match(r, rn)
    }
    if (isTRUE(col_part == "")) {
      cn = ""
      j = 1
    } else {
      c = self$partition$filter(name, .wrt = "Matrix")$partial_labels(col_part)
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



if (FALSE) {
  model_dirs = list.files(system.file("starter_models", package = "macpan2"), full.names = TRUE)
  models = setNames(lapply(model_dirs, ModelFiles), basename(model_dirs))
  pp = Partition(models$seir_symp_vax$variables$all())
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
  seir = Partition(models$seir$variables$all())
  vax = Partition(models$vax$variables$all())

  models$seir$settings()$required_partitions
  models$seir$settings()$state_variables

  m = Model(models$seir_symp_vax)
  m$variables$all()
  m$variables$flow()
  m$variables$state()
  m$flows()
  m$flows_expanded()
  m$derivations()
}

if (FALSE) {
  make_expression = function(model, expr_id, grp_id) {
    v = model$variables$all()
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
vv = m$variables$all()
gg = vv$filter(ee$group_names[j], .wrt = ee$group_partition)
oo = vv$filter(ee$output_names[j], .wrt = ee$output_partition)
##ii = gg$filter(ee$argument_dots, .wrt = ee$input_partition)
##ff = MathExpressionFromStrings(ee$expression, character(0L), include_dots = TRUE)
ii = gg$filter(ee$arguments, .wrt = ee$input_partition)
ff = MathExpressionFromStrings(ee$expression, ee$arguments)
do.call(ff$symbolic$evaluate, as.list(ii$labels()))
}
