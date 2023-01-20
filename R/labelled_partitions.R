## labels objects are vectors that can be dotted
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

## names objects are vectors that cannot be dotted
#' @export
to_names = function(x) UseMethod("to_names")

#' @export
to_names.character = function(x) to_names(StringDottedVector(x))

#' @export
to_names.Partition = function(x) x$names()

#' @export
to_names.StringData = function(x) x$undot()$names()$value()

#' @export
to_names.Scalar = function(x) x$undot()$value()

#' @export
to_names.Names = function(x) x$undot()$value()

## name objects are scalars that can be dotted
#' @export
to_name = function(x) UseMethod("to_name")

#' @export
to_name.character = function(x) {
  if (length(x) == 1L) {
    x = StringDottedScalar(x)
  } else if (length(x) > 1L) {
    x = StringUndottedVector(x)
  } else {
    stop("character vector cannot be turned into a name")
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
#' @export
Partition = function(frame) {
  self = Base()
  self$.partition = frame_to_part(frame)
  self$frame = function() self$.partition$frame()
  self$dotted = function() self$.partition$dot()$frame()
  self$names = function() names(self$frame())
  self$name = function() names(self$dotted())
  self$labels = function() self$dotted()[[1L]]
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
  self$select = function(...) {
    Partition(unique(self$.partition$change_coordinates(...)$frame()))
  }
  return_object(self, "Partition")
}

#' @export
print.Partition = function(x, ...) print(x$frame())

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
