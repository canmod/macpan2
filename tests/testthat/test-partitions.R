r = CSVReader(system.file("model_library", "sir_vax", "variables.csv", package = "macpan2"))
x = macpan2:::Partition(r$read())
x$name()

process_partition_names = function(...) {
  (list(...)
    |> lapply(as.character)
    |> unlist(recursive = TRUE, use.names = FALSE)
    |> paste0(collapse = ".")
    |> strsplit(".", fixed = TRUE)
    |> unlist(recursive = TRUE, use.names = FALSE)
  )
}
process_labels = function(...) {
  (list(...)
    |> lapply(as.character)
    |> unlist(recursive = TRUE, use.names = FALSE)
  )
}
rm_blank_rows = function(partition) {
  partition[apply(partition$frame, 1L, all_equal, ""), , drop = FALSE]
}
valid_rows = function(partition) {
  partition$frame |> unique() |> rm_blank_rows() |> PartitionAlt()
}
filter_join <- function(x, y, by = NULL, type = c("anti", "semi")) {
  type <- match.arg(type, choices = c("anti", "semi"), several.ok = FALSE)
  if (is.null(by)) {
    by <- intersect(names(x), names(y))
  }
  rows <- interaction(x[, by]) %in% interaction(y[, by])
  if (type == "anti") rows <- !rows
  res <- x[rows, , drop = FALSE]
  rownames(res) <- NULL
  res
}

PartitionAlt = function(frame) {
  self = Base()
  rownames(frame) = NULL
  self$frame = frame
  self$names = function() names(self$frame)
  self$name = function() paste(self$names(), collapse = ".")
  self$labels = function() do.call(paste, c(self$frame, sep = "."))
  self$dotted = function() as.data.frame(setNames(list(self$labels()), self$name()))
  self$filter = function(..., .wrt, .comparison_function = all_equal) {
    labels = process_labels(...)
    f = self$select(.wrt)
    f$frame[f$labels() %in% labels, , drop = FALSE]
  }
  self$select = function(...) {
    cols_to_keep = process_partition_names(...)
    f = self$frame[cols_to_keep]
    l = do.call(paste, c(f, sep = "."))
    i = !duplicated(l) & (l != "")
    PartitionAlt(f[i, , drop = FALSE])
  }
  self$select_out = function(...) {
    cols_to_remove = process_partition_names(...)
    self$select(setdiff(self$names(), cols_to_remove))
  }
  return_object(self, "PartitionAlt")
}
print.PartitionAlt = function(x, ...) print(x$frame)
xx = PartitionAlt(r$read())
x = macpan2:::Partition(r$read())
identical(xx$names(), x$names())
identical(xx$name(), x$name())
identical(xx$labels(), x$labels())
identical(xx$dotted(), xx$dotted())
identical(xx$select("Vax", "Epi")$frame, x$select("Vax", "Epi")$frame())
identical(xx$select("Vax")$frame, x$select("Vax")$frame())
identical(xx$select_out("Epi")$frame, x$select_out("Epi")$frame())
xx
xx$select("Vax")
x$select("Vax")
