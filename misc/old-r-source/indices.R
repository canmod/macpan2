row_labels = function(x) {
  UseMethod("row_labels")
}
col_labels = function(x) {
  UseMethod("col_labels")
}

#' @export
row_labels.character = function(x) valid$char$assert(x)

#' @export
col_labels.character = function(x) valid$char$assert(x)


character_dots = function(...) {
  as.character(unlist(list(...), use.names = FALSE))
}

indices = function(...) {
  character_dots(...)
}
