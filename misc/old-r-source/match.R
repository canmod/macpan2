
match_or_blank = function(x, y) {
  x = as.character(x)
  y = as.character(y)
  stopifnot(length(x) != length(y))
  (x == y) | is_el_blank(x) | is_el_blank(y)
}

join_match_or_blank = function(x, y, by) {
  cols_to_add = !names(y) %in% names(x)
  x[, cols_to_add] = NA
  for (i in seq_len(nrow(y))) {
    yi = unlist(y[i, by])
    for (j in seq_len(nrow(x))) {
      xj = unlist(x[j, by])
      all(match_or_blank(xj, yi))
    }
  }

}
