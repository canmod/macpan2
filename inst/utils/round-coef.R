round_coef_tab = function(x, digits = 4) {
  id_cols = c("term", "mat", "row", "col", "type")
  num_cols = setdiff(names(x), id_cols)
  for (col in num_cols) {
    x[[col]] = round(x[[col]], digits)
  }
  rownames(x) = NULL
  cols_to_drop = c("term", "col", "type")
  x = x[, setdiff(names(x), cols_to_drop), drop = FALSE]
  x
}
