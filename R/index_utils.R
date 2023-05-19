## split a numeric or character vector, x, into two components:
## (1) $indices : integer vector the same length as x containing
##     integers (as strings or numbers) in x, and NAs in the positions
##     that do not contain integers
## (2) $labels : character vector the same length as x containing
##     non-integers in x, and NAs in the positions that contain
##     integers
split_index_label_mix = function(x) {
  if (is.numeric(x)) {
    l = list(
      indices = as.integer(x),
      labels = rep(NA_character_, length(x))
    )
    return(l)
  }
  x = as.character(x)
  index_locations = grepl("^[0-9]+$", x)
  label_locations = !index_locations
  indices = integer(length(x))
  labels = character(length(x))
  indices[] = NA
  labels[] = NA
  indices[index_locations] = as.integer(x[index_locations])
  labels[label_locations] = x[label_locations]
  nlist(indices, labels)
}

## get row and column indices from a vector that (possibly)
## mixes integer and character representations of ids
##
## take three vectors the same length:
## (1) mat -- names of matrices
## (2) row -- character/numeric with row names or indices
## (3) col -- character/numeric with col names or indices
## also take .dimnames which is a named list of dimnames
## for a set of matrices
##
## return a list with row_id and col_id integer vectors,
## each the same length as the three input vectors
make_row_col_ids = function(mat, row, col, .dimnames) {
  if (is.numeric(row) & is.numeric(col)) {
    l = list(row_id = as.integer(row), col_id = as.integer(col))
    return(l)
  }
  row = split_index_label_mix(row)
  col = split_index_label_mix(col)
  missing_row_indices = is.na(row$indices)
  missing_col_indices = is.na(col$indices)
  for (m in names(.dimnames)) {
    mi = mat == m
    i = mi & missing_row_indices
    j = mi & missing_col_indices
    row_vec = length(.dimnames[[m]][[1L]]) == 1L
    col_vec = length(.dimnames[[m]][[2L]]) == 1L
    if (any(i)) {
      if (row_vec) {
        row$indices[i] = 0L
      } else {
        row$indices[i] = match(row$labels[i], .dimnames[[m]][[1L]]) - 1L
      }
    }
    if (any(j)) {
      if (col_vec) {
        col$indices[i] = 0L
      } else {
        col$indices[i] = match(col$labels[i], .dimnames[[m]][[1L]]) - 1L
      }
    }
  }
  list(row_id = row$indices, col_id = col$indices)
}

## get 1-based expression indices from a vector that (possibly)
## mixes integer and character representations of ids
make_expr_id = function(expr_id, expr_names) {
  if (is.numeric(expr_id)) return(as.integer(expr_id))
  expr_id = split_index_label_mix(expr_id)
  missing_id = is.na(expr_id$indices)
  expr_id$indices[missing_id] = match(expr_id$labels[missing_id], expr_names)
  return(expr_id$indices)
}
