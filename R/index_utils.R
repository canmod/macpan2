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

## permutations

apply_permutations = function(x) {
  matrix(x[permutations(length(x))], ncol = length(x))
}
apply_k_int_permutations = function(n, k) {
  (n
    |> increasing_int_seq(k)
    |> lapply(apply_permutations)
    |> do.call(what = rbind)
  )
}
apply_k_incr_int_permutations = function(n, k) {
  do.call(rbind, increasing_int_seq(n, k))
}

# return a list with all strictly increasing length-m
# sequences of integers between 1 and n
increasing_int_seq = function(n, m, x = list(1:m)) {
  if (n == m) {
    return(list(1:n))
  } else if (m > n) {
    stop("cannot return such a sequence")
  }
  l = length(x)
  for (i in 0:(m - 1L)) {
    if (x[[l]][m - i] != (n - i)) {
      x[[l + 1]] = x[[l]]
      place = x[[l]][m - i] + 1L
      place_indices = (m - i):m
      place_length = i + 1L
      x[[l + 1]][place_indices] = seq(from = place, length = place_length)
      break
    }
  }
  final_sequence = (n - m + 1L):n
  last_sequence = x[[l + 1L]]
  if (identical(last_sequence, final_sequence)) {
    return(x) ## done
  } else {
    increasing_int_seq(n, m, x) ## recursion to find more sequences
  }
}
increasing_int_seq = memoise(increasing_int_seq)

## https://stackoverflow.com/questions/11095992/generating-all-distinct-permutations-of-a-list-in-r
permutations <- function(n) {
  if (n == 1) {
    return(matrix(1))
  } else {
    sp <- permutations(n - 1)
    p <- nrow(sp)
    A <- matrix(nrow = n * p, ncol = n)
    for (i in 1:n) {
        A[(i - 1) * p + 1:p,] <- cbind(i,sp + (sp >= i))
    }
    return(A)
  }
}
permutations = memoise(permutations)
