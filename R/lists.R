#' Self Naming List
#'
#' @param ... Objects to put into the list
#'
#' @export
nlist = function(...) {
    L = list(...)
    if (valid$char_no_empty$is_true(names(L))) return(L)
    snm = vapply(substitute(list(...)), deparse, character(1))[-1]
    if (is.null(nm <- names(L))) {
        nm = snm
    }
    if (any(nonames <- nm == "")) {
        nm[nonames] <- snm[nonames]
    }
    setNames(L, nm)
}

melt_matrix_int = function(x) {
  dm = dim(x)
  if (is.null(dm)) {
    row = seq_along(x) - 1L
    col = 0
  } else {
    row = rep(seq_len(dm[1]), each = dm[2]) - 1
    col = rep(seq_len(dm[2]), times = dm[1]) - 1
  }
  data.frame(row = row, col = col, value = as.vector(x))
}

melt_matrix = function(x, zeros_are_blank = TRUE) {
  dn = dimnames(x)
  nms = names(x)
  dm = dim(x)
  if (is.null(dm)) {
    if (zeros_are_blank) {
      col = ""
    } else {
      col = "0"
    }
    if (is.null(nms)) {
      if ((length(x) == 1L) & zeros_are_blank) {
        row = ""
      } else {
        row = as.character(seq_along(x) - 1)
      }
    } else {
      row = names(x)
    }
  } else if (is.null(dn)) {
    if ((dm[1] == 1L) & zeros_are_blank) {
      row = ""
    } else {
      row = as.character(rep(seq_len(dm[1]), times = dm[2]) - 1)
    }
    if ((dm[2] == 1L) & zeros_are_blank) {
      col = ""
    } else {
      col = as.character(rep(seq_len(dm[2]), each = dm[1]) - 1)
    }
  } else { 
    rn = rownames(x)
    cn = colnames(x)
    if (is.null(rn)) {
      if ((dm[1] == 1L) & zeros_are_blank) {
        row = ""
      } else {
        row = as.character(rep(seq_len(dm[1]), times = dm[2]) - 1)
      }
    } else {
      row = rep(rownames(x), times = dm[2])
    }
    if (is.null(cn)) {
      if ((dm[2] == 1L) & zeros_are_blank) {
        col = ""
      } else {
        col = as.character(rep(seq_len(dm[2]), each = dm[1]) - 1)
      }
    } else {
      col = rep(colnames(x), each = dm[1])
    }
  }
  data.frame(row = row, col = col, value = as.vector(x))
}

melt_default_matrix_list = function(x, zeros_are_blank = TRUE, simplify_as_scalars = FALSE) {
  if (length(x) == 0L) return(empty_frame("matrix", "row", "col", "value"))
  f = (x
    |> lapply(melt_matrix, zeros_are_blank)
    |> bind_rows(.id = "matrix")
  )
  if (simplify_as_scalars) {
    rm_rs = all(f$row == "")
    rm_cs = all(f$col == "")
    if (rm_rs) f$row = NULL
    if (rm_cs) f$col = NULL
    if (rm_rs & rm_cs) {
      nms = colnames(f)
      mat_col = nms == "matrix"
      if (any(mat_col)) names(f)[mat_col] = "quantity"
    }
  }
  
  rownames(f) = NULL
  f
}

melt_list_of_char_vecs = function(x) {
  n = vapply(x, length, integer(1L))
  list(
      unlist(x, use.names = FALSE, recursive = TRUE) 
    , rep(names(x), times = n)
  )
}

clean_dimnames = function(dn) {
  if (!is.null(dn)) {
    if (identical(as.character(dn[[2L]]), "0")) dn[[2L]] = NULL
    if (identical(as.character(dn[[1L]]), "0")) dn[[1L]] = NULL
  }
  return(dn)
}
cast_default_matrix_list = function(x) {
  val_list = tapply(x$value, x$matrix, c, simplify = FALSE)
  row_list = tapply(x$row, x$matrix, c, simplify = FALSE) |> lapply(unique)
  col_list = tapply(x$col, x$matrix, c, simplify = FALSE) |> lapply(unique)
  dimnames = mapply(list, row_list, col_list, SIMPLIFY = FALSE) |> lapply(clean_dimnames)
  nrow = vapply(row_list, length, integer(1L))
  ncol = vapply(col_list, length, integer(1L))
  mapply(matrix, val_list, nrow, ncol, dimnames = dimnames, SIMPLIFY = FALSE, USE.NAMES = TRUE)
}
cast_default_matrix_list = memoise(cast_default_matrix_list)

empty_named_list = function() list() |> setNames(character(0L))

assert_named_list = function(l) {
  if (is.null(names(l))) {
    if (length(l) == 0L) {
      l = setNames(list(), character())
    } else {
      stop("Developer error: missing names")
    }
  }
  l
}

self_named_vector = function(...) c(...) |> setNames(c(...))

simplify_row_col_ids = function(data_frame) {
  ur = unique(as.character(data_frame$row))
  uc = unique(as.character(data_frame$col))
  if (identical(ur, "0")) data_frame$row = character(nrow(data_frame))
  if (identical(uc, "0")) data_frame$col = character(nrow(data_frame))
  return(data_frame)
}
