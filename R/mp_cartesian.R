#' Cartesian Product of Index Tables
#'
#' Produce a new index table by taking all possible pairwise combinations
#' of the input tables. This is useful for producing product models
#' that expand model components through stratification.
#'
#' @param ... Index tables (see \code{\link{mp_index}}).
#'
#' @examples
#' mp_cartesian(
#'   mp_index(Epi = c("S", "I")),
#'   mp_index(Age = c("young", "old"))
#' )
#'
#' si = mp_index(Epi = c("S", "I"))
#' age = mp_index(Age = c("young", "old"))
#' loc = mp_index(City = c("hamilton", "toronto"))
#' vax = mp_index(Vax = c("unvax", "vax"))
#' (si
#'   |> mp_cartesian(age)
#'   |> mp_cartesian(loc)
#'   |> mp_cartesian(vax)
#' )
#'
#' flow_rates = mp_index(Epi = c("infection", "recovery"))
#' mp_union(
#'   mp_cartesian(
#'     mp_subset(flow_rates, Epi = "infection"),
#'     age
#'   ),
#'   mp_subset(flow_rates, Epi = "recovery")
#' )
#'
#' @family indexes
#' @family products
#' @export
mp_cartesian = function(...) Reduce(mp_cartesian_binary, list(...))

mp_cartesian_binary = function(x, y) {
  shared_columns = intersect(names(x), names(y))
  if (length(shared_columns) != 0) {
    msg_break(
      msg_colon(
        msg(
          "Cannot take the Cartesian product of two indexes that",
          "share columns names. But the input indexes share the",
          "following columns"
        ),
        msg_indent(shared_columns)
      ),
      msg("Perhaps mp_join is more suitable?")
    ) |> stop()
  }
  labelling_column_names = union(x$labelling_column_names, y$labelling_column_names)
  f = join_partitions(x$partition$frame(), y$partition$frame())
  Index(f, labelling_column_names = labelling_column_names)
}

#' Self Cartesian Product
#'
#' @param x An index.
#' @param suffixes Length-2 character vector giving suffixes that
#' disambiguate the column names in the output.
#' @inheritParams cartesian
#' @family products
#' @export
mp_square = function(x, suffixes = c("A", "B")) {
  l1 = sprintf("%s%s", x$labelling_column_names, suffixes[1L])
  l2 = sprintf("%s%s", x$labelling_column_names, suffixes[2L])
  n1 = sprintf("%s%s", names(x), suffixes[1L])
  n2 = sprintf("%s%s", names(x), suffixes[2L])
  x = (x$partition$frame()
    |> setNames(n1)
    |> Index(labelling_column_names = l1)
  )
  y = (x$partition$frame()
    |> setNames(n2)
    |> Index(labelling_column_names = l2)
  )
  mp_cartesian(x, y)
}

#' Self Cartesian Product Excluding One Off-Diagonal Side
#'
#' @inheritParams mp_square
#' @param y_labelling_column_names TODO
#' @param exclude_diag Should 'diagonal' commponents be excluded from the output.
#' @param lower_tri Should the lower triangular components be include from the
#' output. If \code{FALSE} the result is upper triangular.
#'
#' @family products
#' @export
mp_triangle = function(x, y_labelling_column_names, exclude_diag = TRUE, lower_tri = FALSE) {
  f = x$partition$frame()
  g = setNames(f, y_labelling_column_names)
  pairings = triangle_indices(nrow(f), exclude_diag, lower_tri)
  f = cbind(
    f[pairings$i, , drop = FALSE],
    g[pairings$j, , drop = FALSE]
  )
  Index(f, labelling_column_names = names(f))
}

triangle_indices = function(n, exclude_diag = TRUE, lower_tri = FALSE) {
  if (exclude_diag) {
    k = 2:n
    i = sequence(k - 1)
    j = rep(k, k - 1)
  } else if (!exclude_diag) {
    k = seq_len(n)
    i = sequence(k)
    j = rep(k, k)
  }
  if (lower_tri) {
    ii = i
    i = j
    j = ii
  }
  nlist(i, j)
}


#' Symmetric Self Cartesian Product
#'
#' @inheritParams mp_triangle
#' @family products
#' @export
mp_symmetric = function(x, y_labelling_column_names, exclude_diag = TRUE) {
  f = x$partition$frame()
  g = setNames(f, y_labelling_column_names)
  n = nrow(f)
  k = seq_len(n)

  if (exclude_diag) {
    i = rep(    k , times = n - 1L)
    j = rep(rev(k), each  = n - 1L)
  } else {
    i = rep(k, times = n)
    j = rep(k, each  = n)
  }

  f = cbind(
    f[i, , drop = FALSE],
    g[j, , drop = FALSE]
  )
  Index(f, labelling_column_names = names(f))
}

#' Linear Chain Product
#'
#' TODO: what does this mean?
#'
#' @inheritParams mp_triangle
#' @family products
#' @export
mp_linear = function(x, y_labelling_column_names) {
  f = x$partition$frame()
  g = setNames(f, y_labelling_column_names)
  n = nrow(f)

  k = c(1L, rep(2L, n - 2L), 1L)
  i = rep(seq_len(n), k)
  j = sequence(k, c(2, seq_len(n - 2L), n - 1L), by = 2)

  f = cbind(
    f[i, , drop = FALSE],
    g[j, , drop = FALSE]
  )
  Index(f, labelling_column_names = names(f))
}


#' @export
Ops.Index = function(e1, e2 = NULL) {
  #unary = nargs() == 1L
  #FUN = get(.Generic, envir = parent.frame(), mode = "function")
  if (.Generic == "*") {
    if (inherits(e2, "character")) {
      e1 * do.call(mp_index, as.list(e2))
    } else {
      return(mp_cartesian(e1, e2))
    }
  } else if (.Generic == "^") {
    if (inherits(e2, "character")) {
      l = setNames(rep(list(e1), length(e2)), e2)
      nms = names(e1)
      updater = as.list(nms)
      for (i in seq_along(e2)) {
        suffix = e2[[i]]
        names(updater) = sprintf("%s%s", nms, suffix)
        l[[i]] = mp_rename_from_list(e1, updater)
      }
      return(Reduce(`*`, l))
    } else if (inherits(e2, "numeric")) {
      e2 = seq_len(as.integer(e2)) |> as.character()
      return(e1 ^ e2)
    } else {
      stop("Can only raise indexes to powers of character vectors or integers.")
    }
  } else {
    stop("Operation not implemented for indexes.")
  }
}

