#' Lookup
#'
#' Lookup a subset or factor index associated with a symbol, and return the
#' index associated with that symbol.
#'
#' @param index Index table (see \code{\link{mp_index}}).
#' @param symbol Character string that could possibly be associated with a
#' subset or factor of `index`.
#'
#' @export
mp_lookup = function(index, symbol) {

  ## check if we are referring to possibly multiple grouping factors
  dim_names = to_names(symbol)
  if (all(dim_names %in% names(index))) {
    return(mp_group(index, symbol))
  }
  all_dim_names = names(index)
  ii = increasing_int_seq(length(all_dim_names), length(dim_names))
  for (i in ii) {
    named_symbol = (symbol
      |> list()
      |> setNames(to_name(all_dim_names[i]))
    )
    args = c(list(index), named_symbol)
    guess = try(do.call(mp_subset, args), silent = TRUE)
    if (!inherits(guess, "try-error")) return(guess)
  }
  for (i in ii) {
    all_perms = apply_permutations(i)
    for (j in seq_len(nrow(all_perms))) {
      named_symbol = (symbol
        |> list()
        |> setNames(to_name(all_dim_names[all_perms[j,]]))
      )
      args = c(list(index), named_symbol)
      guess = try(do.call(mp_subset, args), silent = TRUE)
      if (!inherits(guess, "try-error")) return(guess)
    }
  }
  stop("failed to find symbol")
}


# @param l result of mp_slices or mp_factor or just a named list of indices
mp_unpack = function(l, unpack = c('no', 'maybe', 'yes'), env) {
  unpack = match.arg(unpack)
  if (unpack %in% c('maybe', 'yes')) {
    for (nm in names(l)) {
      already_there = exists(nm, envir = env)
      if (already_there) {
        if (unpack == 'maybe') {
          stop('cannot unpack because slice names already exist')
        } else if (unpack == 'yes') {
          warning('masking or overwriting existing objects with slices')
        }
      }
    }
    for (nm in names(l)) assign(nm, l[[nm]], envir = env)
  }
}

#' Factor an Index
#' 
#' @param index An index to be factored.
#' @param unpack Place factors in the global environment?
#' 
#' @export
mp_factors = function(index, unpack = c('no', 'maybe', 'yes')) {
  unpack = match.arg(unpack)
  factors = list()
  for (d in names(index)) factors[[d]] = mp_factor(index, d)
  pf = parent.frame()
  force(pf)
  mp_unpack(factors, unpack, pf)
  factors
}

mp_subset_list = function(index, ..., unpack = c('no', 'maybe', 'yes')) {
  unpack = match.arg(unpack)
  subsets = list(...)

  for (s in names(subsets)) {
    args = c(list(index), subsets[[s]])
    subsets[[s]] = do.call(mp_subset, args)
  }

  pf = parent.frame()
  force(pf)
  mp_unpack(subsets, unpack, pf)
  subsets
}

mp_custom_slices = function(index, ..., unpack = c('no', 'maybe', 'yes')) {
  unpack = match.arg(unpack)
  slice_refs = list(...)
  nms = names(slice_refs)
  for (i in seq_along(slice_refs)) {
    r = slice_refs[[i]]
    if (is.null(nms[i]) | nchar(nms[i]) == 0L) {
      slice_refs[[i]] = (index
        |> mp_group(r)
        |> as.data.frame()
        |> unlist(recursive = FALSE, use.names = FALSE)
      )
      names(slice_refs)[[i]] = r
    }
  }
  slices = list()
  for (d in names(slice_refs)) {
    for (s in slice_refs[[d]]) {
      if (s %in% names(slices)) {
        warning("duplicated slice")
      } else {
        args = c(list(index), setNames(list(s), d))
        slices[[s]] = do.call(mp_subset, args)
      }
    }
  }
  pf = parent.frame()
  force(pf)
  mp_unpack(slices, unpack, pf)
  slices
}

#' Slice an index 
#' 
#' @param index Index to slice up.
#'
#' @inheritParams mp_factors
#' @export
mp_slices = function(index, unpack = c('no', 'maybe', 'yes')) {
  unpack = match.arg(unpack)
  possible_slices = (index
    |> as.data.frame()
    |> lapply(unique)
    |> lapply(setdiff, "")
  )
  slices = list()
  for (d in names(possible_slices)) {
    for (s in possible_slices[[d]]) {
      if (s %in% names(slices)) {
        warning("duplicated slice")
      } else {
        args = c(list(index), setNames(list(s), d))
        slices[[s]] = do.call(mp_subset, args)
      }
    }
  }
  pf = parent.frame()
  force(pf)
  mp_unpack(slices, unpack, pf)
  slices
}
