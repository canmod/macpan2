#' Lookup
#'
#' Lookup a subset or factor index associated with a symbol, and return the
#' index associated with that symbol.
#'
#' @param index Index table (see \code{\link{mp_index}}).
#' @param symbol Character string that could possibly be associated with a
#' subset or factor of `index`.
#' @param fail_result Should failure to finding an index result in an error?
#' The default is `TRUE`, but if not `TRUE` then `NULL` will be returned
#' when no indexes can be found.
#' @param index_columns_valid_symbols Are the columns of indexes valid
#' symbols to look for?
#'
#' @export
mp_lookup = function(index, symbol
    , fail_with_error = TRUE
    , index_columns_valid_symbols = TRUE
  ) {

  ## check if we are referring to possibly multiple grouping factors
  dim_names = to_names(symbol)
  if (index_columns_valid_symbols) {
    if (all(dim_names %in% names(index))) return(mp_group(index, symbol))
  }
  all_dim_names = names(index)
  if (length(all_dim_names) < length(dim_names)) {
    if (fail_with_error) stop("cannot find such a symbol")
    return(NULL)
  }
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
  if (fail_with_error) stop("failed to find symbol")
  return(NULL)
}
#mp_lookup = memoise(mp_lookup)

#' @export
mp_lookup_in_list = function(
        index_list
      , symbol_vector
      , fail_with_error = TRUE
      , index_columns_valid_symbols = FALSE
  ) {
  indices = list()
  vectors = list()
  for (snm in symbol_vector) {
    for (vnm in names(index_list)) {
      if (snm == vnm) {
        subset_index = index_list[[vnm]]
        indices[[snm]] = subset_index
        vectors[[snm]] = vnm
        break
      }
      subset_index = mp_lookup(index_list[[vnm]]
        , snm
        , fail_with_error = FALSE
        , index_columns_valid_symbols = index_columns_valid_symbols
      )
      if (!is.null(subset_index)) {
        indices[[snm]] = subset_index
        vectors[[snm]] = vnm
        break
      }
    }
    if (is.null(subset_index) & fail_with_error) {
      stop("failed to find symbol")
    }
  }
  structure(indices, vectors = unlist(vectors, use.names = TRUE))
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
