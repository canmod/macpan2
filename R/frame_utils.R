## frame utils from poorman to avoid hard dependency on dplyr

dotdotdot <- function(..., .impute_names = FALSE) {
  dots <- eval(substitute(alist(...)))
  if (isTRUE(.impute_names)) {
    deparse_dots <- lapply(dots, deparse)
    names_dots <- names(dots)
    unnamed <- if (is.null(names_dots)) rep(TRUE, length(dots)) else nchar(names_dots) == 0L
    names(dots)[unnamed] <- deparse_dots[unnamed]
  }
  dots
}

is_nested <- function(lst) vapply(lst, function(x) inherits(x[1L], "list"), FALSE)

have_name <- function(x) {
  nms <- names(x)
  if (is.null(nms)) rep(FALSE, length(x)) else !names_are_invalid(nms)
}

flatten <- function(lst) {
  nested <- is_nested(lst)
  res <- c(lst[!nested], unlist(lst[nested], recursive = FALSE))
  if (sum(nested)) Recall(res) else return(res)
}

check_filter <- function(conditions) {
  named <- have_name(conditions)
  for (i in which(named)) {
    if (!is.logical(conditions[[i]])) {
      stop(
        sprintf("Problem with `filter()` input `..%s`.\n", i),
        sprintf("Input `..%s` is named.\n", i),
        "This usually means that you've used `=` instead of `==`.\n",
        sprintf("Did you mean `%s == %s`?", names(conditions)[[i]], conditions[[i]])
      )
    }
  }
}

bind_rows <- function(..., .id = NULL) {
  lsts <- list(...)
  if (length(lsts) == 1L) if (length(lsts[[1L]]) == 0L) {
    x = lsts[[1L]]
    if (!is.null(.id)) x[[.id]] = character(0L)
    return(x)
  }
  lsts <- flatten(lsts)
  lsts <- Filter(Negate(is.null), lsts)
  
  if (!missing(.id)) {
    lsts <- lapply(seq_along(lsts), function(i) {
      nms <- names(lsts)
      id_col = if (is.null(nms)) {
        as.character(i)
      } else if (nrow(lsts[[i]]) == 0L) {
        character()
      } else {
        nms[i]
      }
      id_df <- data.frame(id = id_col, stringsAsFactors = FALSE)
      colnames(id_df) <- .id
      y = try(cbind(id_df, lsts[[i]]), silent = TRUE)
      if (inherits(y, "try-error")) return(NULL)
      y
    })
  }
  nms <- unique(unlist(lapply(lsts, names)))
  lsts <- lapply(
    lsts,
    function(x) {
      if (!is.data.frame(x)) x <- data.frame(as.list(x), stringsAsFactors = FALSE)
      if (nrow(x) > 0L) {
        for (i in nms[!nms %in% names(x)]) x[[i]] <- ""
      } else {
        x = empty_frame(nms)
      }
      x
    }
  )
  names(lsts) <- NULL
  do.call(rbind, lsts)
}


filter <- function(.data, ...) {
  conditions <- dotdotdot(...)
  if (length(conditions) == 0L) return(.data)
  check_filter(conditions)
  cond_class <- vapply(conditions, typeof, NA_character_)
  cond_class <- cond_class[!cond_class %in% c("language", "logical")]
  if (length(cond_class) > 0L) stop("Conditions must be logical vectors")
  eval_env = new.env()
  eval_env$env <- parent.frame()
  on.exit(rm(list = "env", envir = eval_env), add = TRUE)
  rows <- lapply(
    conditions,
    \(cond, frame) eval(cond, .data, frame),
    frame = eval_env$env
  )
  rows <- Reduce("&", rows)
  .data[rows & !is.na(rows), ]
}


## frame utils _not_ from poorman

reset_rownames = function(x) {
  rownames(x) = NULL
  x
}

frame_formatter = function(frame) {
  make_underline = function(n) paste0(rep("-", times = n), collapse = "")
  cnames = names(frame)
  underlines = lapply(lapply(cnames, nchar), make_underline)
  frame = rbind(
    setNames(as.data.frame(as.list(cnames)), cnames),
    setNames(as.data.frame(underlines), cnames),
    frame
  )
  l = lapply(frame, as.character)
  widths = lapply(lapply(l, nchar), max)
  fixed_width_list = mapply(format, l, width = widths, MoreArgs = list(justify = "left"), SIMPLIFY = FALSE, USE.NAMES = FALSE)
  paste0(do.call(paste, c(fixed_width_list, list(sep = "  "))), collapse = "\n")
}

add_row = function(frame, ...) {
  l = as.list(frame)
  updates = list(...)
  for (col_nm in names(l)) {
    col = l[[col_nm]]
    if (col_nm %in% names(updates)) up = updates[[col_nm]] else up = NA
    if (is.integer(col)) {
      col = c(as.integer(up), col)
    } else if (is.numeric(col)) {
      col = c(as.numeric(up), col)
    } else if (is.character(col)) {
      if (is.na(up)) up = ""
      col = c(up, col)
    } else { ## try our best
      col = c(up, col) 
    }
    l[[col_nm]] = col
  }
  as.data.frame(l)
}

frame_to_mat_list = function(x) {
  y = list()
  for (m in unique(x$matrix)) {
    z = filter(x, matrix == m)
    rnms = unique(z$row)
    cnms = unique(z$col)
    nr = length(rnms)
    nc = length(cnms)
    nv = length(z$value)
    fix_nms = function(nms) {
      if (any(grepl("^[0-9]", nms))) return(NULL)
      nms
    }
    if (nr * nc != nv) {
      if (nr == 1L) {
        nr = nv / nc
        rnms = rep(rnms, nr)
      }
      if (nc == 1L) {
        nc = nv / nr
        rnms = rep(cnms, nc)
      }
      if (nr * nc != nv) stop("cannot find a shape for this matrix")
    }
    if (nc == 1L) {
      if (nr == 1L) { ## scalar
        y[[m]] = z$value
      } else { ## vector
        y[[m]] = setNames(z$value, fix_nms(rnms))
      }
    } else { ## matrix
      y[[m]] = matrix(z$value
        , nr, nc
        , dimnames = list(fix_nms(rnms), fix_nms(cnms))
      )
    }
  }
  y
}
