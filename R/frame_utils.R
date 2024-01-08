## from poorman

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
      id_df <- data.frame(id = if (is.null(nms)) as.character(i) else nms[i], stringsAsFactors = FALSE)
      colnames(id_df) <- .id
      cbind(id_df, lsts[[i]])
    })
  }

  nms <- unique(unlist(lapply(lsts, names)))
  lsts <- lapply(
    lsts,
    function(x) {
      if (!is.data.frame(x)) x <- data.frame(as.list(x), stringsAsFactors = FALSE)
      for (i in nms[!nms %in% names(x)]) x[[i]] <- ""
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


reset_rownames = function(x) {
  rownames(x) = NULL
  x
}
