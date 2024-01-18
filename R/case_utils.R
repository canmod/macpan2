var_case_to_cls_case = function(...) {
  x = unlist(list(...), recursive = TRUE, use.names = FALSE)
  y = strsplit(x, "_")
  first_letters = (y
    |> lapply(substring, 1L, 1L)
    |> lapply(toupper)
  )
  remaining_letters = (y
    |> lapply(substring, 2L)
  )
  mapply(paste
    , first_letters
    , remaining_letters
    , MoreArgs = list(sep = "")
    , SIMPLIFY = FALSE
  ) |> vapply(paste, character(1L), collapse = "")
}

cls_case_to_var_case = function(...) {
  x = unlist(list(...), recursive = TRUE, use.names = FALSE)
  y = strsplit(x, "(?<=[a-z0-9])(?=[A-Z])", perl = TRUE)
  words = unlist(lapply(y, function(z) {
    tolower(z)
  }))
  paste(words, collapse = "_")
}
