process_enum = function(x) {
  RE = "(paren|binop|fwrap)[ ]*:[ ]*\\`?([^`]*)\\`?\\((.*)(\\,.*)*\\)"
  valid_ids = grepl(RE, x)
  if (!all(valid_ids)) {
    stop("Developer error: Malformed enum in misc/dev/dev.cpp.")
  }
  list(
    symb = sub(RE, "\\1", x),
    func = sub(RE, "\\2", x),
    args = lapply(strsplit(sub(RE, "\\3", x), ","), trimws)
  )
}
processed = process_enum(valid_func_sigs)
valid_funcs = setNames(as.list(processed$func), processed$func)
valid_func_args = setNames(processed$args, processed$func)
valid_symb_type = setNames(processed$symb, processed$func)
