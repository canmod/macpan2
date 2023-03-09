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
