memoise_all = function(e) {
  for (m in names(e)) {
    if (is.function(e[[m]])) e[[m]] = memoise(e[[m]])
  }
}
