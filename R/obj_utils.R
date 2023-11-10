method_names = function(x) {
  names(which(unlist(eapply(x, is.function))))
}
