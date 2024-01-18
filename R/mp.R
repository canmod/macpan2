mp = function(mp_func) {
  f = ("mp_%s"
    |> sprintf(mp_func)
    |> getFromNamespace("macpan2")
  )
  prototype = function(...) {l = list(...)}
  target_e = body(f)[[2L]]
  proto_e = body(prototype)[[2L]]
  if (!identical(target_e, proto_e)) stop("developer error: invalid mp function")
  body(f)[[2L]][[3L]] = (~unlist(list(...), recursive = FALSE))[[2L]]
  f
}


# FIXME: What's this??
mp_indicator = function(x, ...) {
  l = list(...)
  for (nm in names(l)) {
    l[[nm]] = x$partition$partial_labels(nm) %in% l[[nm]]
  }
  Reduce(`&`, l)
}
