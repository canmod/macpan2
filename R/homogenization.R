# definitions of R concepts that allow us to treat different kinds of
# objects more homogeneously

dim_def = function(x) {
  if (is.null(dim(x))) return(c(length(x), 1L))
  d = dim(x)
  if (length(d) != 2L) stop('more than two dimensions are not allowed')
  d
}
dimnames_def = function(x) {
  nms = names(x)
  dnms = dimnames(x)
  if (!is.null(nms)) return(list(nms, character()))
  if (!is.null(dnms)) return(dnms)
  if (is_vec(x) & (!is_mat(x))) {
    return(list(make.names(x, unique = TRUE), character()))
  }
  stop('object cannot have dimnames')
}
names_def = function(x) {
  rnms = dimnames_def(x)[[1]]
  cnms = dimnames_def(x)[[2]]
  paste(
    rep(rnms, times = length(cnms)),
    rep(cnms, each = length(rnms)),
    sep = "_"
  )
}

dim_len = function(x) {
  length(dim_def(x))
}
dim_1 = function(x) {
  dim_def(x)[1]
}
dim_2 = function(x) {
  dim_def(x)[2]
}
are_names_unique = function(x) {
  nms = names_def(x)
  (!is.null(nms)) & (!any(duplicated(nms)))
}

is_size = function(object, kind, size) {
  object_kind = structure(
    list(
      orig = object,
      kind = kind,
      size = size
    ),
    class = paste0(c("kind", kind), collapse = "_")
  )
  macpan_is(object_kind)
}

macpan_is_size = function(object) {
  UseMethod("macpan_is")
}

macpan_is_size.default = function(object) {
  #if (!is_size(as.integer(object$size), 'integer', 1L)) return(FALSE)
  with(object, {is(orig, kind) & (length(orig) == size)})
}

macpan_is_size.kind_matrix = function(object) {
  with(object, {is(orig, kind) & all(dim(orig) == size)})
}

is_str_const_len = function(x) {
  is.character(x) & (length(unique(nchar(x))) == 1L)
}

is_str_bin = function(x) {
  is_str_const_len(x) & all(grepl("^[0-1]+$", x))
}

