#' Namer
#'
#' Name model variables and parse names into labelled partitions.
#'
#' @return An object of class \code{Namer} with two methods public,
#' \code{names} and \code{partitions}, which are inverses of each other.
#'
#' @export
Namer = function() {
  self = Base()
  self$.csv = function(x) paste0(as.character(x), collapse = ",")
  self$.dot = function(x) paste0(as.character(x), collapse = ".")
  self$.tager = MathExpressionFromFunc(function(label, partition) partition[label])
  self$.tag = function(labels, partitions) {
    mapply(
      self$.tager$symbolic$evaluate,
      labels,
      partitions,
      USE.NAMES = FALSE,
      SIMPLIFY = TRUE
    )
  }
  self$.mat = function(x) vapply(x, as.character, character(nrow(x)))
  self$.parts = function(x) sub("\\[[a-zA-Z0-9_]*\\]$", "", x)
  self$.labs = function(x, parts) {
    re = paste(
      "^(",
      paste0(parts, collapse = "|"),
      ")\\[([a-zA-Z0-9_]*)\\]$",
      sep = ""
    )
    sub(re, "\\2", x)
  }
  self$names = function(variables) {
    var_mat = self$.mat(variables)
    apply(var_mat, 1L, self$.dot)
  }
  self$names_inverse = function(names, partition_names) {
    read.table(text = names, sep = ".", col.names = partition_names)
  }
  self$string = function(variables) {
    var_mat = self$.mat(variables)
    tagged_mat = matrix(
      sweep(var_mat, 2L, names(variables), self$.tag),
      nrow = nrow(variables)
    )
    apply(tagged_mat, 1L, self$.csv)
  }
  self$string_inverse = function(strings) {
    #browser()
    vars = strsplit(strings, ",")
    partition_names = unique(lapply(vars, self$.parts))
    if (length(partition_names) != 1L) stop("Malformed variable names")
    partition_names = partition_names[[1L]]
    var_mat = t(vapply(vars
      , self$.labs
      , character(length(partition_names))
      , partition_names
    ))
    setNames(as.data.frame(var_mat), partition_names)
  }
  return_object(self, "Namer")
}

# xx = Namer()
# xx$string(m)
# xx$names(m)
#
# jj = setNames(as.list(xx$string(m)), xx$names(m))
# jj$`I-mild-unvax`
#
# identical(xx$names_inverse(xx$names(m), c("epi", "symp", "vax")), m)
#
# identical(xx$partitions(xx$names(m)), m)


# m = epi_vax = data.frame(
#   epi = c(rep(c("S", "E", "I", "R", "beta", "N"), 2), "alpha", "gamma", ""),
#   vax = c(rep(c("unvax", "vax"), each = 6), "", "", "dose_rate")
# )
# namer = Namer()
# var_nms = namer$names(m)
# namer$names_inverse(namer$names(m), c("epi", "vax"))
