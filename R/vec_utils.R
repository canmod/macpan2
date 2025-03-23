#' Zero Vector
#' 
#' Create a \code{\link{numeric}} vector of all zeros with names
#' given by \code{x}
#' 
#' @param x Object representing the names of the output vector. Most
#' commonly this will be a \code{\link{character}} vector.
#' @param ... Passed on to S3 methods.
#' 
#' @export
mp_zero_vector = function(x, ...) {
  UseMethod("mp_zero_vector")
}

#' @export
mp_zero_vector.character = function(x, ...) {
  (x
    |> as.vector()
    |> zero_vector()
  )
}

#' @export
mp_zero_vector.Index = function(x, labelling_column_names, ...) {
  (x
   |> mp_subset(...)
   |> mp_labels(labelling_column_names)
   |> zero_vector()
  )
}

zero_vector = function(labels) setNames(rep(0, length(labels)), labels)

rep_name = function(x, name) setNames(x, rep(name, length(x)))
