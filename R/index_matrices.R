#' Index Matrices
#'
#' Get the indices of a binary matrix that have a value of 1,
#' to simplify matrix multiplication.
#' 
#' Given a column vector \code{x}, and binary Matrix \code{M}. We can 
#' perform matrix multiplication by writing,
#' \code{M %*% x}
#' To simplify this computation, we can extract the indices of \code{M}
#' that have a value of 1, reducing the dimension of the problem.
#' These indices can then be used to identify entries in \code{x} that 
#' should be grouped and summed over to compute \eqn{Mx}.
#' 
#'
#' @param M matrix containing numbers that are in \{0,1\} or a matrix
#' with many values close to zero.
#' 
#' @return named list of two integer vectors. 
#' * \code{col_index} vector of column indices of M that have a value of 1,
#' ordered by row. Indices start at 0, for C++ implementation.
#' * \code{row_index} vector of row indices of M that have a value of 1,
#' ordered by row. Indices start at 0, for C++ implementation.
#' 
#' This function was initially created to return integer vectors that can be
#' used as inputs to `group_sums(x[col_index],row_index,n)`
#' @author Jen Freeman
#' @noRd
binary_matrix_notation <- function(M){
  col_index = c(t(col(M) * M))
  col_index = as.integer(col_index[col_index != 0] - 1) 
  
  row_index = as.integer(rep(1:nrow(M), times = rowSums(M)) - 1)
  
  return(nlist(col_index, row_index))
}



sparse_matrix_notation = function(M, zero_based = TRUE, tol = 1e-4) {
  non_zero_loc = M > tol
  col_index = c(t(col(M) * non_zero_loc))
  col_index = as.integer(col_index[col_index != 0])
  row_index = as.integer(rep(1:nrow(M), times = rowSums(non_zero_loc)))
  values = M[cbind(row_index, col_index)]
  if (zero_based) {
    row_index = row_index - 1L
    col_index = col_index - 1L
  }
  Msparse = M
  Msparse[!non_zero_loc] = 0
  return(nlist(row_index, col_index, values, M, Msparse))
}

sparse_rbf_notation = function(time_steps, dimension, zero_based = TRUE, tol = 1e-2) {
  rbf(time_steps, dimension) |> sparse_matrix_notation(zero_based, tol)
}
# 
# bb = 80
# x = sparse_rbf_notation(100, bb, FALSE)
# b = rnorm(bb)
# 
# times = x$M %*% b
# times_approx = tapply(
#     x$values * b[x$col_index]
#   , x$row_index
#   , sum
# )
# plot(times, times_approx)
# length(x$values) / prod(dim(x$M))
# matplot(x$M, type = "l")
# matplot(x$Msparse, type = "l")
# plot(x$M %*% b, type = "n")
# lines(x$M %*% b)
# lines(x$Msparse %*% b, col = "red")
