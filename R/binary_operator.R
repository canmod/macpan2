binop_validity_message = paste(
  "Valid binary operations have the following characteristics.",
  "   They are functions.",
  "   They have exactly two arguments.",
  "   They do not have any ... arguments",
  sep = "\n"
)

valid_binop = oor::ValidityMessager(
  All(
    is.function,
    TestPipeline(
      Summarizer(args, formals, length),
      TestRange(2L, 2L)
    ),
    TestPipeline(
      Summarizer(args, formals, names),
      Not(MappedAnyTest(TestSubset("...")))
    )
  ),
  binop_validity_message
)

#' Binary Operator
#'
#' Convert a function that represents an elementwise binary
#' operator into one that is consistent with the \code{C++}
#' engine.
#'
#' @param operator A binary operator. r binop_validity_message
#' @return A binary operator consistent with the \code{C++} engine.
#' @examples
#' set.seed(1L)
#' A = matrix(abs(rnorm(6)), 3, 2)  # 3 by 2 matrix
#' x = matrix(abs(rnorm(3)))        # 3 by 1 matrix
#' y = t(abs(rnorm(2)))             # 1 by 2 matrix
#' times = BinaryOperator(`*`)
#' pow = BinaryOperator(`^`)
#' identical(times(A, x), times(x, A))  ## TRUE
#' identical(pow(A, x), pow(x, A))  ## FALSE
#'
#' @export
BinaryOperator = function(operator) {
  op = valid_binop$assert(operator)
  function(x, y) {
    x = valid$num_mat$assert(x)
    y = valid$num_mat$assert(y)
    ## simple case of operands
    ## with the same shape
    eq = dim(x) == dim(y)
    scal_x = all(dim(x) == 1L)
    scal_y = all(dim(y) == 1L)
    if (scal_x) dim(x) = NULL
    if (scal_y) dim(y) = NULL
    if (all(eq) | scal_x | scal_y) return(op(x, y))

    ## for non-commutative
    ## operations we swap the order
    ## of the arguments so that we
    ## can use the sweep function below
    vec_x = any(dim(x) == 1L)
    op1 = op
    if (vec_x) op1 = function(x, y) op(y, x)

    ## make sweep (used below) fail hard,
    ## because a warning is issued for
    ## dimensional mismatch by default
    saved_options = options(warn = 2L)
    on.exit(options(saved_options))

    ## to use the sweep function the
    ## matrix needs to come first
    if (any(eq) & vec_x) return(sweep(y, which(eq), x, op1))
    if (any(eq))         return(sweep(x, which(eq), y, op1))
    stop("something went dredfully wrong")
  }
}
