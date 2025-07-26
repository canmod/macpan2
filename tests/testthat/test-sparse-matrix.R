test_that("sparse_mat_mult works properly", {
  make_args = function(dn, dm, tol) {
    set.seed(1)
    n = 10
    m = 5
    p = 3
    A = matrix(rnorm(n * (m + dm)) / 10, n, m + dm)
    B = matrix(rnorm(m * p) / 10, m, p)
    a = macpan2:::sparse_matrix_notation(A, tol = tol)
    traj = simple_sims(list(Y ~ sparse_mat_mult(m, i, j, B, Y))
        , time_steps = 1
        , int_vecs = list(
            i = a$row_index
          , j = a$col_index
          
        )
        , mats = list(
            m = a$values
          , B = B
          , Y = matrix(0, n + dn, p)
        )
    )
    Y_mp = traj |> filter(matrix == "Y") |> with(matrix(value, max(row) + 1))
    Y_base_r = a$Msparse %*% B
    nlist(Y_mp, Y_base_r)
  }
  
  expect_error(
      make_args(0, 1, 0.1)
    , regexp = "Number of rows in the second"
  )
  expect_error(
      make_args(-2, 0, 0.1)
    , regexp = "Number of rows in the output matrix"
  )
  
  args = make_args(-1, 0, 0.1)
  expect_equal(args$Y_mp, args$Y_base_r[1:9, ])
  expect_equal(dim(args$Y_mp), c(9, 3))
  
  args = make_args(0, 0, 0.1)
  expect_equal(args$Y_mp, args$Y_base_r)
  
  args = make_args(0, 0, 0)
  expect_equal(args$Y_mp, args$Y_base_r)
  
  expect_error(
      engine_eval(~sparse_mat_mult(matrix(0, 2, 2), 0, 0, 0, 0))
    , "First matrix must be represented as a vector with associated row and column indices"
  )
  expect_error(
      engine_eval(~sparse_mat_mult(0, 0, 0))
    , "Require exactly 5 arguments, but got fewer"
  )
  expect_error(
      engine_eval(~sparse_mat_mult(0, 0, 0, 0, 0, 0))
    , "Require exactly 5 arguments, but got more"
  )
})
