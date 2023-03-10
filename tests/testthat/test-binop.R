test_that("elementwise binary operator executable specs match spec doc", {
  ## https://canmod.net/misc/elementwise_binary_operators
  times = BinaryOperator(`*`)
  pow = BinaryOperator(`^`)
  A = matrix(1:6, 3, 2)
  a = matrix(1)
  x = matrix(1:2, 1)
  y = matrix(1:3)
  z = matrix(1:4)
  v = 1:2
  expect_equal(times(A, A)
    ,structure(c(1L, 4L, 9L, 16L, 25L, 36L), dim = 3:2)
  )
  expect_equal(times(A, a)
    ,structure(c(1, 2, 3, 4, 5, 6), dim = 3:2)
  )
  expect_equal(times(A, x)
    ,structure(c(1L, 2L, 3L, 8L, 10L, 12L), dim = 3:2)
  )
  expect_equal(times(A, y)
    ,structure(c(1L, 4L, 9L, 4L, 10L, 18L), dim = 3:2)
  )
  expect_equal(times(A, a), times(a, A))
  expect_equal(times(A, x), times(x, A))
  expect_equal(times(A, y), times(y, A))
  expect_equal(pow(A, A)
    ,structure(c(1, 4, 27, 256, 3125, 46656), dim = 3:2)
  )
  expect_equal(pow(x, A)
    ,structure(c(1, 1, 1, 16, 32, 64), dim = 3:2)
  )
  expect_error(times(A, z))
  expect_error(pow(z, y))

  expect_equal(
    times(A, A),
    engine_eval(~A * A, A = A)
  )

  expect_equal(
    times(A, a),
    engine_eval(~A * a, A = A, a = a)
  )

  expect_equal(
    times(A, x),
    engine_eval(~A * x, A = A, x = x)
  )

  expect_equal(
    times(x, A),
    engine_eval(~x * A, A = A, x = x)
  )

  expect_equal(
    times(A, y),
    engine_eval(~A * y, A = A, y = y)
  )

  expect_equal(
    pow(A, A),
    engine_eval(~A^A, A = A)
  )

  expect_equal(
    A - 1,
    engine_eval(~A - 1, A = A)
  )
})
