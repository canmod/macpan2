test_that("multiplication works", {
  f = macpan2:::MathExpressionFromStrings(
    "exp(x) + log(y) * exp(z) / log(x) ^ exp(y) - log(z)",
    c("x", "y", "z")
  )
  expect_equal(
    "((exp(X) + ((log(Y) * exp(Z)) / (log(X) ^ exp(Y)))) - log(Z))",
    f$symbolic$evaluate("X", "Y", "Z")
  )
  expect_equal(
    ((exp(2) + ((log(3) * exp(4)) / (log(2) ^ exp(3)))) - log(4)),
    f$numeric$evaluate(2, 3, 4)
  )
  g = macpan2:::MathExpressionFromFunc(function(x) x^2) ## exercise alternative constructor
  print(f) ## to exercise print method
})
