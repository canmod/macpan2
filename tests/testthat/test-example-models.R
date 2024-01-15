test_that("example models are properly parsed", {
  r = ("starter_models"
    |> mp_tmb_library("sir", package = "macpan2")
    |> mp_simulator(time_steps = 80, outputs = "I"
      , default = list(beta = 0.5, gamma = 0.2, N = 100, I = 1)
    )
    |> mp_trajectory()
  )

  expected_head = structure(
    list(
        matrix = c("I", "I", "I", "I", "I", "I")
      , time = 1:6
      , row = c(0, 0, 0, 0, 0, 0)
      , col = c(0, 0, 0, 0, 0, 0)
      , value = c(
          1.295, 1.673819875, 2.15811605601715
        , 2.77369837437042, 3.55034660108989
        , 4.52082543972383
      )
    )
    , row.names = c(NA, 6L)
    , class = "data.frame"
  )

  expected_tail = structure(
    list(
        matrix = c("I", "I", "I", "I", "I", "I")
      , time = 75:80
      , row = c(0, 0, 0, 0, 0, 0)
      , col = c(0, 0, 0, 0, 0, 0)
      , value = c(
          0.00460845639937598, 0.00389989999247531
        , 0.00330028093582821, 0.00279285178324815
        , 0.00236343941648981, 0.00200004942841338
      )
    )
    , row.names = 75:80
    , class = "data.frame"
  )
  
  expect_equal(head(r, 6L), expected_head)
  expect_equal(tail(r, 6L), expected_tail)
})
