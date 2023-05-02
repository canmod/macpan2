test_that("all flow types can be used", {
  state = setNames(rep(2, 7), LETTERS[1:7])
  flow = setNames(rep(0.1, 6), letters[1:6])
  m = Compartmental(system.file("testing_models", "all_flow_types", package = "macpan2"))
  s = m$simulators$tmb(
    time_steps = 3,
    state = state, flow = flow
  )
  r = s$report()

  expect_equal(
    c(
      2 -0.1 * 2,
      2 + 0.1 * 2 - 0.1,
      2 + 0.1,
      2 + 0.1 * 2 - 0.1 * 2,
      2,
      2 + 0.1 - 0.1,
      2
    ),
    r[r$time == 1, ]$value
  )

# expect_equal(
#     s$report(),
#     structure(list(matrix = c("state", "state", "state", "state",
#     "state", "state", "state", "state", "state", "state", "state",
#     "state", "state", "state", "state", "state", "state", "state",
#     "state", "state", "state", "state", "state", "state", "state",
#     "state", "state", "state", "state", "state", "state", "state",
#     "state", "state", "state"), time = c(0L, 0L, 0L, 0L, 0L, 0L,
#     0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L,
#     3L, 3L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 4L, 4L, 4L), row = c("A",
#     "B", "C", "D", "E", "F", "G", "A", "B", "C", "D", "E", "F", "G",
#     "A", "B", "C", "D", "E", "F", "G", "A", "B", "C", "D", "E", "F",
#     "G", "A", "B", "C", "D", "E", "F", "G"), col = c("", "", "",
#     "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
#     "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", ""
#     ), value = c(2, 2, 2, 2, 2, 2, 2, 1.8, 2.1, 2.1, 1.8, 2, 1.9,
#     2, 1.62, 2.18, 2.2, 1.62, 2, 1.8, 2, 1.458, 2.242, 2.3, 1.458,
#     2, 1.7, 2, 1.458, 2.242, 2.3, 1.458, 2, 1.7, 2)), row.names = c(NA,
#     -35L), class = "data.frame")
#   )
})
