test_that("example models are properly parsed", {
  f = system.file("starter_models", "sir_vax", package = "macpan2")
  m = Compartmental(f)
  s = m$simulators$tmb(
    time_steps = 80,
    state = c(
      S.unvax = 99,
      I.unvax = 1,
      R.unvax = 0,
      S.vax = 0,
      I.vax = 0,
      R.vax = 0
    ),
    flow = c(
      infection.unvax = 0,
      infection.vax = 0,
      gamma.unvax = 0.1,
      gamma.vax = 0.3,
      .vax_rate = 0.1
    ),
    beta.unvax = 0.2,
    beta.vax = 0.01,
    sigma.unvax = 1,
    sigma.vax = 0.2,
    N.unvax = empty_matrix,
    N.vax = empty_matrix,
    foi.unvax = empty_matrix,
    foi.vax = empty_matrix,
    foi. = empty_matrix
  )

  expected_head = structure(list(matrix = c("state", "state", "state", "state",
  "state", "state", "state", "state", "state", "state", "state",
  "state", "state", "state", "state", "state", "state", "state",
  "state", "state", "state", "state", "state", "state", "state",
  "state", "state", "state", "state", "state", "state", "state",
  "state", "state", "state", "state", "state", "state", "state",
  "state"), time = c(0L, 0L, 0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L, 1L,
  1L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 4L, 4L, 4L,
  4L, 4L, 4L, 5L, 5L, 5L, 5L, 5L, 5L, 6L, 6L, 6L, 6L), row = c("S.unvax",
  "I.unvax", "R.unvax", "S.vax", "I.vax", "R.vax", "S.unvax", "I.unvax",
  "R.unvax", "S.vax", "I.vax", "R.vax", "S.unvax", "I.unvax", "R.unvax",
  "S.vax", "I.vax", "R.vax", "S.unvax", "I.unvax", "R.unvax", "S.vax",
  "I.vax", "R.vax", "S.unvax", "I.unvax", "R.unvax", "S.vax", "I.vax",
  "R.vax", "S.unvax", "I.unvax", "R.unvax", "S.vax", "I.vax", "R.vax",
  "S.unvax", "I.unvax", "R.unvax", "S.vax"), col = c("", "", "",
  "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
  "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
  "", "", "", "", ""), value = c(99, 1, 0, 0, 0, 0, 88.902, 1.098,
  0.1, 9.9, 0, 0, 79.7951198756937, 1.20488012430633, 0.2098, 18.7853741620422,
  0.00482583795782464, 0, 71.5786247459434, 1.32137525405659, 0.330288012430633,
  26.7537280311629, 0.0145362050190736, 0.00144775138734739, 64.1620591000826,
  1.44794089991735, 0.462425537836292, 33.8922515639728, 0.0295142852977935,
  0.00580861289306946, 57.464079777476, 1.58492022252397, 0.607219627828027,
  40.2786893040758, 0.0504281696137448, 0.0146628984824075, 51.411617437995,
  1.73248256200496, 0.765711650080424, 45.9821923276533)), row.names = c(NA,
  40L), class = "data.frame")

  expected_tail = structure(list(matrix = c("state", "state", "state", "state",
  "state", "state", "state", "state", "state", "state", "state",
  "state", "state", "state", "state", "state", "state", "state",
  "state", "state", "state", "state", "state", "state", "state",
  "state", "state", "state", "state", "state", "state", "state",
  "state", "state", "state", "state", "state", "state", "state",
  "state"), time = c(75L, 75L, 75L, 75L, 76L, 76L, 76L, 76L, 76L,
  76L, 77L, 77L, 77L, 77L, 77L, 77L, 78L, 78L, 78L, 78L, 78L, 78L,
  79L, 79L, 79L, 79L, 79L, 79L, 80L, 80L, 80L, 80L, 80L, 80L, 81L,
  81L, 81L, 81L, 81L, 81L), row = c("R.unvax", "S.vax", "I.vax",
  "R.vax", "S.unvax", "I.unvax", "R.unvax", "S.vax", "I.vax", "R.vax",
  "S.unvax", "I.unvax", "R.unvax", "S.vax", "I.vax", "R.vax", "S.unvax",
  "I.unvax", "R.unvax", "S.vax", "I.vax", "R.vax", "S.unvax", "I.unvax",
  "R.unvax", "S.vax", "I.vax", "R.vax", "S.unvax", "I.unvax", "R.unvax",
  "S.vax", "I.vax", "R.vax", "S.unvax", "I.unvax", "R.unvax", "S.vax",
  "I.vax", "R.vax"), col = c("", "", "", "", "", "", "", "", "",
  "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
  "", "", "", "", "", "", "", "", "", "", "", "", "", "", ""),
      value = c(11.11904089301, 69.5471580869164, 0.0360129884910043,
      19.260789183079, 0.00746993224426355, 0.0258290314088979,
      11.1219103069748, 69.5407769633133, 0.032420686432392, 19.2715930796263,
      0.00671945255000025, 0.023249614737845, 11.1244932101157,
      69.5350325503671, 0.0291858866732368, 19.2813192855561, 0.00604468410605343,
      0.0209274764530073, 11.1268181715895, 69.5298614441577, 0.0262731721356657,
      19.290075051558, 0.00543792953346754, 0.0188370149696871,
      11.1289109192348, 69.5252065301769, 0.0236506028864187, 19.2979570031987,
      0.00489228524182762, 0.0169551648110116, 11.1307946207317,
      69.5210163641336, 0.0212893810171205, 19.3050521840647, 0.00489228524182762,
      0.0169551648110116, 11.1307946207317, 69.5210163641336, 0.0212893810171205,
      19.3050521840647)), row.names = 453:492, class = "data.frame")

  r = s$report()
  expect_equal(head(r, 40L), expected_head)
  expect_equal(tail(r, 40L), expected_tail)
})
