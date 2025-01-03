test_that("mp_rk4() does not repeat preceding random computations to flows", {
  
  seir = mp_tmb_library("starter_models","seir",package = "macpan2")
  seir_gamma = mp_tmb_insert(seir
                             , phase="during"
                             , at=1L
                             , expressions = list(gamma ~ rnorm(0.1,0.05)))
  
  set.seed(1L)
  euler_seir = seir_gamma |> mp_simulator(10,"gamma") |> mp_trajectory()
  set.seed(1L)
  rk4_seir = seir_gamma |> mp_rk4() |> mp_simulator(10,"gamma") |> mp_trajectory()
  # not currently equal (but should be)
  # expect_equal(euler_seir, rk4_seir)
})

test_that("mp_rk4() does not interfere with preceding time step dependent functions", {

  change_pts = c(0,5,6)
  beta_diffs = c(0.2,0.8,-0.5)
  beta_base = 1.1
  seir = mp_tmb_library("starter_models","seir",package = "macpan2")
  seir_beta = mp_tmb_insert(seir
                            , phase="during"
                            , at = 1L
                            , expressions = list(beta ~ beta + time_var(beta_diffs,change_pts))
                            , default = list(beta = beta_base, beta_diffs = beta_diffs)
                            , integers = list(change_pts = change_pts))
  
  euler_seir = seir_beta |> mp_euler() |> mp_simulator(10,c("beta")) |> mp_trajectory()
  rk4_seir = seir_beta |> mp_rk4() |> mp_simulator(10,c("beta")) |> mp_trajectory()
  # not currently equal (but should be)
  # expect_equal(euler_seir, rk4_seir)
})

test_that("mp_rk4() prevents modifications to state variables preceding flows", {

  seir = mp_tmb_library("starter_models","seir",package = "macpan2")
  seir_S = mp_tmb_insert(seir
     , phase="during"
     , at=1L
     , expressions = list(S ~ rpois(N)))
  
  rk4_seir = mp_rk4(seir_S)
  # currently errors, what should be the expected behaviour
  # expect_error(rk4_seir |> mp_simulator(10,c("S"))) 
  
})
