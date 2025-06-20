test_that("backtrans works properly for logit", {
    spec <- ("starter_models"
        |> mp_tmb_library("seir", package = "macpan2")
        |> mp_tmb_update(default = list(beta = 0.6))
    )
    res <- (spec
        |> mp_tmb_insert_backtrans(c("beta"),transformation = mp_logit)
        |> mp_default()
        |> dplyr::filter(matrix == "logit_beta")
    )
    expect_equal(res$value, qlogis(0.6))
})

test_that("mix of transformations works", {
  init_spec = ("starter_models"
    |> mp_tmb_library("seir", package = "macpan2")
    |> mp_tmb_insert("before"
        , at = 1L
        , expressions = list(beta ~ p * beta)
        , default = list(p = 0.3)
    )
    |> mp_tmb_insert("during"
        , at = 1L
        , expressions = list(frac ~ S/N)
    )
  )
  init_defaults = mp_default_list(init_spec)
  expected_spec = (init_spec
    |> mp_tmb_insert("before"
        , at = 1L
        , expressions = list(
            beta ~ exp(log_beta)
          , alpha ~ exp(log_alpha)
          , p ~ (1/(1 + exp(-logit_p)))
          , gamma ~ (sqrt_gamma^2)
        )
        , default = list(
            log_beta = log(init_defaults$beta)
          , log_alpha = log(init_defaults$alpha)
          , logit_p = qlogis(init_defaults$p)
          , sqrt_gamma = sqrt(init_defaults$gamma)
        )
    )
    |> mp_tmb_insert("during"
        , at = Inf
        , expressions = list(
            logit_frac ~ (log(frac) - log(1-frac))
          , sqrt_R ~ (R^0.5)
          , sqrt_I ~ (I^0.5)
          , log_E ~ log(E)
          , log_S ~ log(S)
        )
    )
  )
  spec = (init_spec
    |> mp_tmb_insert_backtrans("gamma", mp_sqrt)
    |> mp_tmb_insert_backtrans("p", mp_logit)
    |> mp_tmb_insert_backtrans(c("beta", "alpha"), mp_log)
    |> mp_tmb_insert_trans("frac", mp_logit)
    |> mp_tmb_insert_trans(c("R", "I"), mp_sqrt)
    |> mp_tmb_insert_trans(c("E", "S"), mp_log)
  )
  implicit_backtrans = c("sqrt_gamma", "logit_p", "log_beta", "log_alpha")
  implicit_trans = c("logit_frac", "sqrt_R", "sqrt_I", "log_E", "log_S")
  implicit_spec = (init_spec
    |> mp_tmb_implicit_backtrans(implicit_backtrans)
    |> mp_tmb_implicit_trans(implicit_trans)
  )
  expect_equal(
      mp_default_list(spec)
    , mp_default_list(expected_spec)
  )
  expect_equal(
      spec$before
    , expected_spec$before
    , ignore_attr = TRUE
  )
  expect_equal(
      spec$during
    , expected_spec$during
    , ignore_attr = TRUE
  )
  expect_setequal(
      names(mp_default_list(spec))
    , names(mp_default_list(implicit_spec))
  )
  expect_setequal(
      unname(mp_default_list(spec))
    , unname(mp_default_list(implicit_spec))
  )
  expect_setequal(
      spec$before
    , implicit_spec$before
  )
  expect_setequal(
      spec$during
    , implicit_spec$during
  )
})

test_that("implicit backtransformation in calibration is consistent", {
  cal = test_cache_read("CAL-sir_50_beta_infection.rds")
  cal_log = test_cache_read("CAL-sir_50_log_beta_infection.rds")
  mp_optimize(cal)
  mp_optimize(cal_log)
  expect_equal(mp_tmb_coef(cal), mp_tmb_coef(cal_log), tolerance = 1e-4)
})
