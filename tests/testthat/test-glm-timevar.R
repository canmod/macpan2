test_that("Basic insertion with identity link works", {
  design_matrix <- diag(3)
  timevar_coef <- matrix(0.1, nrow = 3, ncol = 1)
  
  expect_error(
    mp_tmb_insert_glm_timevar(
      mp_tmb_model_spec(),
      parameter_name = "beta",
      design_matrix = design_matrix,
      timevar_coef = timevar_coef,
      link_function = mp_identity
    )
    , regexp = "not found in model"
  )
  out = mp_tmb_insert_glm_timevar(
    mp_tmb_model_spec(default = list(beta = 0.2)),
    parameter_name = "beta",
    design_matrix = design_matrix,
    timevar_coef = timevar_coef,
    link_function = mp_identity  # Assuming identity link for simplicity
  )
  
  # Should have inserted default variables
  expect_true("matrix_coef_beta" %in% names(out$default))
  expect_true("linear_pred_beta" %in% names(out$default))
  expect_true("time_var_beta" %in% names(out$default))
  expect_true("timeseries_beta" %in% names(out$default))
  
  # Should have inserted integer variables
  expect_true("matrix_row_beta" %in% names(out$integers))
  expect_true("matrix_col_beta" %in% names(out$integers))
  expect_true("time_index_beta" %in% names(out$integers))
  
  # Should insert expressions into 'before' and 'during' phases
  expect_true(any(grepl("linear_pred_beta", out$before)))
  expect_true(any(grepl("timeseries_beta", out$before)))
  expect_true(any(grepl("beta ~ time_var", out$during)))
})

test_that("Sparse matrix conversion zeros small elements", {
  model <- mp_tmb_model_spec(default = list(gamma = 0.2))
  design_matrix <- matrix(c(0.001, 0.1, 0, 0), nrow = 2)
  timevar_coef <- matrix(0.2, nrow = 2, ncol = 1)
  
  tol = 0.01
  out <- mp_tmb_insert_glm_timevar(
    model,
    parameter_name = "gamma",
    design_matrix = design_matrix,
    timevar_coef = timevar_coef,
    sparsity_tolerance = tol
  )
  
  matrix_coefs <- out$default[["matrix_coef_gamma"]]
  expect_false(any(abs(matrix_coefs) < tol))
})

test_that("Handles empty design matrix gracefully", {
  model <- mp_tmb_model_spec(default = list(empty = 1))
  
  expect_error(
      mp_tmb_insert_glm_timevar(
        model,
        "empty",
        empty_matrix,
        empty_matrix
      )
    , regexp = "The design matrix must have at least one row"
  )
  
  expect_error(
      mp_tmb_insert_glm_timevar(
        model,
        "empty",
        matrix(0, 3, 2),
        matrix(0, 3, 2)
      )
    , regexp = "The design_matrix and timevar_coef matrix "
  )
})

test_that("Link function is correctly referenced", {
  model <- mp_tmb_model_spec(default = list(phi = 1))
  design_matrix <- diag(1)
  timevar_coef <- matrix(1, nrow = 1, ncol = 1)
  
  out <- mp_tmb_insert_glm_timevar(
    model,
    "phi",
    design_matrix,
    timevar_coef,
    link_function = mp_log
  )
  
  expect_true(any(grepl("log\\(phi\\)", out$before)))
})

test_that("Time-varying beta affects SI model dynamics as expected", {
  # Define time points and design matrix for linear trend
  t_steps <- 1:50
  X <- cbind(1, t_steps)  # Intercept + linear trend
  
  # Coefficients: intercept = log(0.05), slope = 0.02
  coef_mat <- matrix(c(log(0.1), 0.04), ncol = 1)
  
  # Build SI model spec with default beta
  model <- mp_tmb_model_spec(
      before = S ~ N - I
    , during = mp_per_capita_flow("S", "I", "beta * I / N", "infection")
    , default = list(S = 999, I = 1, N = 1000, beta = 1)
  ) 
  
  # Insert time-varying beta (log link)
  model_tv <- mp_tmb_insert_glm_timevar(mp_hazard(model)
    , parameter_name = "beta"
    , design_matrix = X
    , timevar_coef = coef_mat
    , link_function = mp_log
  )
  
  # Run simulation
  sims <- (model_tv
    |> mp_simulator(
        time_steps = max(t_steps)
      , outputs = c("beta", "infection")
    )
    |> mp_trajectory()
  )
  
  actual_beta = sims |> filter(matrix == "beta") |> pull(value)
  
  # Check that beta increases over time as per the design matrix
  expected_beta <- exp(X %*% coef_mat) |> as.numeric()
  expect_equal(actual_beta, expected_beta, tolerance = 1e-6)
  
  # (sims
  #   |> ggplot()
  #   + aes(time, value)
  #   + geom_line()
  #   + facet_wrap(~matrix, ncol = 1, scales = 'free')
  # )
})
