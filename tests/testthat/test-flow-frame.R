test_that("flow function arg errors and warnings are appropriate", {
  expect_error(
      mp_per_capita_flow(from = "S", to = "I", rate = "beta * I")
    , "flow_name must be specified when rate is a one_sided formula or character string"
  )
  expect_error(
      mp_inflow(to = "I", rate = "beta")
    , "flow_name must be specified when rate is a one_sided formula or character string"
  )
  expect_error(
      mp_outflow(from = "S", rate = "beta")
    , "flow_name must be specified when rate is a one_sided formula or character string"
  )
  expect_error(
      mp_per_capita_inflow(from = "N", to = "S", rate = "mu")
    , "flow_name must be specified when rate is a one_sided formula or character string"
  )
  expect_error(
      mp_per_capita_outflow(from = "S", rate = "beta")
    , "flow_name must be specified when rate is a one_sided formula or character string"
  )
  expect_warning(
      mp_per_capita_flow(from = "S", to = "I", rate = "beta * I", abs_rate = "infection")
    , "The abs_rate argument is deprecated; please use 'flow_name' instead"
  )
  expect_warning(
      mp_inflow(to = "I", rate = "beta", abs_rate = "infection")
    , "The abs_rate argument is deprecated; please use 'flow_name' instead"
  )
  expect_warning(
      mp_inflow(to = "I", rate = "beta", abs_rate = "infection")
    , "The abs_rate argument is deprecated; please use 'flow_name' instead"
  )
  expect_warning(
      mp_per_capita_inflow(from = "N", to = "S", rate = "mu", abs_rate = "birth")
    , "The abs_rate argument is deprecated; please use 'flow_name' instead"
  )
  expect_warning(
      mp_per_capita_outflow(from = "S", rate = "beta", abs_rate = "death")
    , "The abs_rate argument is deprecated; please use 'flow_name' instead"
  )
})

test_that("empty specs give empty information", {
  expect_identical(nrow(mp_flow_frame(mp_tmb_model_spec())), 0L)
  expect_identical(mp_state_vars(mp_tmb_model_spec()), character())
})

test_that("warnings given if formulas are between flows", {
  expect_warning({
      mp_tmb_model_spec(
        during = list(
            mp_per_capita_flow("S", "I", "beta * I / N",  "infection")
          , N ~ S + I
          , mp_per_capita_flow("I", "R", "gamma",  "recovery")
        )
      )
    }
    , "Raw formula-valued expressions were inserted between flow-based expressions"
  )
})

test_that("find all paths works with non-dag", {
  shiver = test_cache_read("SPEC-shiver.rds")
  expect_setequal(
      shiver |> mp_flow_frame(loops = "^vacc") |> find_all_paths("S")
    , list(
          c("S", "E", "I", "H", "R")
        , c("S", "E", "I", "R")
        , c("S", "V", "E", "I", "H", "R")
        , c("S", "V", "E", "I", "R")
      )
  )
})
