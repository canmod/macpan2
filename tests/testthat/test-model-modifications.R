library(macpan2); library(testthat); library(dplyr); library(tidyr); library(ggplot2)
test_that("expressions can be deleted from model spec",{
  sir = mp_tmb_library("starter_models","sir", package = "macpan2")
  # remove I to R flow
  sir_delete = mp_tmb_delete(sir,"during",2)
  si = mp_tmb_library("starter_models","si", package = "macpan2")
  
  expect_identical(sir_delete$change_model$change_frame(), si$change_model$change_frame())
})

test_that("expressions can be deleted from simulators",{
  sir = mp_tmb_library("starter_models","sir", package = "macpan2")
  sir_sim = mp_simulator(sir, 5, "S")
  
  sir_sim$delete$expressions(1,"before")
  expect_equal(sir_sim$tmb_model$expr_list$before, list())
})

test_that("parameter transformations can be added to simulators",{
  sir = mp_tmb_library("starter_models","sir", package = "macpan2")
  sir_sim = mp_simulator(sir, 5, "S")
  # add log(beta) transformation
  sir_sim$add$transformations(Log("beta"))
  expect_true("log_beta" %in% sir_sim$matrix_names())
})

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
