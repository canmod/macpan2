## ----pkgs, message = FALSE----------------------------------------------------
library(macpan2)
library(macpan2helpers)
mp2hver <- "0.0.3"
if (packageVersion("macpan2helpers") < mp2hver) {
    stop("please install macpan2helpers version >= ", mp2hver)
}
library(dplyr)
library(tidyr)
library(ggplot2); theme_set(theme_bw())

## ----sir_spec-----------------------------------------------------------------
sir_spec = mp_tmb_library("starter_models"
  , "sir"
  , package = "macpan2"
)

## hard code weekly indicator (I'd rather not but this seems easiest)
## why does %/% coerce integer -> numeric?
## is there a way to change 'integers' at runtime?
## can mp_tmb_update() be used on simulators ?

index_offset <- 0  ## 0 or 1
wk <- as.integer((0:99) %/% 7) + index_offset
nweek <- max(wk) + (1-index_offset)

## add daily incidence
sir_spec = mp_tmb_insert(sir_spec, at = Inf,
              expressions = list(incidence ~ recovery),
              must_save = "incidence"
              )

sir_spec = mp_tmb_insert(sir_spec, at = Inf,
              integers = list(wk = wk),
              phase = "during",
              default = list(nweek = nweek))

sir_spec = mp_tmb_insert(sir_spec, phase = "after",
              expressions =
                  list(wk_incidence ~ group_sums(rbind_time(incidence), wk, rep(0, nweek))),
                  must_save = "wk_incidence")



## ----sir_setup----------------------------------------------------------------
sir_simulator = mp_simulator(sir_spec
  , time_steps = 100
  , outputs = c("S", "I", "R", "incidence", "wk_incidence")
  , default = list(N = 100, beta = 0.2, gamma = 0.1)
)

## is there a nice front end for reporting after phase?
wk_ind_obs <- (sir_simulator$report(.phases = "after")
    |> filter(matrix == "wk_incidence")
    |> mutate(obs_val = rnorm(n = n(), mean = value, sd = 0.1))
    |> pull(obs_val)
)

## ----mk_calibrate-------------------------------------------------------------
## mm <- macpan2helpers::mk_calibrate(sir_simulator,
##    data = data.frame(inc_obs = wk_ind_obs),
##    params = list(beta = 1, I_sd = 1),
##    transforms = list(beta = "log", I_sd = "log"),
##    exprs = list(log_lik ~ dnorm(inc_obs, wk_incidence, I_sd))
## )

## mk_calibrate isn't quite flexible enough to handle this case
## (it assumes that sim vars are 'during' vars, not something created
##  explicitly in the 'after' phase; also tries to replace time steps
sir_simulator$add$matrices(log_lik = empty_matrix)
sir_simulator$add$matrices(inc_obs = wk_ind_obs)
sir_simulator$add$matrices(I_sd = 1)
sir_simulator$insert$expressions(
               log_lik ~ dnorm(inc_obs, wk_incidence, I_sd),
               .phase = 'after', .at = Inf)
pframe <- data.frame(mat = c("log_beta", "log_I_sd"), row = 0, col = 0, default = c(1, 1))
sir_simulator$add$transformations(Log("beta"))
sir_simulator$add$transformations(Log("I_sd"))
sir_simulator$replace$params_frame(pframe)
sir_simulator$replace$obj_fn(~ -sum(log_lik))

sir_simulator
sir_simulator$ad_fun()$fn()
sir_simulator$ad_fun()$fn(c(0.9,0.9))


fit <- sir_simulator$optimize$nlminb()


