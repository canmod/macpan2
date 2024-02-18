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

do_weekly <- TRUE

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
mp_tmb_insert(sir_spec, at = Inf,
              expressions = list(incidence ~ recovery),
              must_save = "incidence"
              )

## ??? how do I tell to report additional variables in the output? used
##  to be "mats_to_save". Adding "incidence" to "must_save" doesn't get it
## reported ...

if (do_weekly) {
    mp_tmb_insert(sir_spec, at = Inf,
                  integers = list(wk = wk),
                  phase = "during",
                  default = list(nweek = nweek))

    mp_tmb_insert(sir_spec, phase = "after",
                  expressions =
                      list(wk_incidence ~ group_sums(rbind_time(incidence), wk, nweek)))
}


## ----sir_setup----------------------------------------------------------------
sir_simulator = mp_simulator(sir_spec
  , time_steps = 100
  , outputs = c("S", "I", "R")
  , default = list(N = 100, beta = 0.2, gamma = 0.1)
)
sir_results = mp_trajectory(sir_simulator)
summary(sir_results)
head(sir_results)
tail(sir_results)

## The following error was thrown by the TMB engine:
##   Group indexes are out of range.
## This error occurred at the following expression:
##   wk_incidence ~ group_sums(rbind_time(incidence), wk, nweek)

## ??? could this be specified any more informatively?
## which index was out of range?

## stopped here, but the rest of the code is retained because
    ## we'll want it eventually

stop("stopped here")   

## ----sir_noise----------------------------------------------------------------
set.seed(101)
sir_prevalence = (sir_results
    |> dplyr::select(-c(row, col))
    |> filter(matrix == "I")
    |> mutate(obs_val = value + rnorm(n(), sd = 1))
)
gg0 <- ggplot(sir_prevalence, aes(time)) +
    geom_point(aes(y = obs_val)) +
    geom_line(aes(y = value))
print(gg0)


## ----mk_calibrate-------------------------------------------------------------
macpan2helpers::mk_calibrate(sir_simulator,
   data = data.frame(I_obs = sir_prevalence$obs_val),
   params = list(beta = 1, I_sd = 1),
   transforms = list(beta = "log", I_sd = "log"),
   exprs = list(log_lik ~ dnorm(I_obs, I, I_sd))
)


## ----plot1--------------------------------------------------------------------
(sir_simulator 
 |> mp_trajectory()
 |> filter(matrix == "I")
 |> ggplot(aes(time, value)) 
 + geom_line()
)


## ----plot2--------------------------------------------------------------------
(sir_simulator$report(c(0,0)) |>
 filter(matrix == "I") |>
 ggplot(aes(time, value)) + geom_line()
)


## ----repl_param---------------------------------------------------------------
sir_simulator$replace$params(
    c(0         , 1         )
  , c("log_beta", "log_I_sd")
)


## ----sir_fit, results = "hide"------------------------------------------------
fit <- sir_simulator$optimize$nlminb()


## ----check_fit----------------------------------------------------------------
print(fit)


## ----params-------------------------------------------------------------------
exp(fit$par)


## ----coef_funs----------------------------------------------------------------
coefnames <- function(x) {
    x$current$params_frame()$mat
}
drop_trans <- function(x) gsub("(log|logit)_", "", x)


## ----params_fancy, eval = require("broom.mixed")------------------------------
ff <- sir_simulator$ad_fun()
class(ff) <- "TMB"
(broom.mixed::tidy(ff, conf.int = TRUE)
    |> select(-c(type, std.error))
    |> mutate(term = drop_trans(coefnames(sir_simulator)))
    |> mutate(across(where(is.numeric), exp))
    |> as_tibble()
)


## ----plot_results-------------------------------------------------------------
sim_vals <- (sir_simulator
  |> mp_trajectory()
  |> filter(matrix == "I")
)
gg0 + geom_line(data = sim_vals, aes(y= value), colour = "red")


