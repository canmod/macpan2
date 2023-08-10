library(macpan2)
library(macpan2helpers)
library(tidyverse)

## note: typos in $tmb don't give warnings (assumes it's another matrix
## you want to add ...)
setup_everything <- function(v = 1.0) {
    ## can't call the arg do_pred_sdreport or we get a recursive
    ## default arg reference error ...
    ## can't specify as an integer here ???
    mk_sim <- function(do_pred_sdreport = v,
                       init_state = c(S = 99, I = 1, R = 0)) {
        sir = Compartmental(system.file("starter_models", "sir", package = "macpan2"))
    sim <- sir$simulators$tmb(
                       .do_pred_sdreport = do_pred_sdreport,
                       time_steps = 100,
                       state = init_state,
                       flow = c(foi = NA, gamma = 0.1),
                       beta = 0.2,
                       N = empty_matrix
                       )
    return(sim)
}
sir_simulator <- mk_sim()
sir_results = sir_simulator$report(.phases = "during")
set.seed(101)
sir_prevalence = (sir_results
    |> dplyr::select(-c(matrix, col))
    |> filter(row == "I")
    |> mutate(obs_val = value + rnorm(n(), sd = 1))
)
mk_calibrate(sir_simulator,
     data = data.frame(I_obs = sir_prevalence$obs_val),
     params = list(beta = 1, I_sd = 1),
     transforms = list(beta = "log", I_sd = "log"),
     exprs = list(log_lik ~ dnorm(I_obs, I, I_sd)),
     )
    fit <- sir_simulator$optimize$nlminb()
    ss <- TMB::sdreport(sir_simulator$ad_fun())
    return(ss)
}

S1 <- setup_everything()
length(ss1$sd)

S2 <- setup_everything(1L)
all.equal(S1, S2)
S3 <- setup_everything(TRUE)
all.equal(S1, S3)

S4 <- setup_everything(0)
length(ss4$sd)

S5 <- setup_everything(FALSE)
all.equal(S4, S5)
