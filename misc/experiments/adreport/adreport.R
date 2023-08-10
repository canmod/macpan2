library(macpan2)
library(macpan2helpers)
library(tidyverse)
library(peakRAM)

## note: typos in $tmb don't give warnings (assumes it's another matrix
## you want to add ...)
setup_everything <- function(adreport = 1.0, jointCov = FALSE) {
    ## can't call the arg do_pred_sdreport or we get a recursive
    ## default arg reference error ...
    ## can't specify as an integer here ???
    mk_sim <- function(do_pred_sdreport = adreport,
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
    ss <- TMB::sdreport(sir_simulator$ad_fun(), getJointPrecision = jointCov)
    return(ss)
}

## various configurations
S1 <- setup_everything()
length(S1$sd)

S2 <- setup_everything(1L)
stopifnot(all.equal(S1, S2))
S3 <- setup_everything(TRUE)
stopifnot(all.equal(S1, S3))

S4 <- setup_everything(0)
stopifnot(length(S4$sd) == 0)

S5 <- setup_everything(FALSE)
stopifnot(all.equal(S4, S5))

## jointCov makes no difference in this case because we don't have random effects ...
p1 <- peakRAM(s1 <- setup_everything(TRUE))
p2 <- peakRAM(s2 <- setup_everything(FALSE))
p3 <- peakRAM(s3 <- setup_everything(TRUE, jointCov = TRUE))
p4 <- peakRAM(s4 <- setup_everything(FALSE, jointCov = TRUE))

results_simple <- rbind(p1, p2, p3, p4)


### RBF example from time-varying vignette

## change the structure slightly, because we want to count the time/memory
## only of the sdreport() part (the setup and optimization was pretty trivial
## in the previous case), and because re-running the setup just to try out
## getJointPrecision TRUE/FALSE cases is a waste of time

setup2 <- function(adreport = TRUE, d = 20, n  = 500) {
    sir = Compartmental(system.file("starter_models", "sir_waning", package = "macpan2"))
    ## ----sim_rbf------------------------------------------------------------------
    set.seed(1L)
    simulator = sir$simulators$tmb(
                                   time_steps = n
                                 , state = c(S = 100000 - 500, I = 500, R = 0)
                                 , flow = c(foi = NA, gamma = 0.2, wane = 0.01)
                                 , beta = 1
                                 , N = 100000
                                 , X = rbf(n, d)
                                 , b = rnorm(d, sd = 0.01)
                                 , incidence = empty_matrix
                                 , eta = empty_matrix
                                 , .do_pred_sdreport = adreport
                                 , .mats_to_save = c("state", "incidence", "beta")
                                 , .mats_to_return = c("state", "incidence", "beta")
                               )
    simulator$insert$expressions(
                                     eta ~ gamma * exp(X %*% b)
                                   , .phase = "before"
                                   , .at = Inf
                                 )
    simulator$insert$expressions(
                                     beta ~ eta[time_step(1)] / clamp(S/N, 1/100)
                                   , .phase = "during"
                                   , .at = 1)
    simulator$insert$expressions(
                                     incidence ~ I
                                   , .vec_by_states = "total_inflow"
                                   , .phase = "during"
                                   , .at = Inf
                                 )
    simulator$replace$params(
                                      default = rnorm(d, sd = 0.01)
                                    , mat = rep("b", d)
                                    , row = seq_len(d) - 1L
                                  )
    obs_I <- (
        simulator$report(.phases = "during")
        |> filter(row == "I", matrix == "state")
        |> mutate(across(value, ~ rnorm(n(), ., sd = 50)))
        |> pull(value)
    )
    simulator$add$matrices(
                      I_obs = obs_I,
                      I_sim = empty_matrix, 
                      log_lik = empty_matrix,
                      .mats_to_save = c("I_sim"),
                      .mats_to_return = c("I_sim")
                  )
    simulator$insert$expressions(
                     I_sim ~ I,
                     .phase = "during",
                     .at = Inf
                 )
    simulator$add$matrices(I_sd = 1, rbf_sd = 1)
    simulator$insert$expressions(
                         log_lik ~ -sum(dnorm(I_obs, rbind_time(I_sim), I_sd)) +
                             -1*sum(dnorm(b, 0.0, rbf_sd)),
                         .phase = "after"
                     )
    simulator$replace$obj_fn(~ log_lik)
    simulator$add$transformations(Log("I_sd"))
    simulator$add$transformations(Log("rbf_sd"))
    params <- read.delim(sep = "|", header = TRUE,
                         text = "
mat         | row | col | default
log_I_sd    | 0   | 0   | 0
log_rbf_sd  | 0	  | 0   | 1
")
    simulator$replace$params_frame(params)
    rparams <- data.frame(
        mat  = "b",
        row = 0:19,
        col = 0,
        default = 0)
    simulator$replace$random_frame(rparams)
    fit <- simulator$optimize$nlminb()
    return(simulator)
}


sdr <- function(simulator, jointCov = FALSE) {
    ss <- TMB::sdreport(simulator$ad_fun(), getJointPrecision = jointCov)
    return(ss)
}

## now see how time and memory use scale with
## * setup2 config:
##   * number of time steps (n)
##   * number of RBF elements (d)
##   * adreport TRUE/FALSE
## * sdr config:
##   * jointCov

p5A <- peakRAM(S5A <- setup2())
p5B1 <- peakRAM(S5B1 <- sdr(S5A))
p5B2 <- peakRAM(S5B2 <- sdr(S5A, jointCov = TRUE))

p6A <- peakRAM(S6A <- setup2(adreport = FALSE))
p6B1 <- peakRAM(S6B1 <- sdr(S6A))
p6B2 <- peakRAM(S6B2 <- sdr(S6A, jointCov = TRUE))

results_rbf <- do.call(rbind, mget(ls(pattern="^p[56]")))

## now (1) check that cov matrices in jointCov case are the right size,
## (2) try simulating from ensemble ...

save("results_rbf", "results_simple", file = "adreport.rda")
