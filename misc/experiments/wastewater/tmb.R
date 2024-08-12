library(macpan2); library(testthat); library(dplyr); library(tidyr); library(ggplot2)
macpan1.5 = readRDS("misc/experiments/wastewater/macpan1-5_comparison_info.RDS")

sim1.5 = (macpan1.5$sim
  |> group_by(Date, state)
  |> summarise(value = sum(value))
  |> ungroup()
  |> rename(time = Date, matrix = state)
  |> filter(matrix %in% c("conv", "W"))
  |> filter(!is.na(value))
  |> mutate(matrix = ifelse(matrix == "conv", "reported_incidence", matrix))
)

computations = list(
  N ~ sum(S, E, Ia, Ip, Im, Is, R, H, ICUs, ICUd, H2, D)
)

foi = "(beta / N) * (Ia * Ca + Ip * Cp + Im * Cm * (1 - iso_m) + Is * Cs *(1 - iso_s))"
flows = list(
    beta ~ beta0 * beta1
  , mp_per_capita_flow("S", "E", foi, "incidence")
  , mp_per_capita_flow("E", "Ia", "alpha * sigma", "E.Ia")
  , mp_per_capita_flow("E", "Ip",  "(1 - alpha) * sigma", "E.Ip")
  , mp_per_capita_flow("Ia", "R", Ia.R ~ gamma_a)
  , mp_per_capita_flow("Ip", "Im", Ip.Im ~ mu * gamma_p)
  , mp_per_capita_flow("Im", "R", Im.R ~ gamma_m)
  , mp_per_capita_flow("Ip", "Is", Ip.Is ~ (1 - mu) * gamma_p)
  , mp_per_capita_flow("Is", "ICUs", Is.ICUs ~ (1 - nonhosp_mort) * (1 - phi1) * (1 - phi2) * gamma_s)
  , mp_per_capita_flow("Is", "ICUd", Is.ICUd ~ (1 - nonhosp_mort) * (1 - phi1) * phi2 * gamma_s)
  , mp_per_capita_flow("ICUs", "H2", ICUs.H2 ~ psi1)
  , mp_per_capita_flow("H2", "R", H2.R ~ psi3)
  , mp_per_capita_flow("Is", "H", Is.H ~ (1 - nonhosp_mort) * phi1 * gamma_s)
  , mp_per_capita_flow("ICUd", "D", ICUd.D ~ psi2)
  , mp_per_capita_flow("H", "R", H.R ~ rho)
  , mp_per_capita_inflow("Ia", "W", Ia.W ~ nu)
  , mp_per_capita_inflow("Ip", "W", Ip.W ~ nu)
  , mp_per_capita_inflow("Im", "W", Im.W ~ nu)
  , mp_per_capita_inflow("Is", "W", Is.W ~ nu)
  , mp_per_capita_flow("W", "A", W.A ~ xi)
)


# set defaults
default = list(
    beta0        = 1        # Baseline (non-intervention) transmission across categories
  , Ca           = 2/3      # relative asymptomatic transmission (or contact)
  , Cp           = 1        # relative presymptomatic transmission (or contact)
  , Cm           = 1        # relative mildly symptomatic transmission (or contact)
  , Cs           = 1        # relative severely symptomatic transmission (or contact)
  , alpha        = 0.39     # Fraction of cases asymptomatic
  , sigma        = 1/3.3    # 1/time in exposed class
  , gamma_a      = 1/7      # 1/time for asymptomatic recovery
  , gamma_m      = 1/7      # 1/time for mildly symptomatic recovery
  , gamma_s      = 1/5.72   # 1/time for severely symptomatic transition to hospital/death
  , gamma_p      = 1/1.2    # 1/time in pre-symptomatic class
  , rho          = 1/10     # 1/time in hospital (acute care)
  , delta        = 0        # Fraction of acute-care cases that are fatal
  , mu           = 0.956    # Fraction of symptomatic cases that are mild
  #, E0           = 5        # Initial number exposed
  , nonhosp_mort = 0        # probability of mortality without hospitalization
  , iso_m        = 0        # Relative self-isolation/distancing of mild cases
  , iso_s        = 0        # Relative self-isolation/distancing of severe cases
  , phi1         = 0.76     # Fraction of hospital cases to ICU
  , phi2         = 0.26     # Fraction of ICU cases dying
  , psi1         = 1/20     # Rate of ICU back to acute care
  , psi2         = 1/8      # Rate of ICU to death
  , psi3         = 1/5      # Rate of post-ICU to discharge
  , c_prop       = 1/10     # fraction of incidence reported as positive tests
  , c_delay_mean = 11       # average delay between incidence and test report
  , c_delay_cv   = 0.25     # coefficient of variation of testing delay
  , proc_disp    = 0        # dispersion parameter for process error (0=demog stoch only)
  , zeta         = 0        # phenomenological heterogeneity parameter
  , nu           = 0.1        # something to do with waste-water
  , xi           = 0.5        # something to do with waste-water
  # initial states
  , S = 1.00E+06
  , E = 1
  , Ia = 0
  , Ip = 0
  , Im = 0
  , Is = 0
  , R = 0
  , H = 0
  , ICUs = 0
  , ICUd = 0
  , H2 = 0
  , D = 0
  , W = 0
  , A = 0
)

## model spec
ww_spec =  mp_tmb_model_spec(
    before = computations
  , during = flows
  , default = default
)


## only vax parameters and state initialization parameters
## are missing. we can work with this for now.
names(macpan1.5$params)[!names(macpan1.5$params) %in% names(ww_spec$default)]

tv = (macpan1.5$params_tv
  |> rename(time = Date, matrix = Symbol, value = Value)
  |> filter(matrix == "beta0")
)
obs = (macpan1.5$obs
  |> rename(time = date, matrix = var)
  |> filter(!is.na(value))
  |> mutate(matrix = ifelse(matrix == "report_inc", "reported_incidence", matrix))
)

## controls when the simulations start
starter = data.frame(
    time = as.Date("2020-01-15")
  , matrix = "reported_incidence"
  , value = 0
)

useful_params = names(macpan1.5$params) %in% names(ww_spec$default)
ww_spec = (ww_spec
  |> mp_tmb_update(
    default = c(macpan1.5$params[useful_params], list(S = macpan1.5$params$N, beta1 = 1))
  )
  |> mp_tmb_insert(phase = "before"
    , expressions = list(
        incidence_report_prob ~ 1 / (1 + exp(-logit_report_prob))
      , beta0 ~ exp(log_beta0)
      , nu ~ exp(log_nu)
    )
    , default = list(logit_report_prob = 0, log_beta0 = 0, log_nu = 0)
  )
  |> mp_tmb_insert_reports("incidence" 
    , report_prob = 0.5
    , mean_delay = 11
    , cv_delay = 0.25
  )
)

ww_cal = mp_tmb_calibrator(ww_spec |> mp_hazard()
  , data = rbind(starter, obs)
  , traj = list(
        reported_incidence = mp_neg_bin(disp = 0.1)
      , W = mp_normal(sd = 1)
    )
  , par = c("log_beta0", "log_nu")
  , tv = mp_rbf("beta1", dimension = 5)
  , outputs = c("reported_incidence", "W", "beta")
)

replicate(15, mp_optimize(ww_cal), simplify = FALSE)
cc = mp_tmb_coef(ww_cal)
print(cc)

sim = (ww_cal
  |> mp_trajectory_sd(conf.int = TRUE)
  |> mutate(
    time = starter$time + lubridate::days(time - 1)
  )
)
(
    ggplot()
  + geom_line(aes(time, value), data = sim, colour = "red")
  + geom_ribbon(aes(time, ymax = conf.high, ymin = conf.low), data = sim, fill = "pink", colour = "red")
  + geom_line(aes(time, value), data = sim1.5, colour = "blue")
  + geom_line(aes(time, value), data = obs, colour = "grey")
  + facet_wrap(~matrix, scales = "free_y")
)
