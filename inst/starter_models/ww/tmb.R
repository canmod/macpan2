library(macpan2)

computations = list(
  N ~ sum(S, E, Ia, Ip, Im, Is, R, H, ICUs, ICUd, H2, D)
)

flow_rates = list(
    S.E ~ S * (beta0 / N) * (Ia * Ca + Ip * Cp + Im * Cm * (1 - iso_m) + Is * Cs *(1 - iso_s))
  , E.Ia ~ E * alpha * sigma
  , E.Ip ~ E * (1 - alpha)* sigma
  , Ia.R ~ Ia * gamma_a
  , Ip.Im ~ Ip * mu * gamma_p
  , Im.R ~ Im * gamma_m
  , Ip.Is ~ Ip * (1 - mu) * gamma_p
  , Is.ICUs ~ Is * (1 - nonhosp_mort) * (1 - phi1) * (1 - phi2) * gamma_s
  , Is.ICUd ~ Is * (1 - nonhosp_mort) * (1 - phi1) * phi2 * gamma_s
  , ICUs.H2 ~ ICUs * psi1
  , H2.R ~ H2 * psi3
  , Is.H ~ Is * (1 - nonhosp_mort) * phi1 * gamma_s
  , ICUd.D ~ ICUd * psi2
  , H.R ~ H * rho
  , Ia.W ~ Ia * nu
  , Ip.W ~ Ip * nu 
  , Im.W ~ Im * nu
  , Is.W ~ Is * nu
  , W.A ~ W * xi
)

state_updates = list(
    S ~ S - S.E
  , E ~ E + S.E - E.Ia - E.Ip
  , Ia ~ Ia + E.Ia - Ia.R
  , Ip ~ Ip + E.Ip - Ip.Im - Ip.Is
  , Im ~ Im + Ip.Im - Im.R
  , Is ~ Is + Ip.Is - Is.ICUs - Is.H - Is.ICUd
  , H ~ H + Is.H - H.R
  , ICUs ~ ICUs + Is.ICUs - ICUs.H2
  , ICUd ~ ICUd + Is.ICUd - ICUd.D
  , H2 ~ H2 + ICUs.H2 - H2.R
  , R ~ R + Ia.R + Im.R + H.R + H2.R
  , D ~ D + ICUd.D
  , W ~ W + Ia.W + Ip.W + Im.W + Is.W - W.A
  , A ~ A + W.A
)

foi = S.E ~ (beta0 / N) * (Ia * Ca + Ip * Cp + Im * Cm * (1 - iso_m) + Is * Cs *(1 - iso_s))
flows = list(
    mp_per_capita_flow("S", "E", foi)
  , mp_per_capita_flow("E", "Ia", E.Ia ~ alpha * sigma)
  , mp_per_capita_flow("E", "Ip", E.Ip ~ (1 - alpha)* sigma)
  , mp_per_capita_flow("Ia", "R", Ia.R ~ gamma_a)
  , mp_per_capita_flow("Ip", "Im", Ip.Im ~ mu * gamma_p)
  , mp_per_capita_flow("Im", "R", Im.R ~ gamma_m)
  , mp_per_capita_flow("Ip", "Is", Ip.Is ~ (1 - mu) * gamma_p)
  , mp_per_capita_flow("Is", "ICUs", Is.ICUs ~ (1 - nonhosp_mort) * (1 - phi1) * (1 - phi2) * gamma_s)
  , mp_per_capita_flow("Is", "ICDd", Is.ICUd ~ (1 - nonhosp_mort) * (1 - phi1) * phi2 * gamma_s)
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

# state_updates = list(
#     S ~ S - S.E
#   , E ~ E + S.E - E.Ia - E.Ip
#   , Ia ~ Ia + E.Ia - Ia.R
#   , Ip ~ Ip + E.Ip - Ip.Im - Ip.Is
#   , Im ~ Im + Ip.Im - Im.R
#   , Is ~ Is + Ip.Is - Is.ICUs - Is.H - Is.ICUd
#   , H ~ H + Is.H - H.R
#   , ICUs ~ ICUs + Is.ICUs - ICUs.H2
#   , ICUd ~ ICUd + Is.ICUd - ICUd.D
#   , H2 ~ H2 + ICUs.H2 - H2.R
#   , R ~ R + Ia.R + Im.R + H.R + H2.R
#   , D ~ D + ICUd.D
#   , W ~ W + Ia.W + Ip.W + Im.W + Is.W - W.A
#   , A ~ A + W.A
# )

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
specs =  list(
    explicit = mp_tmb_model_spec(
        before = computations
      , during = flows
      , default = default
    )
  , implicit = mp_tmb_model_spec(
        before = computations
      , during = c(flow_rates, state_updates)
      , default = default
    )
)
spec = specs[["implicit"]]
