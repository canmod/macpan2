library(macpan2)

computations = list(
  N ~ sum(S, E, Ia, Ip, Im, Is, R, H, ICUs, ICUd, H2, D)
)

flow_rates = list(
    mp_per_capita_flow("S", "E", S.E ~ (beta0 / N) * (Ia * Ca + Ip * Cp + Im * Cm * (1 - iso_m) + Is * Cs *(1 - iso_s)))
  , mp_per_capita_flow("E", "Ia", E.Ia ~ alpha * sigma)
  , mp_per_capita_flow("E", "Ip", E.Ip ~ (1 - alpha)* sigma)
  , mp_per_capita_flow("Ia", "R", Ia.R ~ gamma_a)
  , mp_per_capita_flow("Ip", "Im", Ip.Im ~ mu * gamma_p)
  , mp_per_capita_flow("Im", "R", Im.R ~ gamma_m) 
  , mp_per_capita_flow("Ip", "Is", Ip.Is ~ (1 - mu) * gamma_p) 
  , mp_per_capita_flow("Is", "ICUs", Is.ICUs ~ (1 - nonhosp_mort) * (1 - phi1) * (1 - phi2) * gamma_s)
  , mp_per_capita_flow("Is", "ICUd", Is.ICUd ~ (1 - nonhosp_mort) * (1 - phi1) * phi2 * gamma_s)
  , mp_per_capita_flow("ICUs", "H2", ICUs.H2 ~ psi1) 
  , mp_per_capita_flow("H2", "R", H2.R ~ psi3) 
  , mp_per_capita_flow("Is", "H", Is.H ~ (1 - nonhosp_mort) * phi1 * gamma_s) 
  , mp_per_capita_flow("Is", "D", Is.D ~ (nonhosp_mort) * gamma_s) 
  , mp_per_capita_flow("ICUd", "D", ICUd.D ~ psi2)
  , mp_per_capita_flow("H", "R", H.R ~ rho)
)

## set defaults
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
)

## model specification
spec = mp_tmb_model_spec(
    before = computations
  , during = flow_rates
  , default = default
)
