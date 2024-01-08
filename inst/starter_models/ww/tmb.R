library(macpan2)

## expression lists
###################################################

## free form computations for convenience
computations = list(
  N ~ sum(S, E, Ia, Ip, Im, Is, R, H, ICUs, ICUd, H2, D)
)

## absolute flow rates (per time only)
flow_rates = list(
    SE ~ S * (beta0 / N) * (Ia * Ca + Ip * Cp + Im * Cm * (1 - iso_m) + Is * Cs *(1 - iso_s))
  , EIa ~ E * alpha * sigma
  , EIp ~ E * (1 - alpha)* sigma
  , IaR ~ Ia * gamma_a
  , IpIm ~ Ip * mu * gamma_p
  , ImR ~ Im * gamma_m
  , IpIs ~ Ip * (1 - mu) * gamma_p
  , IsICUs ~ Is * (1 - nonhosp_mort) * (1 - phi1) * (1 - phi2) * gamma_s
  , ICUsH2 ~ ICUs * psi1
  , H2R ~ H2 * psi3
  , IsH ~ Is * (1 - nonhosp_mort) * phi1 * gamma_s
  , IsICUd ~ Is * (1 - nonhosp_mort) * (1 - phi1) * phi2 * gamma_s
  , ICUdD ~ ICUd * psi2
  , HR ~ H * rho
  , IaW ~ Ia * nu
  , IpW ~ Ip * nu 
  , ImW ~ Im * nu
  , IsW ~ Is * nu
  , WA ~ W * xi
)

## state updates
state_updates = list(
    S ~ S - SE
  , E ~ E + SE - EIa - EIp
  , Ia ~ Ia + EIa - IaR
  , Ip ~ Ip + EIp - IpIm - IpIs
  , Im ~ Im + IpIm - ImR
  , Is ~ Is + IpIs - IsICUs - IsH - IsICUd
  , R ~ R + IaR + H2R + HR + ImR
  , H ~ H + IsH - HR
  , ICUs ~ ICUs + IsICUs - ICUsH2
  , ICUd ~ ICUd + IsICUd - ICUdD
  , H2 ~ H2 + ICUsH2 - H2R
  , D ~ D + ICUdD
  , W ~ W + IaW + IpW + ImW + IsW - WA
  , A ~ A + WA
)

## simple unstructured scalar expression
spec =  mp_tmb_model_spec(
    before = computations
  , during = c(
      flow_rates
    , state_updates
  )
  , default = list(
        S = 1.00E+06, E = 1
      , Ia = 0, Ip = 0, Im = 0, Is = 0
      , R = 0
      , H = 0, ICUs = 0, ICUd = 0, H2 = 0
      , D = 0
      , W = 0, A = 0
      , beta0        = 1        # Baseline (non-intervention) transmission across categories
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
      #, N            = 1.00E+06 # Population size
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
  )
)
