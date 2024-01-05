library(macpan2)

## expression lists
###################################################

## free form computations for convenience
computations = list(
  N ~ sum(S, E, Ia, Ip, Im, Is, R, H, ICUs, ICUd, H2, D)
)

## absolute flow rates (per time only)
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

## state updates
state_updates = list(
    S ~ S - S.E
  , E ~ E + S.E - EI.a - EI.p
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

## simple unstructured scalar expression
expr_list =  ExprList(
    before = computations
  , during = c(
      flow_rates
    , state_updates
  )
)
