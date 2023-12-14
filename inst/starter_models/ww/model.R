library(macpan2)

## expression lists
###################################################

## free form computations for convenience
computations = list(
  N ~ sum(S, E, Ia, Ip, Im, Is, R, H, ICUs, ICUd, H2, D, W, A)
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
  , IaW ~ Ia * nu # or * or N?
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
  , Im ~ Im + IpIm
  , Is ~ Is + IpIs - IsICUs - IsH - IsICUd
  , R ~ R + IaR + H2R + HR
  , H ~ H + IsH - HR
  , ICUs ~ ICUs + IsICUs - ICUsH2
  , ICUd ~ ICUd + IsICUd - ICUdD
  , H2 ~ H2 + ICUsH2 - H2R
  , D ~ D + ICUdD
  , W ~ W + IaW + IpW + ImW + IsW - WA
  , A ~ A + WA
)

## simple unstructured scalar expression
expr_list =  ExprList(
    before = computations
  , during = c(
      flow_rates
    , state_updates
  )
)