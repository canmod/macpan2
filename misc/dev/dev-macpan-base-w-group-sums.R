library(macpan2)
library(TMB)

compile('dev.cpp')
dyn.load(dynlib("dev"))

m = TMBModel(
  init_mats = MatsList(
      state = c(1-1e-2, 1e-2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    , from = c(0, 1, 1, 2, 3, 3, 4, 5, 5, 5, 5, 6, 7,  8, 9)
    , to = c(1, 2, 3, 10, 4, 5, 10, 6, 7, 8, 11, 9, 11,  10, 10)
    , alpha = 0.39
    , beta0 = 1
    , Ca = 2/3
    , Cp = 1
    , Cm = 1
    , Cs = 1
    , gamma_a = 1/7
    , gamma_p = 1/7
    , gamma_m = 1/7
    , gamma_s = 1/7
    , phi1 = 0.76
    , phi2 = 0.26
    , psi1 = 1/20
    , psi2 = 1/8
    , psi3 = 1/5
    , rho = 1/10
    , mu = 0.956
    , sigma = 1/3
    , iso_m = 0
    , iso_s = 0
    , nonhosp_mort = 0
    , N = empty_matrix
    , EIa = empty_matrix
    , EIp = empty_matrix
    , IaR = empty_matrix
    , IpIm = empty_matrix
    , IpIs = empty_matrix
    , ImR = empty_matrix
    , IsH = empty_matrix
    , IsICUs = empty_matrix
    , IsICUd = empty_matrix
    , IsD = empty_matrix
    , ICUsH2 = empty_matrix
    , ICUdD = empty_matrix
    , H2R = empty_matrix
    , HR = empty_matrix
    , foi = empty_matrix
    , rate = empty_matrix
    , flow = empty_matrix
    , .mats_to_save = c("state", "rate", "flow")
    , .mats_to_return = c("state", "rate", "flow")
    , .dimnames = list(
      state = list(c("S", "E", "Ia", "Ip", "Im", "Is", "ICUs", "ICUd", "H", "H2", "R", "D"))
      )
    )
  , expr_list = ExprList(
    during = list(
        N ~ sum(state) - state[11, 0]
      , EIa ~alpha*sigma
      , EIp ~(1-alpha)*sigma
      , IaR ~gamma_a
      , IpIm ~mu*gamma_p
      , IpIs ~(1-mu)*gamma_p
      , ImR ~gamma_m
      , IsH ~(1-nonhosp_mort)*phi1*gamma_s
      , IsICUs ~(1-nonhosp_mort)*(1-phi1)*(1-phi2)*gamma_s
      , IsICUd ~(1-nonhosp_mort)*(1-phi1)*phi2*gamma_s
      , IsD ~nonhosp_mort*gamma_s
      , ICUsH2 ~psi1
      , ICUdD ~psi2
      , H2R ~psi3
      , HR ~rho
      , foi ~ state[2, 0] * beta0 * Ca / N  +
        state[3, 0] * beta0 * Cp / N +
        state[4, 0] * beta0 * Cm / N * (1 - iso_m) +
        state[5, 0] * beta0 * Cs / N  * (1 - iso_s)
      , rate ~ c(foi, EIa, EIp, IaR, IpIm, IpIs, ImR, IsICUs, IsICUd, IsH, IsD, ICUsH2, ICUdD, HR, H2R)
      , flow ~ state[from, 0]*rate
      , state ~ state - groupSums(flow, from, 12) + groupSums(flow, to, 12)
      )
  )
    , params = OptParamsList(2/3, 1, 1, 1
                             , par_id = 0:3
                             , mat = c("Ca", "Cp", "Cm", "Cs")
                             , row_id = rep(0L, 4L)
                             , col_id = rep(0L, 4L)
      )
    , random = OptParamsList()
    , obj_fn = ObjectiveFunction(~0)
    , time_steps = Time(100L)
  )
s = TMBSimulator(m, "dev")
s$report(2/3, 1, 1, 1)
