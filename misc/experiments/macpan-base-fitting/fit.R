
library(macpan2)
source('./misc/experiments/macpan-base-fitting/data.R')


## -------------------------
## format data
## -------------------------


# get data in the right format for calibration
formatted_tsdata = (clean_tsdata
                    |> rename(matrix=var)
                    # dates from base model calibration (Figure 4)
                    |> filter(date >= "2020-02-24" & date < "2020-08-31")
                    |> arrange(date)
                    |> group_by(date)
                    |> mutate(time = cur_group_id())
                    |> ungroup()
                    |> filter(matrix == "death" )
                    |> mutate(matrix = "ICUd.D")
                    )

# get time steps for specific dates
# there are two additional breaks (not specified in the paper?)
# see ML comment Figure 3
break_times = (formatted_tsdata
 |> filter(date %in% c("2020-02-24", "2020-04-01", "2020-08-07"))
 |> select(time,date)
)

beta_changepoints = break_times$time
beta_values = rep(log(1e-2),length(beta_changepoints))

# remove synonym (can't have both 'date' and 'time')
formatted_tsdata = formatted_tsdata |> select(-c(date))

## -------------------------
## update model spec (base model)
## -------------------------

## things we need to incorporate
# calibrates to new confirmation and death time series
#   "new confirmation" = daily positive tests (said earlier) conflicting terminology I think
# using "new confirmation" as "new cases = case reports" because base model has no testing structure
# need to do convolution to compute case reports from incidence
# mobility with two additional breaks
# phenomenological heterogeneity

macpan_base = (mp_tmb_library("starter_models","macpan_base",package="macpan2")
  # add accumulator variables:
  # H2 - individuals in acute care after discharge (already computed)
  # D - cumulative deaths (already computed)
  # X - cumulative hospital admissions
 |> mp_tmb_insert(phase = "during", at = Inf,
                  expressions = list(mp_per_capita_flow("H", "X", H.X ~ Is.ICUs + Is.ICUd)),
                  default = list(X = 0))

 # add phenomenological heterogeneity:
 |> mp_tmb_update(phase = "during", at =1L, 
                  expressions = list(mp_per_capita_flow("S", "E", S.E ~ (S^(zeta - 1)) * (beta0 / (N^zeta)) * (Ia * Ca + Ip * Cp + Im * Cm * (1 - iso_m) + Is * Cs *(1 - iso_s)))), 
                  default = list(zeta = 0))
 

)

(macpan_base
  %>% mp_simulator(time_steps = 10, outputs = "X")
  %>% mp_trajectory()
  %>% ggplot(aes(time,value))+
    geom_point()
)

## -------------------------
## calibration (base model)
## -------------------------

# fitting to deaths and new cases (incidence)  
# - need to compute case reports with convolution first
mb_calib = mp_tmb_calibrator(
    spec = macpan_base |> mp_rk4()
  , data = formatted_tsdata
  , traj = "ICUd.D" 
  , par = c("E","beta0") ##more in table 1
)

mp_optimize(mb_calib)

# get fitted data
fitted_data = mp_trajectory_sd(mb_calib, conf.int = TRUE)

# check estimate
mp_tmb_coef(mb_calib) %>% mutate(estimate = exp(estimate))


if (interactive()) {
  (ggplot(sim_data, aes(time,value))
   + geom_point()
   + geom_line(aes(time, value)
               , data = fitted_data
               , colour = "red"
   )
   + geom_ribbon(aes(time, ymin = conf.low, ymax = conf.high)
                 , data = fitted_data
                 , alpha = 0.2
                 , colour = "red"
   )
   + facet_wrap(vars(matrix),scales = 'free')
   + theme_bw()
   + ylab("prevalence")
  )
}




# time variation in beta
# two piecewise breaks for tranmission rate on April 1 and August 7
# two piecewise breaks for mobility power on  April 1 and August 7
#Ca, Cp, Cm, Cs these are the transmission rates that would vary with time, would beta_1 be the sum of these four
## |> mp_tmb_update(phase = "before", at = -Inf,
#                  expressions = list(
#                    Ca ~ exp(log_Ca)
#                    , Cp ~ exp(log_Cp)
#                    , Cm ~ exp(log_Cm)
#                    , Cs ~ exp(log_Cs)
#                    , beta0 ~ exp(log_beta0)
#                    , beta ~ beta0 + Ca + Cp + Cm + Cs
#                  ),
#                  default = list(
#                    log_beta0 = log(1e-2)
#                    , log_Ca = log(1e-2)
#                    , log_Cp = log(1e-2)
#                    , log_Cm = log(1e-2)
#                    , log_Cs = log(1e-2)
#                  ))
## |> mp_tmb_insert(phase = "during", at = 1,
#                  expressions = list(beta ~ time_var(beta_values, beta_changepoints)),
#                  default = list(beta_changepoints=beta_changepoints,beta_values=beta_values ))