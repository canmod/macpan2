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
                    |> filter(matrix %in% c("death","report"))
                    |> mutate(matrix = if_else(matrix == "death","ICUd.D",matrix))
                    )

# get time steps for specific dates
# there are two additional breaks (not specified in the paper?)
# see ML comment Figure 3
break_times = (formatted_tsdata
 |> filter(date %in% c("2020-02-24", "2020-04-01", "2020-08-07"))
 |> select(time,date)
 |> unique()
)

beta_changepoints = break_times$time %>% as.integer()
beta_values = rep(log(1e-2),length(beta_changepoints))

# remove synonym (can't have both 'date' and 'time')
formatted_tsdata = formatted_tsdata |> select(-c(date))

# one negative value for daily deaths
# removing for now
formatted_tsdata = formatted_tsdata |> filter(value>=0)

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


# kernel
# should be Gamma distribution with moments 
# chosen to match empirical estimates of case-reporting delays (where is this info)
k=c(0.5, 0.25, 0.25)


macpan_base = (mp_tmb_library("starter_models","macpan_base",package="macpan2")
  
   # add variable transformations:
  |> mp_tmb_insert(phase = "before", at = 1L,
                   expressions = list(
                       beta0 ~ exp(log_beta0)
                     , zeta ~ exp(log_zeta)
                     , beta ~ exp(log_beta)
                   ),
                   default = list(
                       log_beta0 = log(1e-2)
                     , log_zeta = log(1e-2)
                     , log_beta = log(1e-2)
                  )
  )
               
  # add accumulator variables:
  # H2 - individuals in acute care after discharge (already computed)
  # D - cumulative deaths (already computed)
  # X - cumulative hospital admissions
 |> mp_tmb_insert(phase = "during", at = Inf,
                  expressions = list(mp_per_capita_flow("H", "X", H.X ~ Is.ICUs + Is.ICUd)),
                  default = list(X = 0))

 # add phenomenological heterogeneity:
 # is beta1 = beta, 
 # or beta1 = (Ia * Ca + Ip * Cp + Im * Cm * (1 - iso_m) + Is * Cs *(1 - iso_s))?
 |> mp_tmb_update(phase = "during", at =1L, 
                  expressions = list(
                    mp_per_capita_flow("S", "E", S.E ~ (S^(zeta - 1)) * (beta0 / (N^zeta)) * beta * (Ia * Ca + Ip * Cp + Im * Cm * (1 - iso_m) + Is * Cs *(1 - iso_s)))), 
                  default = list(zeta = 0))
 
 # add condensation variables:
 # I = sum of all infectious compartments
 # might not need this
 ## |> mp_tmb_insert(phase = "during"
 #                  , at = Inf
 #                  , expressions = list(I ~ Ia + Ip + Im + Is)
 #                  , default = list(I=0))
 
 # add convolution to compute case reports:
 |> mp_tmb_insert(phase = "during"
                  , at = Inf
                  , expressions = list(report ~ c_prop * convolution(S.E, k=k))
                  , default = list(k=k))
 
 # add piecewise time-varying transmission:
 ## |> mp_tmb_insert(phase = "during", at = 1,
 #      expressions = list(beta ~ time_var(beta_values, beta_changepoints)),
 #      default = list(beta_values=beta_values),
 #      integers = list(beta_changepoints = beta_changepoints))
)

(macpan_base
  %>% mp_simulator(time_steps = 10, outputs = c("Ia","Ip","Im","report"))
  %>% mp_trajectory()
  %>% ggplot(aes(time,value))+
    geom_point()+
    facet_wrap(vars(matrix),scales = 'free')
)

## -------------------------
## calibration (base model)
## -------------------------

# fitting to deaths and reports
mb_calib = mp_tmb_calibrator(
    spec = macpan_base |> mp_rk4()
  , data = formatted_tsdata
  , traj = c("report","ICUd.D") 
  , par = c("E","log_beta0","log_zeta", "log_beta") ##more in table 1
)

mp_optimize(mb_calib)

# get fitted data
fitted_data = mp_trajectory_sd(mb_calib, conf.int = TRUE)

# check estimate
mp_tmb_coef(mb_calib) %>% mutate(estimate = exp(estimate))


(ggplot(formatted_tsdata, aes(time,value))
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

