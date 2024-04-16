
library(macpan2)
source('./misc/experiments/macpan-base-fitting/data.R')


## -------------------------
## format data
## -------------------------


# get data in the right format for calibration
formatted_tsdata = (clean_tsdata
                    |> rename(matrix=var)
                    |> filter(date >= "2020-02-24" & date < "2020-08-31")
                    |> arrange(date)
                    |> group_by(date)
                    |> mutate(time = cur_group_id())
                    |> ungroup()
                    |> filter(matrix == "death" )
                    |> mutate(matrix = "ICUd.D")
                    # remove synonym (can't have both 'date' and 'time')
                    |> select(-c(date))
                    )

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
                  expressions = list(mp_per_capita_flow("S", "E", S.E ~ ((S/N)^zeta) * (beta0 / N) * (Ia * Ca + Ip * Cp + Im * Cm * (1 - iso_m) + Is * Cs *(1 - iso_s)))), 
                  default = list(zeta = 1))

 
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
    spec = mp_tmb_library("starter_models","macpan_base",package="macpan2")
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