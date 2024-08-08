library(macpan2)
library(ggplot2)
library(dplyr)
options(macpan2_default_loss = "neg_bin") 

## -------------------------
## Model Specification
## -------------------------

source("./inst/starter_models/ww/tmb.R")
#specs = mp_tmb_library("starter_models", "ww", alternative_specs = TRUE, package = "macpan2")

# Update model specification to include additional components described here:
# https://github.com/canmod/macpan2/issues/156
focal_model = (specs$ww_euler
               #specs$ww_hazard
               
               # add variable transformations:
               |> mp_tmb_insert(phase = "before"
                                , at = 1L
                                , expressions = list(
                                    zeta ~ exp(log_zeta)
                                  , beta0 ~ exp(log_beta0)
                                  , nonhosp_mort ~ 1/(1+exp((-logit_nonhosp_mort)))
                                  , E ~ exp(log_E)
                                  , Ca ~ exp(log_Ca)
                                )
                                , default = list(
                                    log_zeta = empty_matrix
                                  , log_beta0 = empty_matrix
                                  , logit_nonhosp_mort = empty_matrix
                                  , log_E = empty_matrix
                                  , log_Ca = empty_matrix
                                )
                                
               )
               
               # add accumulator variables:
               # death - new deaths each time step
               |> mp_tmb_insert(phase = "during"
                                , at = Inf
                                , expressions = list(death ~ ICUd.D + Is.D)
               )
               
               # add phenomenological heterogeneity:
               # might need to modify if we add time-varying transmission
               |> mp_tmb_update(phase = "during"
                                , at =1L
                                , expressions = list(mp_per_capita_flow("S", "E", S.E ~ ((S/N)^zeta) * (beta0 / N) * (Ia * Ca + Ip * Cp + Im * Cm * (1 - iso_m) + Is * Cs *(1 - iso_s))))
                                
               )
               
               # compute gamma-density delay kernel for convolution:
               |> mp_tmb_insert(phase = "before"
                                , at = Inf
                                , expressions = list(
                                    gamma_shape ~ 1 / (c_delay_cv^2)
                                  , gamma_scale ~ c_delay_mean * c_delay_cv^2
                                  , gamma ~ pgamma(1:(qmax+1), gamma_shape, gamma_scale)
                                  , delta ~ gamma[1:(qmax)] - gamma[0:(qmax - 1)]
                                  , kappa ~ c_prop * delta / sum(delta)
                                )
                                , default = list(qmax = empty_matrix)
               )
               
               # add convolution to compute case reports from incidence:
               |> mp_tmb_insert(phase = "during"
                                , at = Inf
                                , expressions = list(report ~ convolution(S.E, kappa))
               )
)

## -------------------------
## Data Prep
## -------------------------

# For now, using simulated data to calibrate.

# set number of time steps in simulation
time_steps = 100L

# width of report convolution kernel computed according to:
# https://canmod.net/misc/flex_specs#computing-convolutions
shape = 1/(mp_default_list(focal_model)$c_delay_cv^2)
scale = mp_default_list(focal_model)$c_delay_mean * mp_default_list(focal_model)$c_delay_cv^2
qmax = ceiling(qgamma(0.95, shape, scale))

# simulated data
sim_data = (focal_model 
  |> mp_simulator(
      time_steps = time_steps
    , outputs = c("report","death", "W", "A")
    , default = list(
        S = 14.57e6 - 5 
      , log_E = log(5)
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
      , log_beta0=log(5)
      , logit_nonhosp_mort = -0.5
      , log_zeta = 1
      , qmax = qmax
      , log_Ca = log(2/3)
      )
  )
  |> mp_trajectory()
)

## -------------------------
## calibration
## -------------------------

focal_calib = mp_tmb_calibrator(
    spec = focal_model
  , data = sim_data
  , traj = c("report","death", "W", "A") 
  , par = c(
      "log_Ca"
    # negative binomial dispersion parameters for reports and deaths get added
    # automatically with options(macpan2_default_loss = "neg_bin") set above
  )
  , default = list(
    
    # states
    # Population of Ontario (2019) from:
    # https://github.com/mac-theobio/macpan_base/blob/main/code/ontario_calibrate_comb.R
      S = 14.57e6 - 5 
    , log_E = log(5)
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
    
    # set initial parameter values for optimizer
    , log_beta0=log(5)
    , logit_nonhosp_mort = -0.5
    , qmax = qmax
    , log_zeta = 1
    , log_Ca = log(1/3)
  )
)
# converges
mp_optimize(focal_calib)

# Get fitted data
fitted_data = mp_trajectory_sd(focal_calib, conf.int = TRUE)

# parameter estimates
mp_tmb_coef(focal_calib, conf.int = TRUE)

# back transform Ca estimate
# estimate is fairly off, perhaps trajectories are not informative enough
# use Ia instead of reports?
(mp_tmb_coef(focal_calib, conf.int = TRUE)[1,]
  |> mutate(mat = "Ca")
  |> mutate_if(is.numeric,exp)
)

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
  )
}

## -------------------------
## previous implementation
## -------------------------

# component = "during"
# identical(
#     specs$explicit$expand()[[component]]
#   , specs$implicit[[component]]
# )
# mp_hazard(specs$explicit)$expand()
# char_expl = sort(vapply(specs$explicit$expand()[[component]], macpan2:::formula_as_character, character(1L)))
# char_impl = sort(vapply(specs$implicit[[component]], macpan2:::formula_as_character, character(1L)))
# setdiff(char_expl, char_impl)
# setdiff(char_impl, char_expl)
# intersect(char_expl, char_impl)
# for (i in 1:33) {
#   print(i)
#   print(char_expl[i])
#   print(char_impl[i])
# }
# char_expl[[34]]
# char_impl[[34]]


## -------------------------
## exploring
## -------------------------
ww_exploring = mp_simulator(
    model = specs$ww_euler
  , time_steps = time_steps
  , outputs = c("Ia", "Ip", "Im", "Is", "W", "A")
) |> mp_trajectory()

## all infectious compartments
if (interactive()) {
  ggplot(ww_exploring %>% filter(grepl("I",matrix))%>% select(time,value,matrix), aes(time,value,col=matrix))+
    geom_line()+
    theme_bw()+
    ylab("individuals")
}

## W
if (interactive()) {
  ggplot(ww_exploring %>% filter(matrix=="W")%>% select(time,value), aes(time,value))+
    geom_line()+
    theme_bw()+
    ylab("W")
}

## A
if (interactive()) {
  ggplot(ww_exploring %>% filter(matrix=="A")%>% select(time,value), aes(time,value))+
    geom_line()+
    theme_bw()+
    ylab("A")
}
