## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "./figures/",
  message = FALSE,
  warning = FALSE
)
round_coef_tab = function(x, digits = 4) {
  id_cols = c("term", "mat", "row", "col", "type")
  num_cols = setdiff(names(x), id_cols)
  for (col in num_cols) {
    x[[col]] = round(x[[col]], digits)
  }
  rownames(x) = NULL
  cols_to_drop = c("term", "col", "type")
  x = x[, setdiff(names(x), cols_to_drop), drop = FALSE]
  x
}
system.file("utils", "box-drawing.R", package = "macpan2") |> source()


## ----packages, warning=FALSE, message=FALSE-----------------------------------
library(macpan2)
library(ggplot2)
library(dplyr)


## ----options------------------------------------------------------------------
options(macpan2_verbose = FALSE)


## ----model_lib----------------------------------------------------------------
spec = mp_tmb_library(
    "starter_models"
  , "macpan_base"
  , package = "macpan2"
)


## ----diagram, echo = FALSE, fig.height = 5, fig.width = 8---------------------
system.file("utils", "box-drawing.R", package = "macpan2") |> source()
layout = mp_layout_paths(spec, x_gap = 0.1, y_gap = 0.1)
plot_flow_diagram(layout)


## ----get_data-----------------------------------------------------------------
ts_data  = ("https://github.com/canmod/macpan2"
  |> file.path("releases/download/macpan1.5_data/covid_on.RDS")
  |> url()
  |> readRDS()
)


## ----prep_ts------------------------------------------------------------------
prepped_ts_data = (ts_data
  |> rename(matrix = var)
  # dates from base model calibration (Figure 4)
  |> filter(date >= "2020-02-24" & date < "2020-08-31")
  # create unique time identifier
  |> arrange(date)
  |> group_by(date)
  |> mutate(time = cur_group_id())
  |> ungroup()
  # one negative value for daily deaths (removing for now time==178)
  # this explains negative values:
  # https://github.com/ccodwg/Covid19Canada?tab=readme-ov-file#datasets
  |> filter(matrix %in% c("death","report") & value >= 0)
)


## ----prep_mob-----------------------------------------------------------------
prepped_mobility_data = (ts_data
  |> filter(var == "mobility_index")
  # dates from base model calibration (Figure 4)
  |> filter(date >= "2020-02-24" & date < "2020-08-31")
  # create unique time identifier
  |> arrange(date)
  |> group_by(date)
  |> mutate(time = cur_group_id())
  |> ungroup()
  |> mutate(log_mobility_ind = log(value))
)


## ----mob_breaks---------------------------------------------------------------
mobility_breaks = (prepped_ts_data
  |> filter(date %in% c("2020-04-01", "2020-08-07"), matrix == "report")
  |> pull(time)
)


## ----time_steps---------------------------------------------------------------
time_steps = nrow(prepped_ts_data %>% select(date) %>% unique())
prepped_ts_data = select(prepped_ts_data, -date)


## ----mobility_matrix----------------------------------------------------------
S_j = function(t, tau_j, s) 1/(1 + exp((t - tau_j) / s))
X = cbind(
      prepped_mobility_data$log_mobility_ind
    , S_j(1:time_steps, mobility_breaks[1], 3)
    , S_j(1:time_steps, mobility_breaks[1], 3) * prepped_mobility_data$log_mobility_ind
    , S_j(1:time_steps, mobility_breaks[2], 3)
    , S_j(1:time_steps, mobility_breaks[2], 3) * prepped_mobility_data$log_mobility_ind
    ) %>% as.matrix()
X_sparse = macpan2:::sparse_matrix_notation(X, TRUE)
model_matrix_values = X_sparse$values
row_ind = X_sparse$row_index
col_ind = X_sparse$col_index


## ----focal_model--------------------------------------------------------------
focal_model = (spec 
   # add variable transformations:
   |> mp_tmb_insert(phase = "before"
      , at = 1L
      , expressions = list(
          zeta ~ exp(log_zeta)
        , beta0 ~ exp(log_beta0)
        , nonhosp_mort ~ 1/(1+exp((-logit_nonhosp_mort)))
        , E ~ exp(log_E)
      )
      , default = list(
          log_zeta = empty_matrix
        , log_beta0 = empty_matrix
        , logit_nonhosp_mort = empty_matrix
        , log_E = empty_matrix
      )

   )
   
   # add accumulator variables:
   # death - new deaths each time step
   |> mp_tmb_insert(phase = "during"
      , at = Inf
      , expressions = list(death ~ ICUd.D + Is.D)
   )
   
   # add phenomenological heterogeneity:
   |> mp_tmb_update(phase = "during"
      , at = 1L
      , expressions = list(
        mp_per_capita_flow(
            "S", "E"
          , "((S/N)^zeta) * (beta / N) * (Ia * Ca + Ip * Cp + Im * Cm * (1 - iso_m) + Is * Cs *(1 - iso_s))"
          , "S.E"
        ))

   )
   
   # compute gamma-density delay kernel for convolution:
   |> mp_tmb_insert_reports("S.E"
      , report_prob = 0.1
      , mean_delay = 11
      , cv_delay = 0.25
      , reports_name = "report"
   )
   
   # add time-varying transmission with mobility data:
   |> mp_tmb_insert(phase = "before"
      , at = Inf
      , expressions = list(relative_beta_values ~ group_sums(model_matrix_values * log_mobility_coefficients[col_ind], row_ind, model_matrix_values))
      , default = list(log_mobility_coefficients = empty_matrix, model_matrix_values = empty_matrix)
      , integers = list(row_ind = row_ind, col_ind = col_ind)
   )
   |> mp_tmb_insert(phase = "during"
      , at = 1L
      , expressions = list(
        beta ~ exp(log_beta0 + relative_beta_values[time_step(1)])
      )
   )

)


## ----calibrator---------------------------------------------------------------
focal_calib = mp_tmb_calibrator(
    spec = focal_model |> mp_rk4()
  , data = prepped_ts_data
  , traj = c("report","death") 
  , par = c(
      "log_zeta"
    , "log_beta0"
    , "logit_nonhosp_mort"
    , "log_mobility_coefficients"
    , "log_E" 
    # negative binomial dispersion parameters for reports and deaths get added
    # automatically with options(macpan2_default_loss = "neg_bin") set above
  )
  , outputs = c("death","report")
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
    
    , model_matrix_values = model_matrix_values
    
    # set initial parameter values for optimizer
    
    , log_beta0 = log(5)
    , logit_nonhosp_mort = -0.5
    , log_mobility_coefficients = rep(0, 5)
    , log_zeta = 1
    
  )
)
# converges
mp_optimize(focal_calib)


## ----get_trajectory-----------------------------------------------------------
fitted_data = mp_trajectory_sd(focal_calib, conf.int = TRUE)


## ----coefficients-------------------------------------------------------------
mp_tmb_coef(focal_calib, conf.int = TRUE) |> round_coef_tab()


## ----plot_fit-----------------------------------------------------------------
(ggplot(prepped_ts_data, aes(time,value))
  + geom_point()
  + geom_line(aes(time, value)
              , data = fitted_data |> filter(matrix %in% c("death","report"))
              , colour = "red"
  )
  + geom_ribbon(aes(time, ymin = conf.low, ymax = conf.high)
                , data = fitted_data |> filter(matrix %in% c("death","report"))
                , alpha = 0.2
                , colour = "red"
  )
  + facet_wrap(vars(matrix),scales = 'free')
  + theme_bw()
)


## ----cohort_model-------------------------------------------------------------
cohort_model_ph = (
  focal_model
  %>% mp_tmb_update(phase="during"
                    , at = 2L
                    , expressions = list(S.E ~ (S^zeta) * (beta / (N^(zeta) * N_cohort)) * (Ia * Ca + Ip * Cp + Im * Cm * (1 - iso_m) + Is * Cs *(1 - iso_s)))
                    , default = list(S.E = 0))
)


## ----simulate_cohort----------------------------------------------------------
cohort_sim_ph = (mp_simulator(cohort_model_ph
                  , time_steps = 100L
                  , outputs = "S.E"
                  , default = list(
                      S = 14.57e6 - 1 
                    , log_E = log(1)
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
                    , N_cohort = 1
                    , model_matrix_values = model_matrix_values
                    , qmax = 34
                    , log_beta0=log(1)
                    , logit_nonhosp_mort = -0.5
                    , log_mobility_coefficients = rep(0,5)
                    , log_zeta = 1
                    )
  ) |> mp_trajectory()
) 


## ----R0-----------------------------------------------------------------------
R0_ph = sum(cohort_sim_ph$value)
print(R0_ph)


## ----cohort_no_ph-------------------------------------------------------------
cohort_model = (focal_model
  %>% mp_tmb_update(phase="during"
                    , at = 2L
                    , expressions = list(mp_per_capita_flow(
        "S", "E"
      , "(beta / N) * (Ia * Ca + Ip * Cp + Im * Cm * (1 - iso_m) + Is * Cs * (1 - iso_s))"
      , "S.E" 
    )
  ))
  %>% mp_tmb_insert(phase="during"
                    , at = Inf
                    , expressions = list(foi ~ (beta / N) * (Ia * Ca + Ip * Cp + Im * Cm * (1 - iso_m) + Is * Cs *(1 - iso_s)))
  )
)

cohort_sim = (mp_simulator(cohort_model
                           , time_steps = 100L
                           , outputs = "foi"
                           , default = list(
                               S = 0
                             , log_E = log(1)
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
                             , N = 1
                             , model_matrix_values = model_matrix_values
                             , qmax = 34
                             , log_beta0=log(1)
                             , logit_nonhosp_mort = -0.5
                             , log_mobility_coefficients = rep(0,5)
                             , log_zeta = 1) # not actually used
  ) |> mp_trajectory()
) 

R0 = sum(cohort_sim$value)
print(R0)


## ----check_R0-----------------------------------------------------------------
all.equal(R0_ph, R0)


## ----euler_lotka--------------------------------------------------------------
euler_lotka = function(r) sum(cohort_sim$value * exp(-r * cohort_sim$time)) - 1
uniroot(euler_lotka, c(0,10))

