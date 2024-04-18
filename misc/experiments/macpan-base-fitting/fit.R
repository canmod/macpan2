library(macpan2)
source('./misc/experiments/macpan-base-fitting/data.R')

## -------------------------
## local function
## -------------------------
# to be included in mp_tmb_coef in the future
# see here, https://github.com/canmod/macpan2/issues/179
backtrans <- function(x) {
  vars1 <- intersect(c("default", "estimate", "conf.low", "conf.high"), names(x))
  prefix <- stringr::str_extract(x[["mat"]], "^log(it)?_")  |> tidyr::replace_na("none")
  sx <- split(x, prefix)
  for (ptype in setdiff(names(sx), "none")) {
    link <- make.link(stringr::str_remove(ptype, "_"))
    sx[[ptype]] <- (sx[[ptype]]
                    |> mutate(across(std.error, ~link$mu.eta(estimate)*.))
                    |> mutate(across(any_of(vars1), link$linkinv))
                    |> mutate(across(mat, ~stringr::str_remove(., paste0("^", ptype))))
    )
  }
  bind_rows(sx)
}


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
# see ML comment Figure 3.
break_times = (formatted_tsdata
 |> filter(date %in% c("2020-04-01", "2020-08-07"))
 |> select(time,date)
 |> unique()
)

# time-varying transmission change points
# time_var needs change_points to start at 0 (for now)
beta_changepoints = c(0,break_times$time)


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

time_steps = 200

# matrix indicing trick for group sums to compute
# relative transmission rate
X = diag(length(beta_values))
X[lower.tri(X)]<-1
obj = macpan2:::sparse_matrix_notation(X, FALSE)
base_values = obj$values
row_ind = obj$row_index-1 #subtract to make zero-based
col_ind = obj$col_index-1

# kernel, for convolution
# should be Gamma distribution with moments 
# chosen to match empirical estimates of case-reporting delays (where is this info)
k=c(0.5, 0.25, 0.25)

# --------------- model spec ---------------
macpan_base = (mp_tmb_library("starter_models","macpan_base",package="macpan2")
  
  # add variable transformations:
  |> mp_tmb_insert(phase = "before"
                   , at = 1L,
                   expressions = list(
                       zeta ~ exp(log_zeta)
                     , beta ~ exp(log_beta)
                     , beta_values ~ exp(log_beta_values)
                    ),
                   default = list(
                       log_zeta = log(1e-2)
                     , log_beta = log(1e-2)
                     , log_beta_values = log_beta_values
                    ) 
                   )
               
  # add accumulator variables:
  # H2 - individuals in acute care after discharge (already computed)
  # D - cumulative deaths (already computed)
  # X - cumulative hospital admissions
  |> mp_tmb_insert(phase = "during"
                   , at = Inf
                   , expressions = list(mp_per_capita_flow("H", "X", H.X ~ Is.ICUs + Is.ICUd))
                   , default = list(X = 0)
                   )

  # add phenomenological heterogeneity:
  |> mp_tmb_update(phase = "during"
                   , at =1L
                   , expressions = list(mp_per_capita_flow("S", "E", S.E ~ (S^(zeta - 1)) * (beta / (N^zeta)) * (Ia * Ca + Ip * Cp + Im * Cm * (1 - iso_m) + Is * Cs *(1 - iso_s))))
                   , default = list(zeta = 0)
                   )
 
  # add condensation variables:
  # I = sum of all infectious compartments (time step incidence)
  |> mp_tmb_insert(phase = "during"
                   , at = Inf
                   , expressions = list(I ~ Ia + Ip + Im + Is)
                   , default = list(I=0))
 
  # add convolution to compute case reports:
  |> mp_tmb_insert(phase = "during"
                  , at = Inf
                  , expressions = list(report ~ c_prop * convolution(I, k))
                  , default = list(k=k))
  |> mp_tmb_insert(phase = "after"
                  , at = 1L
                  # remove beginning time steps depending on kernel length
                  , expressions = list(report ~ rbind_time(report, i))
                  , integers = list(i = length(k):time_steps)
                  )

  # add piecewise time-varying transmission:
  |> mp_tmb_insert(phase = "before"
                  , at = Inf
                  , expressions = list(beta_values ~ group_sums((beta_values[row_ind]) * base_values, col_ind, beta_values))
                  , default = list(base_values = base_values)
                  , integers = list(row_ind = row_ind, col_ind=col_ind)
                  )
  |> mp_tmb_insert(phase = "during"
                  , at = 1L
                  , expressions = list(beta ~ time_var(beta_values, beta_changepoints))
                  , integers = list(beta_changepoints = beta_changepoints)
                  )
)
# ------------------------------------------

## simulate data
(macpan_base
  %>% mp_simulator(time_steps = time_steps, outputs = c("Ia","Ip","Im","report"))
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
  , par = c("E","log_zeta", "log_beta_values") ##more in table 1
)

mp_optimize(mb_calib)

# get fitted data
fitted_data = mp_trajectory_sd(mb_calib, conf.int = TRUE)

# check estimate
mp_tmb_coef(mb_calib, conf.int = TRUE) |> backtrans()


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

