## not done by any stretch

kernel = c(0.5, 0.25, 0.25)
1 %*% kernel[1]
1:2 %*% kernel[1:2]
1:3 %*% kernel
2:4 %*% kernel
simple_sims(
  list(
    x ~ x + 1,
    y ~ convolution(x, kernel)
  ),
  time_steps = 10,
  mats = list(x = 0, y = empty_matrix, kernel = kernel)
) |> macpan2:::filter(time != 0, time != 11, matrix == "y")

library(macpan2)
library(dplyr)
# kernel
k = c(0.5, 0.25, 0.25)
X = 1
Y = 0
expr = list(
    X ~ X + X/2
  , Y ~ convolution(X, k)
)
sim_data = simple_sims(
  iteration_exprs = expr
  , time_steps = 10
  , mats = list(X = X, Y = Y, k = k)
) |> arrange(matrix)
# manual computation
# i = 1 to 3 (up to 3 lags)
X_sim = (sim_data 
         %>% filter(matrix=="X")
         %>% mutate(
            lag_1 = lag(value,default = 0)
          , lag_2 = lag(value,2,default = 0)
          , lag_3 = lag(value,3,default = 0))
)
manual_convolution = k[1] * X_sim$value + k[2] * X_sim$lag_1 + k[3] * X_sim$lag_2
macpan2_convolution = sim_data %>% filter(matrix=="Y") %>% pull(value)
## view differences
cbind(manual_convolution, macpan2_convolution)


s = mp_tmb_model_spec(
    during = list(
        X ~ X + X/2
      , Y ~ convolution(X, k)
    )
  , after = list(Z ~ rbind_time(Y, i))
  , default = list(X = X, Y = Y, k = k)
  , integers = list(i = length(k):10)
  , must_save = c("X", "Y")
)
mp_simulator(s, time_steps = 10L, outputs = "Z") |> mp_final() |> cbind(manual_convolution[3:10])
mp_simulator(s, time_steps = 10L, outputs = "Y") |> mp_trajectory() |> cbind(manual_convolution)


# macpan_base delay kernel and convolution example
# https://canmod.net/misc/flex_specs#computing-convolutions
qmax = 4 # length of kernel
n = 6  # number of time steps
c_delay_cv = 0.25
c_delay_mean = 11
c_prop = 0.1
gamma_shape = 1 / (c_delay_cv^2)
gamma_scale = c_delay_mean * c_delay_cv^2
gamma = pgamma(1:(qmax+1), gamma_shape, gamma_scale)
delta = gamma[2:(qmax+1)] - gamma[1:(qmax)]
kappa = c_prop * delta / sum(delta)

sim_data = simple_sims(
    iteration_exprs = expr
  , time_steps = n
  , mats = list(X = X, Y = Y, k = kappa)
) |> arrange(matrix)

x = sim_data %>% filter(matrix=="X") %>% select(value) %>% pull()

macpan2_convolution = sim_data %>% filter(matrix=="Y" & time %in% c(4,5)) %>% pull(value)
# z calculation from https://canmod.net/misc/flex_specs#computing-convolutions
manual_convolution = c(kappa[4]*x[1]+kappa[3]*x[2]+kappa[2]*x[2]+kappa[1]*x[4]
                       , kappa[4]*x[2]+kappa[3]*x[3]+kappa[2]*x[4]+kappa[1]*x[5])

cbind(manual_convolution, macpan2_convolution)
