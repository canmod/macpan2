library(macpan2)

# parameter names
param_names = c("beta","R_initial")

## -------------------------
## vector format of parameter values
## -------------------------

# scenarios to consider:
# 1. theta is a numeric vector
theta = c(0.2, 50)

# 2. theta is a named numeric vector
#theta = c("beta"=0.2, "R_initial"=0)

# 3. theta is a numeric data frame with names
# row vector
#theta = data.frame("beta"=0.2, "R_initial"=0)

# 4. theta is a numeric data frame without names
# col vector
#theta = data.frame(c(0.2,0)) |> unname() 

initialize_state = list(
    R ~ theta[R_initial_ind]
  , S ~ N - I - R
)

flow_rates = list(
    infection ~ S * I * theta[beta_ind] / N
  , recovery ~ gamma * I
)

update_state = list(
    S ~ S - infection
  , I ~ I + infection - recovery
  , R ~ R + recovery
)

## set defaults
default = list(
    theta = theta
  , gamma = 0.1
  , N = 100
  , I = 1
)

integers = list(
    R_initial_ind = mp_positions("R_initial", param_names)
  , beta_ind = mp_positions("beta", param_names)
)

## model specification
spec = mp_tmb_model_spec(
    before = initialize_state
  , during = c(flow_rates, update_state)
  , default = default
  , integers = integers
)

