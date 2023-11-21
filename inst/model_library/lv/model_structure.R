# model equations
# dX/dt = alpha*X - beta*Y*X = alpha*X - r1*X
# dY/dt = gamma*X*Y - delta*Y = r2*Y - delta*Y

# fx(X, Y) = X + alpha*X - beta*Y*X
# fy(X, Y) = Y + gamma*X*Y - delta*Y

# jacobian
# 1 + alpha - beta*Y,     -beta*X
#            gamma*Y, 1 + gamma*X - delta

# equilibrium
# Y = alpha / beta
# X = delta / gamma

# jacobian at equilibrium
# 1                    , -beta * delta / gamma
# gamma * alpha / beta , 1


## index tables (model quantities)

state = mp_index(
  Eco = c("X", "Y")
)

rate = mp_index(
  Eco = c(
    "alpha", "beta", "gamma", "delta", # from model equations
    "r1", "r2" # derived, state-dependent rates
  )
)

## ledgers

# interactions

interaction_prey = mp_join(
  interaction_flow_rate = mp_subset(rate, Eco = "r1"),
  interaction_rate = mp_subset(rate, Eco = "beta"),
  interaction_state = mp_subset(state, Eco = "Y")
)

interaction_predator = mp_join(
  interaction_flow_rate = mp_subset(rate, Eco = "r2"),
  interaction_rate = mp_subset(rate, Eco = "gamma"),
  interaction_state = mp_subset(state, Eco = "X")
)

# outflows
# join by none option?
outflows = list(
  mp_join(
    outflow_rate = mp_subset(rate, Eco = "r1"),
    outflow_flow_state = mp_subset(state, Eco = "X")
  ),
  mp_join(
    outflow_rate = mp_subset(rate, Eco = "delta"),
    outflow_flow_state = mp_subset(state, Eco = "Y")
  )
)

inflows = list(
  mp_join(
    inflow_rate = mp_subset(rate, Eco = "alpha"),
    inflow_flow_state = mp_subset(state, Eco = "X")
  ),
  mp_join(
    inflow_rate = mp_subset(rate, Eco = "r2"),
    inflow_flow_state = mp_subset(state, Eco = "Y")
  )
)

# archetypes
# interaction terms: -beta*X*Y, gamma*X*Y
# flow terms: alpha*X, -delta*Y

## Set up expressions list for each functional form --------------
## names refer to when the calculation gets performed relative to
## the simulation time-step loop (before, during, ...)
expr_list <- mp_expr_list(
  during = list(
    # calculate interaction flow rates
    rate[interaction_flow_rate] ~ rate[interaction_rate]*state[interaction_state]

    # individual outflows and inflows in order to add sign
  , outflows ~ rate[outflow_rate]*state[outflow_flow_state]
  , inflows ~ rate[inflow_rate]*state[inflow_flow_state]

    # sum flow for each "to" state
  , net_flow ~ (
      + groupSums(inflows, inflow_flow_state, state)
      - groupSums(outflows, outflow_flow_state, state)
    )
  , state ~ state + net_flow
  )
)

## Ledgers for each specific calculation --------------
ledgers <- list(
  interaction = mp_ledgers(interaction_prey, interaction_predator),
  outflows = mp_ledgers(outflows),
  inflows = mp_ledgers(inflows)
)

## Initialize vectors from index tables (with all zeros for values) --------------
# used as placeholders for user input
init_vecs <- list(
  state = mp_vector(state),
  rate = mp_vector(rate)
)

## Initialize model object -----------------
lv = mp_dynamic_model(
  expr_list = expr_list,
  ledgers = ledgers,
  init_vecs = init_vecs
)
