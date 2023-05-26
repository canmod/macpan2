library(macpan2)

# Step 1 -- read the model definition into R
m = Compartmental(file.path("inst", "starter_models", "macpan_base"))


# Step 2 -- get labels of state, flow, and other variables
zero_vector = function(nms) setNames(rep(0, length(nms)), nms)
m$labels$state()
m$labels$flow()
m$labels$other()

state = zero_vector(m$labels$state())
state["S"] = 99; state["Is"] = 1
flow = zero_vector(m$labels$flow())
flow["SE"] = 0.1

## Step 3 --
s = m$simulators$tmb(time_steps = 100L
  , state = state
  , flow = flow
  , alpha = 0
  , beta0 = 0.2
  , sigma = 0
  , mu = 0
  , gamma_a = 0
  , gamma_p = 0
  , gamma_m = 0
  , gamma_s = 0
  , nonhosp_mort = 0
  , phi1 = 0
  , phi2 = 0
  , psi1 = 0
  , psi2 = 0
  , psi3 = 0
  , rho = 0
  , Ca = 0
  , Cp = 0
  , Cm = 0
  , Cs = 0.1
  , iso_m = 0
  , iso_s = 0
  , N = 0
  ### , .mats_to_return = c("state", "total_inflow")
)

s$report()
