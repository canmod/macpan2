library(macpan2)

initialize_state = list(
      X ~ 100
    , Y ~ 100
)

flow_rates = list(
    ## growth rate of species X and Y
    growth_x ~ rx * X
  , growth_y ~ ry * Y
    ## intraspecific effect
    ## species X on X
  , intraspecific_x ~ growth_x * axx * X
    ## species Y on Y
  , intraspecific_y ~ growth_y * ayy * Y
    ## interspecific effect
    ## species Y on X
  , interspecific_xy ~ growth_x * axy * Y
    ## species X on Y
  , interspecific_yx ~ growth_y * ayx * X
)

state_updates = list(
    X ~ X + growth_x - intraspecific_x - interspecific_xy
  , Y ~ Y + growth_y - intraspecific_y - interspecific_yx
)

default = list(
    rx = 0.5
  , ry = 0.5
  , axx = 1/200   # axx = 1/Kx, Kx = carrying capacity of X
  , ayy = 1/50    # ayy = 1/Ky, Ky = carrying capacity of Y
  , axy = 0.8/50  # axy = alpha_xy/Ky, alpha_xy=competition coeff of Y on X, Ky = carrying capacity of Y
  , ayx = 1.5/200 # axy = alpha_yx/Kx, alpha_yx=competition coeff of X on Y, Kx = carrying capacity of X
)

## model spec
spec =  mp_tmb_model_spec(
    before = initialize_state
  , during = c(flow_rates, state_updates)
  , default = default
)

