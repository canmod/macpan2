source("inst/starter_models/nfds/tmb.R")
library(macpan2)
library(ggplot2)
library(dplyr)

## -------------------------
## get model spec from library
## -------------------------

#spec = mp_tmb_library("starter_models", "nfds", package = "macpan2")
spec

## -------------------------
## define simulator
## -------------------------

# set number of time steps in simulation
# believe time_steps = 120L is equal to 10 years (as in paper)
time_steps = 2L 

# simulator object
# slow (~10s per time_step ...)
start_time <- Sys.time()
nfds = mp_simulator(  
  model = spec
  , time_steps = time_steps
  , outputs = c("Y")
) 
end_time <- Sys.time()
end_time-start_time


## -------------------------
## simulating dynamics
## -------------------------

# we expect that prevalences of all genotypes should
# reach equilibirium prevalences (level off) with sufficient time steps
(nfds
  |> mp_trajectory()
  |> mutate(genotype=as.factor(row))
  |> ggplot()
  + geom_line(aes(time,value,col=genotype))
  + theme_bw()
  + theme(legend.position="none")
  + ylab("Prevalence")
)

# if genotype i is in the vaccine >0, else 0
# we expect seroreplacement to take place, genotypes not in vaccine dominate population after
# vaccine perturbation (with sufficient time steps)
(nfds
  |> mp_trajectory()
  |> mutate(in_vax = as.factor(rep(vax_strain,time_steps)))
  |> group_by(time,in_vax)
  |> mutate(value = sum(value))
  |> ungroup()
  |> ggplot()
  + geom_line(aes(time,value,col=in_vax))
  + theme_bw()
  + ylab("Prevalence")
) 

## -------------------------
## specify objective function
## -------------------------


## -------------------------
## parameterize model
## -------------------------