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
# monthly time steps
time_steps = 120L 

# simulator object
start_time <- Sys.time()
nfds = mp_simulator(  
    model = spec
  , time_steps = time_steps
  , outputs = c("Y","vax_strain")
) 
end_time <- Sys.time()
end_time-start_time



## -------------------------
## simulating dynamics
## -------------------------

# we expect that prevalences of all genotypes should
# reach equilibirium prevalences (level off) with sufficient time steps
# although Colijn et al. (2020) mentions curves may not reach mathematical
# equilibrium, but invasiveness equilibriates faster
(nfds
  |> mp_trajectory()
  |> filter(matrix=='Y')
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

in_vax = (nfds
          |> mp_trajectory()
          |> filter(matrix=='vax_strain')
          |> mutate(value=as.factor(value))
)

(nfds
  |> mp_trajectory()
  |> filter(matrix=='Y')
  |> mutate(in_vax = in_vax$value)
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

# 1. infant IPD
# evaluated after simulation loop once we have solution vector
# read in invasiveness scores (OR)

kids_invasiveness <- (read.csv(file.path("inst","starter_models","nfds","data","Invasiveness.kids.csv"), header = FALSE)
                      # why doesn't this work
                      # %>% as.matrix(dimnames = NULL)
                      %>% as.matrix()
)
# dimnames seem to cause issues, so removing
attr(kids_invasiveness, "dimnames") <- NULL 

# negative log likelihood
# obj_fn = ~ -sum(dpois(X_obs, rbind_time(X, X_obs_times)))

obj_fn = ~ (1/N) * (Y %*% exp(kids_invasiveness))


# update simulator to create new variables for invasiveness scores
nfds$update$matrices(
    kids_invasiveness = kids_invasiveness
  , vax_strain = empty_matrix  
)

# update simulator to include this function
nfds$replace$obj_fn(obj_fn)

# how is next vax_strain chosen

# 2. overall IPD

# 3. AMR (anti-microbial resistance) IPD - includes both resistance and invasiveness



## -------------------------
## parameterize model
## -------------------------