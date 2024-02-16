library(ggplot2)
library(dplyr)
library(tidyr)
library(cowplot)
library(macpan2)

## Note: No calibration is done in this script, despite script name.

## -------------------------
## get model spec from library
## -------------------------

spec = mp_tmb_library("starter_models", "nfds", package = "macpan2")
spec

## -------------------------
## define simulator
## -------------------------

# set number of time steps in simulation
# time_steps = 120L is equal to 10 years (as in Colijn et al. 2020)
# step size = months
time_steps = 120L 


# simulator object
start_time <- Sys.time()
nfds = mp_simulator(  
    model = spec
  , time_steps = time_steps
  , outputs = c("Y","vax_strain","vax_serotypes")
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
  + ylab("prevalence")
)

# if genotype i is in the in_vax > 0, else 0
# we expect seroreplacement to take place, genotypes not in vaccine 
# dominate population after vaccine perturbation (with sufficient time steps)
# this graphically looks to be happening
in_vax = (nfds
          |> mp_trajectory()
          |> filter(matrix %in% c("vax_strain"))
          |> mutate(in_vax=as.factor(value))
)
(nfds
  |> mp_trajectory()
  |> filter(matrix=='Y')
  |>  rename(prevalence=value)
  |>  left_join(in_vax, by=c("time", "col", "row"))
  |> ggplot()
  + geom_line(aes(time,prevalence,group=row,col=in_vax),alpha=0.8)
  + scale_colour_manual(name = 'In Vax',values=c('darkgrey','#670000'))
  + theme_bw()
) 

## -------------------------
## validate model
## -------------------------
# compare `nfds` simulated data with simulated data from
# Colijn et al. (2020) Matlab code to check if the model 
# was implemented accurately in tmb.R

# simulation situation:
# Massachusetts carriage population with PCV7 vaccine.
# Note, serotpye '4' is not present in this data set.

# simulated data from Colijn et al. is generated from
# nfds/data/simulate_data.m
sim_colijn = (
  read.csv(
    system.file(
        "starter_models"
      , "nfds"
      , "data"
      , "simdata_PCV7.csv"
      , package = "macpan2"
    )
    , header = FALSE
  )
  %>% as.matrix()
  %>% t()
  %>% data.frame()
  %>% mutate(genotype=row_number())
  %>% pivot_longer(cols= -c(genotype), names_to = "time", values_to = "prevalence")
  %>% mutate(time=as.integer(gsub("^X","",time,perl=TRUE)),model='colijn')
  %>% arrange(time,genotype)
)

# simulated data from TMB
sim_macpan2 = (nfds
               %>% mp_trajectory()
               %>% filter(matrix=='Y')
               %>% mutate(genotype=as.integer(row)+1L,
                          model='macpan2')
               %>% rename(prevalence="value")
               %>% dplyr::select(-c(row,col,matrix))
               %>% arrange(time,genotype)
)


# arrange data
sim_long = (union(sim_colijn, sim_macpan2))

sim_diff = (sim_colijn 
            %>% rename(colijn=prevalence) 
            %>% select(-model)
            %>% inner_join(
              (sim_macpan2 
               %>% rename(macpan2=prevalence) 
               %>% select(-model)
               )
              ) 
            %>% mutate(`colijn - macpan2`= colijn - macpan2)
)

## prevalence of both model implementations at final time step
# looks good
(ggplot((sim_diff %>% filter(time==time_steps)), aes(x=colijn,y=macpan2))
  + geom_point(alpha=0.8)
  + theme_bw()
  + ggtitle("prevalence by model implementation at 10 years")
)


## prevalence differences at final time step
max(abs(subset(sim_diff,time==time_steps)$`colijn - macpan2`))
# maybe this is okay,
# max difference is 17 individuals, in a population of size N=1e5
# 17/1e5 ~ is 0.02% of the population (not ecologically/epidemiologically significant?)
(ggplot((sim_diff %>% filter(time==time_steps)), aes(x=genotype,y=`colijn - macpan2`))
  + geom_col()
  + theme_bw()
  + ggtitle("prevalence difference per genotype at 10 years")
)


# all genotypes over time for both model implementations
# choose random subset of genotypes so we can visualize
# differences easier
set.seed(123)
# genotypes with high prevalence initial conditions
high_ics = (sim_diff 
            %>% filter(time==1 & colijn > 500)
            %>% select(genotype)
            %>% unique() %>% pull() %>% sample(8)
)
# genotypes with low prevalence initial conditions
low_ics = (sim_diff
           %>% filter(time==1 & colijn < 500)
           %>% select(genotype)
           %>% unique() %>% pull() %>% sample(8)
)


## differences in model implementation seem to be minimal
## for this subset of genotypes
high = (ggplot((sim_long %>% filter(genotype %in% high_ics))
               , aes(x=time,y=prevalence,col=as.factor(genotype),linetype=model))
  + geom_line()
  + theme_bw()
  + scale_color_brewer(palette = "Dark2")
  + ggtitle("Random subset of genotypes")
)
low = (ggplot((sim_long) %>% filter(genotype %in% low_ics)
              , aes(x=time,y=prevalence,col=as.factor(genotype),linetype=model))
  + geom_line()
  + theme_bw()
  + scale_color_brewer(palette = "Dark2")
  + guides(linetype="none")
  + scale_y_log10()
)
plot_grid(high,low,ncol=1)



## random genotypes from above, plotting model implementation differences
## early time points, see greater differences, than later?
diff_high = (ggplot((sim_diff %>% filter(genotype %in% high_ics))
               , aes(x=time,y=`colijn - macpan2`,col=as.factor(genotype)))
        + geom_line()
        + geom_hline(yintercept = 0, col="black", linetype="dashed")
        + theme_bw()
        + scale_color_brewer(palette = "Dark2")
        + ggtitle("Random subset of genotypes")
)

diff_low = (ggplot((sim_diff %>% filter(genotype %in% low_ics))
                   , aes(x=time,y=`colijn - macpan2`,col=as.factor(genotype)))
            + geom_line()
            + geom_hline(yintercept = 0, col="black", linetype="dashed")
            + theme_bw()
            + scale_color_brewer(palette = "Dark2")
)
plot_grid(diff_high,diff_low,ncol=1)
