## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "./figures/"
)


## ----packages, include=FALSE--------------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
library(macpan2)


## ----model_spec---------------------------------------------------------------
spec = mp_tmb_library("starter_models","nfds", package="macpan2")



## ----simulated_prevalence-----------------------------------------------------
# set number of time steps in simulation
# time_steps = 120L is equal to 10 years (as in Colijn et al. 2020)
# step size = months
time_steps = 120L 

# create simulator
nfds = mp_simulator(  
    model = spec
  , time_steps = time_steps
  , outputs = c("Y","vax_strain","vax_serotypes")
) 

# simulate Y (genotype prevalence state vector) and plot
(nfds
  |> mp_trajectory()
  |> filter(matrix=='Y')
  |> mutate(genotype=as.factor(row))
  |> ggplot()
  + geom_line(aes(time,value,col=genotype))
  + theme_bw()
  + theme(legend.position="none")
  + ylab("prevalence")
  + xlab("time (months)")
  + ggtitle("Prevalence of all genotypes (M=603)",subtitle = "macpan2 implementation")
)


## ----simulated_prevalence_by_vax----------------------------------------------
# get vaccine mapping by strain/genotype
in_vax = (nfds
          |> mp_trajectory()
          |> filter(matrix %in% c("vax_strain"))
          |> mutate(in_vax=as.factor(value))
)

# simulate Y (genotype prevalence) and plot, coloured by vaccine inclusion
(nfds
  |> mp_trajectory()
  |> filter(matrix=='Y')
  |> rename(prevalence=value)
  |> left_join(in_vax, by=c("time", "col", "row"))
  |> ggplot()
  + geom_line(aes(time,prevalence,group=row,col=in_vax),alpha=0.8)
  + scale_colour_manual(name = 'In vaccine',values=c('darkgrey','#670000'))
  + theme_bw()
  + ylab("prevalence")
  + xlab("time (months)")
  + ggtitle("Prevalence of all genotypes (M=603)",subtitle = "macpan2 implementation")
) 



## ----get_params, include=FALSE------------------------------------------------
# get input parameters and initial conditions
G = system.file("starter_models","nfds","data","G.csv", package="macpan2") %>% read.csv(header=FALSE) %>% as.matrix() %>% unname()
w = system.file("starter_models","nfds","data","locusweights.csv", package="macpan2") %>% read.csv(header=FALSE)  %>% as.matrix() %>% t() %>% unname()
e = system.file("starter_models","nfds","data","locusfreq.csv", package="macpan2") %>% read.csv(header=FALSE)  %>% as.matrix() %>% t() %>% unname()
Y = system.file("starter_models","nfds","data","ics.csv", package="macpan2")%>% read.csv(header=FALSE)  %>% as.matrix() %>% t() %>% unname()
N=1e5


## ----old_expr-----------------------------------------------------------------
# old expression
old_expr = list(
  # compute f_{i,t}
  f ~ (1/N) * (Y %*% G)
)


## ----new_expr-----------------------------------------------------------------
# new expression
new_expr = list(
  # compute f_{i,t}
  f ~ (1/N) * group_sums(Y[Gt_i], Gt_j, f)
)


## ----binary_matrix_notation---------------------------------------------------
binary_matrix_notation <- function(M){
  
  # get col indices
  col_index = c(t(col(M)*M))
  col_index = as.integer(col_index[col_index!=0]-1) 
  
  # get row indices
  row_index = as.integer(rep(1:nrow(M), times=rowSums(M))-1)
  
  return(nlist(col_index,row_index))
}

# get non-zero indices of G transpose (loci-genotype matrix)
# because we have a left multiplication of G (Y %*% G)
Gt_ind <- binary_matrix_notation(t(G)) 

# save indices to a list
integers = list(
    Gt_i = Gt_ind$col_index                                          
  , Gt_j = Gt_ind$row_index                                         
)



## ----dim_reduction------------------------------------------------------------
# number of entries in integer indices
length(integers$Gt_i)+length(integers$Gt_i)


## ----old_vs_new_spec----------------------------------------------------------
# old specification
old_spec = mp_tmb_model_spec(
    during = old_expr
  , default = list(
      N = N
    , G = G
    , Y = Y
  )
)
# new specification
new_spec = mp_tmb_model_spec(
    during = new_expr
  , default = list(
       N = N
     , Y = Y
     # group_sums needs 'n' argument (see ?macpan2::group_sums)
     # so we need to initialize f with length = L (# intermedifate frequency loci)
     , f = rep(0,dim(G)[2]) 
    ) 
  , integers = integers
)


## ----new_time-----------------------------------------------------------------
# old spec
start_time <- Sys.time()
old_sim = mp_simulator(old_spec,time_steps = 1L,outputs = "Y")
end_time <- Sys.time()
end_time - start_time

# new spec
start_time <- Sys.time()
new_sim = mp_simulator(new_spec,time_steps = 1L,outputs = "Y")
end_time <- Sys.time()
end_time - start_time


## ----model_sims, include=FALSE------------------------------------------------
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
  |> as.matrix()
  |> t()
  |> data.frame()
  |> mutate(genotype=row_number())
  |> pivot_longer(cols= -c(genotype), names_to = "time", values_to = "prevalence")
  |> mutate(time=as.integer(gsub("^X","",time,perl=TRUE)),model='colijn')
  |> arrange(time,genotype)
)

# simulated data from macpan2
# Note: the PCV7 vax is the default vaccine in the model spec tmb.R file
# so no changes are required here
sim_macpan2 = (nfds
               |> mp_trajectory()
               |> filter(matrix=='Y')
               |> mutate(genotype=as.integer(row)+1L,
                          model='macpan2')
               |> rename(prevalence="value")
               |> dplyr::select(-c(row,col,matrix))
               |> arrange(time,genotype)
)

# arrange data
# long
sim_long = (union(sim_colijn, sim_macpan2))

# wide
sim_wide = (sim_colijn 
            |> rename(colijn=prevalence) 
            |> select(-model)
            |> inner_join(
              (sim_macpan2 
               |> rename(macpan2=prevalence) 
               |> select(-model)
               )
              , by = join_by(genotype, time)
              ) 
            |> mutate(`colijn - macpan2`= colijn - macpan2)
)


## ----prevalence_10years, echo=FALSE-------------------------------------------
# prevalence of both model implementations at final time step
(ggplot((sim_wide %>% filter(time==time_steps)), aes(x=colijn,y=macpan2))
  + geom_point(pch=1)
  + theme_bw()
  + ggtitle("Prevalence by model implementation at 10 years")
)



## ----difference_10years, echo=FALSE-------------------------------------------
# prevalence difference of both model implementations at final time step
(ggplot((sim_wide %>% filter(time==time_steps)), aes(x=genotype,y=`colijn - macpan2`))
  + geom_col()
  + theme_bw()
  + ggtitle("Prevalence difference per genotype at 10 years")
  + xlab("genotype ID")
)


## ----prep_data, include=FALSE-------------------------------------------------
# choose random subset of genotypes so we can visualize
# differences over time easier
set.seed(123)
n_genotypes = 8 # choosing 8
dark2=c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666")
# genotypes with high prevalence initial conditions
high_ics = (sim_wide 
  |> filter(time==1 & colijn > 500)
  |> select(genotype)
  |> unique()
  |> sample_n(n_genotypes)
  |> mutate(initial_prevalence="high",
            line_col=dark2)
)
# genotypes with low prevalence initial conditions
low_ics = (sim_wide
   |> filter(time==1 & colijn < 500)
   |> select(genotype)
   |> unique()
   |> anti_join(high_ics, by=join_by(genotype))
   |> sample_n(n_genotypes)
   |> mutate(initial_prevalence="low"
             ,line_col=dark2)
)
# union all
all_ics = union(high_ics, low_ics)


rand_genotypes = ((sim_wide 
           %>% inner_join(all_ics, by=join_by(genotype)) 
           %>% select(-c(colijn, macpan2))
           %>% rename(value=`colijn - macpan2`) 
           %>% mutate(value_desc='colijn - macpan2', model=value_desc)
           ) |> union(sim_long 
             %>% inner_join(all_ics, by=join_by(genotype)) 
             %>% rename(value=prevalence)
             %>% mutate(value_desc='prevalence')
           )
)

# set colours
genotype_cols = c(dark2,dark2)
names(genotype_cols) = rand_genotypes |> select(genotype) |> unique() |> pull()


## ----plots_over_time, echo=FALSE----------------------------------------------

 (ggplot(rand_genotypes %>% filter(value_desc=="prevalence"))
  + geom_line(aes(x=time, y=value, col=as.factor(genotype), linetype=model))
  + theme_bw()
  + facet_wrap(vars(initial_prevalence),labeller = "label_both",ncol=2,scales="free_y")
  + xlab("time (months)")
  + ylab("prevalence")
  # +scale_y_log10()
  + scale_colour_manual(values = genotype_cols)
  + guides(col="none",linetype=guide_legend(nrow = 1))
  + ggtitle("Genotype prevalence over time",subtitle="random subset of genotypes (n=16)")
  + theme(legend.position='bottom')
 )

 (ggplot(rand_genotypes %>% filter(value_desc=='colijn - macpan2'))
  + geom_line(aes(x=time, y=value, colour=as.factor(genotype)))
  + theme_bw()
  + facet_wrap(vars(initial_prevalence),labeller = "label_both",ncol=2,scales="free_y")
  + xlab("time (months)")
  + ylab('colijn - macpan2')
  + scale_colour_manual(values = genotype_cols)
  + guides(col="none")
  + ggtitle("Genotype prevalence difference over time",subtitle="random subset of genotypes (n=16)")
  + geom_hline(yintercept = 0, col="black", linetype="dashed")
 )



