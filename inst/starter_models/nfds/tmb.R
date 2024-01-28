library(macpan2)
library(dplyr)

computations = list(
    ## total pop size
    N ~ sum(Y)
  
    ## compute instantaneous locus frequencies (L total)
  , f ~ (1/N) * (t(Y) %*% G)
  
    ## pi term in paper (M total), need to rename
  , p ~ G %*% to_diag(w) %*% t(t(eq) - f) # is taking transpose slow computationally? (maybe a "flow")
  
)

update_state = list(

    Y ~ Y + (log(k/N) - r + rho*p)*Y + m ## why are brackets square for log[k/N], does this signify anything
    
    ## log(k/N) - enforces density dependent selection
    ## r - characterizes the vaccine strategy
    ## p - term that computes where prevalence is in relation to equilibrium frequencies (I think)
)

## -------------------------

## Get default values (this should maybe be included as a 'before' expression step, or defined in the simulation script)
## read in input data (after data/write_data.m has been run)
source_folder = file.path("inst","starter_models","nfds","data")

## names of source fields we need
source_fields <- c(
    "ics"               ## initial conditions
  , "G"                 ## matrix encoding the genome data. Rows are isolates, cols are loci. G_{il} = 1 if strain i encodes locus l, 0 otherwise.
  , "SerotypeToStrain"  ## matrix encoding which serotype each strain is
  , "seronames"         ## names of the serotypes in the dataset, order is important. 
  , "AntiNames"         ## names of the antigens; order is important
  , "sigma"             ## parameter for strength of NFDS
  , "locusweights"      ## locus weights
  , "locusfreq"         ## equilibrium locus frequencies
  , "v"                 ## vaccine efficacy parameter
  , "m"                 ## migration (small parameter)
  , "Invasiveness.kids" ## invasiveness numbers from the meta-analysis, kids only
  )

## create one list object with named elements
mass_data <- lapply(seq_along(source_fields), function(i){
  x <- (read.csv(file.path(source_folder,paste0(source_fields[i],".csv")), header=FALSE)
        %>% as.matrix() 
  )
  attr(x,'dimnames')<- NULL
  x
})
names(mass_data) <- source_fields

## create vector of vaccine strategy

## total number of serotypes, (last one is always 'NT' for non-typables)
n_serotypes <- nrow(mass_data$seronames)-1

## hardcoded as these serotypes should always be included
included_serotypes <- c('1','5','14') 
excluded_serotypes <- c()
# varying_serotypes <- mass_data$seronames[1:n_serotypes,] %>% setdiff(union(included_serotypes, excluded_serotypes))
## PCV7 serotypes 
varying_serotypes <- c('4','6B', '9V', '14', '18C', '19F', '23F')


## vax_serotypes[i] == 1, if serotype i is in the vaccine, 0 otherwise
vax_serotypes <- rep(0,n_serotypes)
vax_serotypes[match(included_serotypes,mass_data$seronames[,1])] <- 1
vax_serotypes[match(excluded_serotypes,mass_data$seronames[,1])] <- 0
#setting to 1, think this is right, just need to tell optimizer that only these vars can be optimized 
vax_serotypes[match(varying_serotypes,mass_data$seronames[,1])] <- 1 

## now convert to level of genotype, is genotype i in the vaccine?
v=0.0610 
vax_strain <- as.numeric(mass_data$v)*(data.matrix(mass_data$SerotypeToStrain[,1:n_serotypes]) %*% matrix(vax_serotypes, nrow=length(vax_serotypes)))

## -------------------------

## set defaults
default = list(
    Y = mass_data$ics 
  , G = mass_data$G
  , w = mass_data$locusweights
  , eq = mass_data$locusfreq
  , k = 1e5                      ## carrying capacity, hardcoded in matlab files
  , r = -log(1-vax_strain)
  , rho = log(1+mass_data$sigma) ## check (Corander et al. 2017) for possible description of sigma
  , m = mass_data$m              ## migration, does this relate to \varepsilon or the "uniform migration rate" they talk about
)

## model specification
spec = mp_tmb_model_spec(
    during = c(computations, update_state)
  , default = default
)
