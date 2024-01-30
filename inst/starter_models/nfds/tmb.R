library(macpan2)
library(dplyr)

## -------------------------
## local function 
## -------------------------

#' Binary Matrix Notation
#'
#' Get the indices of a binary matrix that have a value of 1,
#' to simplify matrix multiplication.
#' 
#' Given a column vector \code{x, and binary Matrix \code{M}. We can 
#' perform matrix multiplication by writing,
#' \code{M %*% x}
#' To simplify this computation, we can extract the indices of \code{M}
#' that have a value of 1, reducing the dimension of the problem.
#' These indices can then be used to identify entries in \code{x} that 
#' should be grouped and summed over to compute \eqn{Mx}.
#' 
#'
#' @param M matrix containing integers in \{0,1\}
#' 
#' @return named list of two integer vectors. 
#' * \code{col_index} vector of column indices of M that have a value of 1,
#' ordered by row. Indices start at 0, for C++ implementation.
#' * \code{row_group} vector of row indices of M that have a value of 1,
#' ordered by row. Indices start at 0, for C++ implementation.
#' 
#' This function was initially created to return integer vectors that can be
#' used as inputs to `macpan2::group_sums(x[col_index],row_group,n)`
#' 
binary_matrix_notation <- function(M){
  
  col_index = c(t(col(M)*M))
  col_index = as.integer(col_index[col_index!=0]-1) 
  
  row_group = as.integer(rep(1:nrow(M), times=rowSums(M))-1)
  
  return(nlist(col_index,row_group))
}


## -------------------------
## read in model spec inputs
## -------------------------

## Get default parameter values and initial conditions used in (Colijn et al. 2020)
## csv files were generated from Matlab script, nfds/data/write_data.m
source_folder = file.path("inst","starter_models","nfds","data")

## names of source fields we need
source_fields <- c(
   "ics"                ## initial conditions
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
        #%>% as.matrix(dimnames=NULL)# why doesn't this work 
  )
  attr(x,'dimnames')<- NULL
  x
})
names(mass_data) <- source_fields

## -------------------------
## vax formulation by serotype
## -------------------------

# not totally sure where these should be defined,
# I don't think they can go in defaults because 
# they are not numeric, but might want them to 
# be accessible from model spec
included_serotypes = c('1','5','14')        ## serotypes names to be included in vaccine formulation, hardcoded in matlab code
                                            ## believe these serotypes have to be in every vaccine formulation
excluded_serotypes = c()                    ## serotypes names to be excluded in vaccine formulation (perhaps not needed)
varying_serotypes = c('4', '6B', '9V', '14',
                        '18C', '19F','23F') ## serotype names for vaccine PCV7, believe varying means optimization step
                                            ## decides which to include/exclude (how this works TBD)

## number of serotypes in data set is -1 because last one is always 'NT' for non-typables
n_serotypes <- nrow(mass_data$seronames)-1

## attempted to use assign() to update vax_serotypes, but getting confusing error messages
## updating locally for now
vax_serotypes <- rep(0,n_serotypes)
vax_serotypes[match(included_serotypes,mass_data$seronames[,1])] <- 1
vax_serotypes[match(excluded_serotypes,mass_data$seronames[,1])] <- 0
#setting to 1, think this is right, just need to tell optimizer that only these vars can be optimized 
vax_serotypes[match(varying_serotypes,mass_data$seronames[,1])] <- 1 

## -------------------------
## get binary matrix indices
## -------------------------

Yind <- binary_matrix_notation(t(mass_data$G))

pind <- binary_matrix_notation(mass_data$G)

sero_mapping <- binary_matrix_notation(mass_data$SerotypeToStrain[,1:n_serotypes-1])

## -------------------------
## expression lists
## -------------------------

vax_strategy = list(
    ## map serotype to strain
    vax_strain ~ group_sums(vax_serotypes[sero_to_strain], sero_group, vax_strain)

    ## nfds vax term
  , r ~ -log(1- (v*vax_strain))
)

computations = list(
    ## total pop size
    N ~ sum(Y)
  
    ## compute instantaneous locus frequencies from prevalences (L total)
  , f ~ (1/N) * group_sums(Y[Y_i], Y_j, f)
   
    ## (L total) 
  , weighted_deviation ~ w*(e - f)
    
    ## the 'NFDS' term, pi term in paper (M total)
  , p ~ group_sums(weighted_deviation[p_i], p_j, p)
  
)

update_state = list(

    Y ~ Y + (log(k/N) - r + rho*p)*Y + m
    
    ## log(k/N) - enforces density dependent selection
    ## r - characterizes the vaccine strategy
)

## set defaults
default = list(
    Y = mass_data$ics 
  , w = mass_data$locusweights
  , e = mass_data$locusfreq
  , k = 1e5                                     ## carrying capacity, hardcoded in matlab files
  , rho = log(1+mass_data$sigma)                ## check (Corander et al. 2017) for possible description of sigma
  , m = mass_data$m                             ## migration, does this relate to \varepsilon or the "uniform migration rate" they talk about?
  , f = rep(0,dim(mass_data$G)[2])              ## initialize f (instantaenouse locus frequencies) to zero vector of length L for group_sums 
                                                ## L = number of loci = dim(mass_data$G)[2]
  
  , p = rep(0,dim(mass_data$G)[1])              ## initialize p (pi term in paper) to zero vector of length M for group_sums 
                                                ## M = number of genotypes/strains = dim(mass_data$G)[1]
  
  , vax_serotypes = vax_serotypes               ## serotypes in vaccine
  , vax_strain = rep(0,dim(mass_data$G)[1])     ## initialize vector of strains in vaccine, vax_strain[i]=1, if genotype i is in vaccine
  , v = mass_data$v                             ## vaccine efficacy

)

integers = list(
  
    Y_i = Yind$col_index
  , Y_j = Yind$row_group
  , p_i = pind$col_index
  , p_j = pind$row_group
  
  ## attempted to pass these indices to macpan2::assign to update vax_serotypes, but wasn't successful
  # , in_vax = which(mass_data$seronames %in% c(included_serotypes, varying_serotypes))
  # , not_in_vax = which(mass_data$seronames %in% excluded_serotypes)
  
  , sero_to_strain = sero_mapping$col_index
  , sero_group = sero_mapping$row_group
)

## model specification
spec = mp_tmb_model_spec(
    before = vax_strategy
  , during = c(computations, update_state)
  , default = default
  , integers = integers
)
