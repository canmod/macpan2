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
#' Given a column vector \code{x}, and binary Matrix \code{M}. We can 
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
#' * \code{row_index} vector of row indices of M that have a value of 1,
#' ordered by row. Indices start at 0, for C++ implementation.
#' 
#' This function was initially created to return integer vectors that can be
#' used as inputs to `macpan2::group_sums(x[col_index],row_index,n)`
#' 
binary_matrix_notation <- function(M){
  
  col_index = c(t(col(M)*M))
  col_index = as.integer(col_index[col_index!=0]-1) 
  
  row_index = as.integer(rep(1:nrow(M), times=rowSums(M))-1)
  
  return(nlist(col_index,row_index))
}


## -------------------------
## read in model spec inputs
## -------------------------

## Get default parameter values and initial conditions used in (Colijn et al. 2020)
## csv files were generated from Matlab script, nfds/data/write_data.m
source_folder = system.file("starter_models","nfds","data", package = "macpan2")

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
  )
  attr(x,'dimnames')<- NULL
  x
})
names(mass_data) <- source_fields

## -------------------------
## vax formulation by serotype
## -------------------------

## serotypes to be included in the vaccine regardless (highly invasive strains)
included_serotypes = c('1','5','14')        
## serotypes to be excluded from the vaccine
excluded_serotypes = c()                  
## serotypes to be optionally included/excluded from the vaccine (what we want to optimize)
## optimizer decides if each serotype here should be included or excluded based on objective
## function (and constraints i.e. max on vaccine valency, not implemented here)
##
## These 6 serotypes plus '14' are the 7 serotypes in PCV7
## Note: serotype '4' doesn't appear to exist in the Massachusetts data set, perhaps
## because detected carriage prevalence was too low in initial pop...?
varying_serotypes = c('4', '6B', '9V', '18C', '19F', '23F') 
                                            
## number of serotypes in data set is -1 because last one is always 'NT' for non-typables
#n_serotypes <- nrow(mass_data$seronames)-1
n_serotypes <- nrow(mass_data$seronames)

## if we wanted to optimize all serotypes that are not in included_serotypes and excluded_serotypes
## we could write
#varying_serotypes <- mass_data$seronames[1:n_serotypes,] %>% setdiff(union(included_serotypes, excluded_serotypes))

## form vaccine vector of serotypes (1 = in vaccine, 0 = not in vaccine)
vax_serotypes = mp_zero_vector(mass_data$seronames) 
vax_serotypes[match(included_serotypes,mass_data$seronames[,1])] = 1
vax_serotypes[match(excluded_serotypes,mass_data$seronames[,1])] = 0

## setting to 1 to include, 
## (if we had proceeded with optimization, varying_serotypes would be optimized, 
## and perhaps the model specification might need to be modified)
vax_serotypes[match(varying_serotypes,mass_data$seronames[,1])] = 1 

## -------------------------
## get binary matrix indices
## -------------------------

Gt <- binary_matrix_notation(t(mass_data$G))

G <- binary_matrix_notation(mass_data$G)

sero_mapping <- binary_matrix_notation(mass_data$SerotypeToStrain[,1:n_serotypes])

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
  , f ~ (1/N) * group_sums(Y[Gt_i], Gt_j, f)
   
    ## (L total) 
  , weighted_deviation ~ w*(e - f)
    
    ## the 'NFDS' term, pi term in paper (M total)
  , p ~ group_sums(weighted_deviation[G_i], G_j, p)
  
)

update_state = list(

    Y ~ Y + (log(k/N) - r + rho*p)*Y + m
    
    ## log(k/N) - enforces density dependent selection
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
                                                ##
  , p = rep(0,dim(mass_data$G)[1])              ## initialize p (pi term in paper) to zero vector of length M for group_sums 
                                                ## M = number of genotypes/strains = dim(mass_data$G)[1]
                                                ##
  , vax_serotypes = vax_serotypes               ## initialize vector of serotypes in vaccine, vax_serotypes[i]=1, if serotype i is in vaccine
  , vax_strain = rep(0,dim(mass_data$G)[1])     ## initialize vector of strains in vaccine, vax_strain[i]=1, if genotype i is in vaccine
  , v = mass_data$v                             ## vaccine efficacy parameter

)

## integer vectors
integers = list(
    Gt_i = Gt$col_index                                           ## non-zero indices of G transpose (loci-genotype) matrix
  , Gt_j = Gt$row_index                                           ##
  , G_i = G$col_index                                             ## non-zero indices of G (genotype-loci) matrix
  , G_j = G$row_index                                             ##
  , sero_to_strain = sero_mapping$col_index                       ## non-zero indices of serotype to strain mapping matrix
  , sero_group = sero_mapping$row_index                           ##
)

## model specification
spec = mp_tmb_model_spec(
    before = vax_strategy
  , during = c(computations, update_state)
  , default = default
  , integers = integers
)
