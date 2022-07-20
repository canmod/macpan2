library(TMB)

set.seed(101)
data <- list(
  MatrixList = list(matrix(rnorm(9), 3, 3)),
  VectorList = list(rnorm(3))
)
  
compile("TMBlist.cpp")
dd <- dyn.load(dynlib("TMBlist"))
dlls <- getLoadedDLLs()
getDLLRegisteredRoutines(dd)
## hmmm ...

## debug(MakeADFun)
obj <- MakeADFun(data = data,
                 parameters = list(),
                 DLL = "TMBlist")

## current status
## crashes R unpredictably
##  sometimes? prints debugging statements, sometimes doesn't
## crashes on "retape(set.defaults = TRUE)" (i.e. first call to
##   DLL ...)
