library(TMB)

set.seed(101)
data <- list(
  MatrixList = list(matrix(rnorm(9), 3, 3)),
  VectorList = list(rnorm(3))
)
param = list(theta = rnorm(1))

compile("TMBlist.cpp")
dd <- dyn.load(dynlib("TMBlist"))
dlls <- getLoadedDLLs()
getDLLRegisteredRoutines(dd)
## hmmm ...

## debug(MakeADFun)
obj <- MakeADFun(
  data = data,
  parameters = param,
  DLL = "TMBlist"
)

obj$fn() == (data$MatrixList[[1]] * data$VectorList[[1]] * param$theta)[1]
obj$gr() == (data$MatrixList[[1]] * data$VectorList[[1]])[1]

## current status
## crashes R unpredictably
##  sometimes? prints debugging statements, sometimes doesn't
## crashes on "retape(set.defaults = TRUE)" (i.e. first call to
##   DLL ...)
