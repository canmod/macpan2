library(TMB)

set.seed(101)
data <- list(
  MatrixList = list(matrix(rnorm(9), 3, 3)),
  VectorList = list(rnorm(3))
)
  
compile("TMBlist.cpp")
dyn.load(dynlib("TMBlist"))
obj <- MakeADFun(data = data,
                 parameters = list(x=1),
                 DLL = "TMBlist")
