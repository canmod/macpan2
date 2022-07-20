library(TMB)
set.seed(1L)
n = 10
a = list(
  data = list(
    #Y = rnorm(n),
    #x = rnorm(n)
    Yx = list(Y = rnorm(n), x = rnorm(n))
    #MatrixList = list(matrix(0, 1, 1), matrix(1, 2, 2)),
    #vec = c(c(1, 2, 3), c(1, 2))
  ),
  param = list(
    a = as.numeric(0),
    b = as.numeric(0),
    logSigma = as.numeric(0)
  )
)
compile('strucs.cpp')
dyn.load(dynlib("strucs"))
f = MakeADFun(data = a$data, parameters = a$param, DLL = 'strucs')
optim(f$par, f$fn)
