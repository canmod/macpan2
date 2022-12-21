library(macpan2)
library(TMB)

compile('dev.cpp')
dyn.load(dynlib("dev"))

set.seed(1L)
x = rnorm(10)
y = rpois(10, exp(2 + x + c(rep(-3, 5), rep(3, 5)) * x))
g = rep(c('a', 'b'), each = 5)

library(lme4)
gf = glFormula(y ~ x + (0 + x | g), family = poisson())

Z = t(as.matrix(gf$reTrms$Zt))
X = gf$X
theta = gf$reTrms$theta
beta = c(0, ncol(X))
u = c(-1, 1)

gm = glmer(y ~ x + (0 + x | g), family = poisson())


m = TMBModel(
  MatsList(
    X = X,
    Z = Z,
    theta = theta,
    beta = beta,
    u = u,
    y = y,
    eta = empty_matrix, # 6
    mu = empty_matrix, # 7
    prior = empty_matrix, # 8
    likelihood = empty_matrix, # 9
    e = exp(1),
    .mats_to_return = c("eta", "mu", "prior", "likelihood")
  ),
  ExprList(
    before = list(
      eta ~ (X %*% beta) + ((Z * theta) %*% u),
      mu ~ e ^ eta,
      prior ~ sum(dnorm(u, rep(0, 2), rep(1, 2))),
      likelihood ~ sum(dpois(y, mu))
    )
  ),
  OptParamsList(0, 0, 1
    , par_id = c(0L, 1L, 2L)
    , mat = c("beta", "beta", "theta")
    , row_id = c(0L, 1L, 0L)
    , col_id = c(0L, 0L, 0L)
  ),
  OptParamsList(-1, 1
    , par_id = c(0L, 1L)
    , mat = c("u", "u")
    , row_id = c(0L, 1L)
    , col_id = c(0L, 0L)
  ),
  ObjectiveFunction(~ prior + likelihood),
  Time(0)
)
f = m$make_ad_fun("dev")
pp = f$par
ff = f$fn(c(2, 1, 3))
rr = f$report(c(2, 1, 3, -1, 1))
opt = do.call(optim, f)
par_names = c("intercept", "slope", "ranef_sd")
actual_answer = setNames(round(opt$par, 2), par_names)
correct_answer = setNames(round(unlist(getME(gm, c("beta", "theta"))), 2), par_names)

print("actual")
print(actual_answer)
print("correct")
print(correct_answer)
