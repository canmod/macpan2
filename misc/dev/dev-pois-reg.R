library(macpan2)
library(TMB)
library(lme4)

compile('dev.cpp')
dyn.load(dynlib("dev"))

set.seed(1L)
x = rnorm(10)
y = rpois(10, exp(2 + x + c(rep(-3, 5), rep(3, 5)) * x))
g = rep(c('a', 'b'), each = 5)
glmer_formula = y ~ x + (0 + x | g)
gf = glFormula(glmer_formula, family = poisson())

Z = t(as.matrix(gf$reTrms$Zt))
X = gf$X
theta = gf$reTrms$theta
beta = c(0, ncol(X))
u = c(-1, 1)

gm = glmer(glmer_formula, family = poisson())


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
    u_sim = empty_matrix,
    eta_sim = empty_matrix,
    mu_sim = empty_matrix,
    y_sim = empty_matrix,
    .mats_to_return = c("y_sim", "mu_sim", "mu")
  ),
  ExprList(
    before = list(
      eta ~ (X %*% beta) + ((Z * theta) %*% u),
      mu ~ e ^ eta,
      prior ~ sum(dnorm(u, rep(0, 2), rep(1, 2))),
      likelihood ~ sum(dpois(y, mu)),
      u_sim = u_sim ~ rnorm(u, rep(1, 2)),
      eta_sim = eta_sim ~ (X %*% beta) + ((Z * theta) %*% u_sim),
      mu_sim = mu_sim ~ e ^ eta_sim,
      y_sim = y_sim ~ rpois(mu_sim)
    ), .simulate_exprs = paste(c("u", "eta", "mu", "y"), "sim", sep = "_")
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
full_par = unlist(f$env$parList())
ss = setNames(
  as.data.frame(cbind(
    matrix(f$simulate(full_par)$table_to_return[,5], 10, 3),
    y)),
  c("cond_mean_mu", "sim_mu", "sim_y", "obs_y")
)


par_names = c("intercept", "slope", "ranef_sd")
actual_answer = setNames(round(opt$par, 3), par_names)
correct_answer = setNames(round(unlist(getME(gm, c("beta", "theta"))), 3), par_names)
print(ss)
print("actual")
print(actual_answer)
print("correct")
print(correct_answer)
