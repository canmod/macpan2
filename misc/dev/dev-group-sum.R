library(macpan2)
library(TMB)

compile('dev.cpp')
dyn.load(dynlib("dev"))

m = TMBModel(
  init_mats = MatsList(
      x = rnorm(10)
    , z = rep(0:3, 1:4)[order(rnorm(10))]
    , y = empty_matrix
    , .mats_to_return = c("x", "y", "z")
  ),
  expr_list = ExprList(
    before = list(y ~ groupSums(x, z))
  ),
  params = OptParamsList(0),
  random = OptParamsList(),
  obj_fn = ObjectiveFunction(~0),
  time_steps = Time(0)
)
m$data_arg()
f = m$make_ad_fun("dev")
v = f$report()$values
xx = v[1:10, 5]
zz = v[11:20, 5]
yy = v[21:24, 5, drop = TRUE]
yy_correct = c(unname(tapply(xx, zz, sum)))
print(data.frame(x = xx, z = zz))
print("correct answer")
print(yy_correct)
print("actual answer")
print(yy)
all.equal(yy_correct, yy)
