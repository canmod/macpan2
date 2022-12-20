library(macpan2)
library(TMB)

compile('dev.cpp')
dyn.load(dynlib("dev"))

m = TMBModel(
    init_mats = MatsList(
        m = matrix(1:12, 4, 3)
      , a = rep(0, 3)
      , b = rep(0, 5)
      , c = matrix(rep(0, 4), 2, 2)
      , dummy = empty_matrix
      , .mats_to_return = c("m", "a", "b", "c")
    )
  , expr_list = ExprList(before = list(dummy ~ assign(m, a, b, c)))
  , params = OptParamsList(0)
  , random = OptParamsList()
  , obj_fn = ObjectiveFunction(~0)
  , time_steps = Time(0L)
)
m$make_ad_fun("dev")$report()
