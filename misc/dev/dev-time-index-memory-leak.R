library(macpan2)
library(TMB)
cpp = macpan2:::dev_choose_cpp()
x = 0.1 * (1:5)
engine_eval(~x[-1]
  , x = x
  , .tmb_cpp = cpp
)
