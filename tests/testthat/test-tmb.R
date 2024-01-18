
library(macpan2)
m = macpan2:::TMBModel(
  MatsList(x = 0, y = 0, .mats_to_save = c("x", "y"), .mats_to_return = c("x", "y")),
 mp_tmb_expr_list(during = list(x ~ x + 1)),
  OptParamsList(0, par_id = 0, mat = "x", row_id = 0, col_id = 0),
  OptParamsList(),
  ObjectiveFunction(~x),
  Time(10)
)
#f = m$make_ad_fun()
#f$report()$mats_returned
#f$fn()
