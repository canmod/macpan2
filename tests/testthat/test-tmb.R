
library(macpan2)
m = macpan2:::TMBModel(
  macpan2:::MatsList(x = 0, y = 0, .mats_to_save = c("x", "y"), .mats_to_return = c("x", "y")),
 mp_tmb_expr_list(during = list(x ~ x + 1)),
  macpan2:::OptParamsList(0, par_id = 0, mat = "x", row_id = 0, col_id = 0),
  macpan2:::OptParamsList(),
  macpan2:::ObjectiveFunction(~x),
  macpan2:::Time(10)
)
#f = m$make_ad_fun()
#f$report()$mats_returned
#f$fn()
