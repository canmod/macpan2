library(macpan2)
library(TMB)

compile('dev.cpp')
dyn.load(dynlib("dev"))

correct_answer = function() {
  x = 1
  mat = 0
  time = 0
  value = x
  for (t in 1:3) {
    x = x / 2
    mat = c(mat, 0)
    time = c(time, t)
    value = c(value, x)
    if (t - 2L >= 0L) {
      y = value[mat == 0 & time == t - 2L]
      mat = c(mat, 1)
      time = c(time, t)
      value = c(value, y)
    }
  }
  mat = c(mat, 0, 1)
  time = c(time, 4, 4)
  value = c(value, x, y)
  dplyr::arrange(as.data.frame(macpan2:::nlist(mat, time, row = 0, col = 0, value)), mat, time)
}

m = TMBModel(
  MatsList(
    x = 1,
    y = empty_matrix,
    z = 1,
    .mats_to_save = c("x", "y", "z"),
    .mats_to_return = c("x", "y")
  ),
  ExprList(
    during = list(
      x ~ x / 2,
      y ~ rbind_lag(x, 2, 0)
    )
  ),
  OptParamsList(1, par_id = 0L, mat = "x", row_id = 0L, col_id = 0L),
  OptParamsList(),
  ObjectiveFunction(~x),
  Time(3)
)
m$data_arg()
m$param_arg()
f = m$make_ad_fun("dev")

correct_hist = correct_answer()
actual_hist = setNames(
  as.data.frame(f$report()$table_to_return),
  names(correct_hist)
)

print("actual")
print(actual_hist)
print("correct")
print(correct_hist)
#print(head(actual_hist, 1000))
#print(head(correct_hist, 1000))

# print(cbind(
#   macpan2:::ExprListUtils()$.set_name_prefix(actual_hist, "actual_"),
#   macpan2:::ExprListUtils()$.set_name_prefix(correct_hist, "correct_")
# )[c(1, 6, 2, 7, 3, 8, 4, 9, 5, 10)])
#
print(all.equal(actual_hist, correct_hist))
