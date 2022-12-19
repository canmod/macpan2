library(macpan2)
library(TMB)

compile('dev.cpp')
dyn.load(dynlib("dev"))

r = "y"
correct_answer = function(x, return_mat = "y") {
  x = list(x)
  y = list(as.matrix(0))

  for (i in 2:16) {
    x[[i]] = 1.3 * x[[i - 1]] - 0.2
    y[[i]] = as.matrix(unlist(x[pmax(2, i - 5):i], recursive = FALSE))
  }
  x[[17]] = x[[16]]
  y[[17]] = y[[16]]
  x = lapply(x, as.matrix)
  nlist(x, y)[[return_mat]]
}

m = TMBModel(
  MatsList(
    x = 1,
    y = 0,
    z = 1,
    .mats_to_save = c("x", "y", "z"),
    .mats_to_return = r
  ),
  ExprList(
    during = list(
      x ~ 1.3 * x - 0.2,
      y ~ rbind_lag(x, 0:5)
    )
    # after = list(
    #   y ~ rbind_time(x, 1:15)
    # )
  ),
  OptParamsList(0.9, par_id = 0L, mat = "x", row_id = 0L, col_id = 0L),
  OptParamsList(),
  ObjectiveFunction(~x),
  Time(15)
)
m$data_arg()
m$param_arg()
f = m$make_ad_fun("dev")

correct = correct_answer(0.9, r)
correct_hist = data.frame(
  mat = which(c("x", "y") == r) - 1L,
  time = rep(seq_along(correct), times = vapply(correct, length, integer(1L))) - 1L,
  row = unlist(lapply(correct, row)) - 1L,
  col = unlist(lapply(correct, col)) - 1L,
  value = unlist(lapply(correct, c))
)
actual_hist = setNames(
  as.data.frame(f$report()$table_to_return),
  names(correct_hist)
)

print("actual")
print(head(actual_hist, 1000))
print("correct")
print(head(correct_hist, 1000))

# print(cbind(
#   macpan2:::ExprListUtils()$.set_name_prefix(actual_hist, "actual_"),
#   macpan2:::ExprListUtils()$.set_name_prefix(correct_hist, "correct_")
# )[c(1, 6, 2, 7, 3, 8, 4, 9, 5, 10)])
#
print(all.equal(actual_hist, correct_hist))
