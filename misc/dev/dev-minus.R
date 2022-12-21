library(macpan2)
library(TMB)

compile('dev.cpp')
dyn.load(dynlib("dev"))

m = TMBModel(
  MatsList(
    x = c(0.2, 0.45),
    y = empty_matrix,
    .mats_to_return = c("x", "y")
  ),
  ExprList(
    before = list(
      y ~ -sum(x)
    )
  ),
  OptParamsList(0),
  OptParamsList(),
  ObjectiveFunction(~ 0),
  Time(0)
)
m$data_arg()
f = m$make_ad_fun("dev")
actual_answer = f$report()$table_to_return
correct_answer = structure(c(0, 0, 1, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0.2, 0.45, -0.65
), dim = c(3L, 5L))

print("actual")
print(actual_answer)
print("correct")
print(correct_answer)
