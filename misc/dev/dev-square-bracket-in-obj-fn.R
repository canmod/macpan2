library(macpan2)
library(TMB)

compile('dev.cpp')
dyn.load(dynlib("dev"))

error_model = TMBModel(
  MatsList(
    const1 = 0.3,
    ratemat = matrix(0, 2, 2),
    flowmat = matrix(0, 2, 2),
    state = c(1, 0),
    .mats_to_save = c("const1", "flowmat", "state"),
    .mats_to_return = c("const1", "flowmat", "state")
  ),
  ExprList(
    before = list(
    ratemat ~ matrix(
      c(
        0, 0,
        const1, 0
      ), 2, 2
    )),
    during = list(
    flowmat ~ ratemat*state,
    state ~ state - rowSums(flowmat) + t(colSums(flowmat))
    )
  ),
  OptParamsList(0.3, par_id = 0L, mat = "const1", row_id = 0L, col_id = 0L),
  OptParamsList(),
  #ObjectiveFunction(~state[1, 0]),
  ObjectiveFunction(~state[0, 0]),
  Time(2)
)

model = error_model$make_ad_fun("dev")

model$report()
