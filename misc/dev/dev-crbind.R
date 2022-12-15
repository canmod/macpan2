library(macpan2)
library(TMB)

compile('dev.cpp')
dyn.load(dynlib("dev"))

correct_answer = function(den = 89){
  state = c(1, 0, 0)
  inputmat = matrix(
    c(
      1, 4, 7,
      2, 7, 8,
      5, 7, 9
    ), 3, 3, byrow = TRUE
  )
  ratemat = inputmat/den
  flowmat = matrix(0, 3, 3)
  state_hist = list(as.matrix(state))
  flowmat_hist = list(as.matrix(flowmat))
  for (i in 1:2){
    flowmat = sweep(ratemat, 1, state, "*")
    state = state - rowSums(flowmat) + t(colSums(flowmat))
    state_hist = c(state_hist, list(as.matrix(state)))
    flowmat_hist = c(flowmat_hist, list(as.matrix(flowmat)))
  }
  flowmat = flowmat_hist
  state = state_hist
  return(nlist(state, flowmat))
}

bindingModel = TMBModel(
  init_mats = MatsList(
    state = c(1, 0, 0),
    seq1 = matrix(0, 1, 2),
    seq2 = matrix(0, 1, 2),
    seq3 = matrix(0, 1, 2),
    seq4 = matrix(0, 3, 1),
    tmpmat0 = matrix(0, 2, 2),
    tmpmat1 = matrix(0, 2, 3),
    tmpmat2 = matrix(0, 3, 3),
    den = 89,
    ratemat = matrix(0, 3, 3),
    flowmat = matrix(0, 3, 3),
    .mats_to_save = c("state", "seq1", "seq2", "seq3", "seq4","tmpmat0", "tmpmat1", "tmpmat2", "ratemat", "flowmat"),
    .mats_to_return = c("state", "seq1", "seq2", "seq3", "seq4", "tmpmat0", "tmpmat1", "tmpmat2", "ratemat", "flowmat")
    # .mats_to_save = c("flowmat"),
    # .mats_to_return = c("flowmat")
  ),
  expr_list = ExprList(
    before = list(
      seq1 ~ t(seq(1, 2, 3)),
      seq2 ~ t(seq(2, 2, 5)),
      seq3 ~ t(seq(5, 2, 2)),
      seq4 ~ seq(7, 3, 1),
      tmpmat0 ~rbind(seq1, seq2),
      tmpmat1 ~ rbind(tmpmat0, seq3),
      tmpmat2 ~ cbind(tmpmat1, seq4),
      ratemat ~ tmpmat2/den
    ),
    during = list(
      flowmat ~ ratemat*state,
      state ~ state - rowSums(flowmat) + t(colSums(flowmat))
    )
  ),
  params = OptParamsList(89,
                         par_id = 0L,
                         mat = "den",
                         row_id = 0L,
                         col_id = 0L
  ),
  random = OptParamsList(),
  obj_fn = ObjectiveFunction(~ sum(rowSums(flowmat))),
  time_steps = Time(time_steps = 2L)
)

data_args = bindingModel$data_arg()
parameter_args = bindingModel$param_arg()
random_args = bindingModel$random_arg()
# print(random_args)

print("data args ...")
print(data_args)

print("parameter args ...")
print(parameter_args)

tmb_function = try(TMB::MakeADFun(
  data = data_args,
  parameters = parameter_args,
  random = random_args,
  DLL = 'macpan2'
))
correct_answer()
tmb_function$report()
