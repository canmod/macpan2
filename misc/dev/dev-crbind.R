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

#    p_table_x p_table_n p_table_i
## seq1 ~ t(seq(1, 2, 3)),
# 1         15         1         1  t(seq(1, 2, 3))
# 2         21         3         2  seq(1, 2, 3)
# 3          0        -1         0  literal = 1
# 4          1        -1         0  literal = 2
# 5          2        -1         0  literal = 3
## seq2 ~ t(seq(2, 2, 5)),
# 6         15         1         6  t
# 7         21         3         7  seq
# 8          3        -1         0  literal = 2
# 9          4        -1         0  literal = 2
# 10         5        -1         0  literal =
## seq3 ~ t(seq(5, 2, 2)),
# 11        15         1        11
# 12        21         3        12
# 13         6        -1         0
# 14         7        -1         0
# 15         8        -1         0
## seq4 ~ seq(7, 3, 1),
# 16        21         3        16
# 17         9        -1         0
# 18        10        -1         0
# 19        11        -1         0
## tmpmat0 ~rbind(seq1, seq2),
# 20        24         2        20
# 21         1         0         0
# 22         2         0         0
## tmpmat1 ~ rbind(tmpmat0, seq3),
# 23        24         2        23
# 24         5         0         0
# 25         3         0         0
## tmpmat2 ~ cbind(tmpmat1, seq4),
# 26        23         2        26
# 27         6         0         0
# 28         4         0         0
## ratemat ~ tmpmat2/den
# 29         4         2        29
# 30         7         0         0
# 31         8         0         0
## flowmat ~ ratemat*state,
# 32         3         2        32
# 33         9         0         0
# 34         0         0         0
## state ~ state - rowSums(flowmat) + t(colSums(flowmat))
# 35         1         2        35  `+`(`-`(state,rowSums(flowmat)),t(colSums(flowmat)))
# 36         2         2        37  `-`(state,rowSums(flowmat))
# 37        15         1        39  t(colSums(flowmat))
# 38         0         0         0  matrix = state
# 39        12         1        40  rowSums(flowmat)
# 40        13         1        41  colSums(flowmat)
# 41        10         0         0  matrix = flowmat
# 42        10         0         0  matrix = flowmat

keepers = c("state", "seq1", "seq2", "seq3", "seq4", "tmpmat0", "tmpmat1", "tmpmat2", "ratemat", "flowmat")
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
    .mats_to_save = keepers,
    .mats_to_return = keepers
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
  params = OptParamsList(89
    , par_id = 0L
    , mat = "den"
    , row_id = 0L
    , col_id = 0L
  ),
  random = OptParamsList(),
  obj_fn = ObjectiveFunction(~ sum(rowSums(flowmat))),
  time_steps = Time(time_steps = 2L)
)

data_args = bindingModel$data_arg()
parameter_args = bindingModel$param_arg()
random_args = bindingModel$random_arg()
# print(random_args)

mm = setNames(bindingModel$.init_mats$.mats(), bindingModel$.init_mats$.names())


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
