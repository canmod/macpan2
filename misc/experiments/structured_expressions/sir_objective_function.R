library(macpan2)
library(TMB)

compile('macpan2.cpp')
dyn.load(dynlib("macpan2"))

correct_answer = function(beta = 0.3) {

  ## matrices
  state = c(1 - 1e-2, 1e-2, 0)
  gamma = 0.2
  N = 1
  foi = 0
  ratemat = matrix(0, 3, 3)
  flowmat = matrix(0, 3, 3)

  state_hist = list(as.matrix(state))
  N_hist = list(as.matrix(N))
  foi_hist = list(as.matrix(foi))
  for (i in 1:2) {
    N = sum(state)
    foi = beta * state[2] / N
    ratemat = matrix(
      c(
        0, foi, 0,
        0, 0,   gamma,
        0, 0,   0
      ), 3, 3, byrow = TRUE
    )
    flowmat = sweep(ratemat, 1, state, "*")
    state = state - rowSums(flowmat) + colSums(flowmat)
    state_hist = c(state_hist, list(as.matrix(state)))
    N_hist = c(N_hist, list(as.matrix(N)))
    foi_hist = c(foi_hist, list(as.matrix(foi)))
  }
  correct_obj_fn_output = sum(state)
  state = state_hist
  N = N_hist
  foi = foi_hist
  return(
    nlist(state, N, foi, correct_obj_fn_output)
  )
}

input_mats = list(
  state = c(1 - 1e-2, 1e-2, 0),
  beta = 0.3,
  gamma = 0.2
)

# valid_funcs = nlist(
#   `+`, `-`, `*`, `/`, `^`,
#   `(`,
#   `c`, `matrix`,
#   `%*%`, `sum`, `rep`,
#   `rowSums`, `colSums`,
#   `[`, `t`
# )

valid_funcs = macpan2:::valid_funcs

valid_vars = lapply(input_mats, as.matrix)
valid_literals = numeric(0L)
literals_list = list()
parse_expr = make_expr_parser(finalizer = finalizer_index)

N_expr = ~ sum(state)
N = parse_expr(N_expr)
valid_vars = c(valid_vars, list(N = 0))
literals_list = c(literals_list, list(N$valid_literals))
#valid_literals = c(valid_literals, N$valid_literals)

foi_expr = ~ beta * state[1, 0] / N
foi = parse_expr(foi_expr)  # zero-based indexing row 1, column 0
valid_vars = c(valid_vars, list(foi = 0))
literals_list = c(literals_list, list(foi$valid_literals))
#valid_literals = c(valid_literals, foi$valid_literals)

ratemat_expr = ~ matrix(
  c(
    0,   0,     0,
    foi, 0,     0,
    0,   gamma, 0
  ), 3, 3
)
ratemat = parse_expr(ratemat_expr)
valid_vars = c(valid_vars, list(ratemat = matrix(0, 3, 3)))
literals_list = c(literals_list, list(ratemat$valid_literals))
#valid_literals = c(valid_literals, ratemat$valid_literals)

flowmat_expr = ~ ratemat * state ## col_multiply
flowmat = parse_expr(flowmat_expr)
valid_vars = c(valid_vars, list(flowmat = matrix(0, 3, 3)))
literals_list = c(literals_list, list(flowmat$valid_literals))
#valid_literals = c(valid_literals, flowmat$valid_literals)

state_update_expr = ~ state - rowSums(flowmat) + t(colSums(flowmat))
state_update = parse_expr(state_update_expr)
literals_list = c(literals_list, list(state_update$valid_literals))

literal_offsets = c(0, cumsum(vapply(literals_list, length, integer(1L)))[-length(literals_list)])
literals = unlist(literals_list)

mats = lapply(valid_vars, as.matrix)

parse_tables = list(
  N = N$parse_table,
  foi = foi$parse_table,
  ratemat = ratemat$parse_table,
  flowmat = flowmat$parse_table,
  state = state_update$parse_table
)

parse_table_offsets = c(0, cumsum(lapply(parse_tables[-length(parse_tables)], nrow)))

parse_table = setNames(
  as.list(do.call(rbind, parse_tables)),
  c("p_table_x", "p_table_n", "p_table_i")
)
parse_table$p_table_i = unlist(mapply(
  function(ii, y) {
    ii[ii != -1L] = ii[ii != -1L] + y
    ii
  },
  lapply(lapply(parse_tables, getElement, "i"), function(x) x - 1L),
  parse_table_offsets
), use.names = FALSE)
parse_table$p_table_x = unlist(mapply(
  function(xx, nn, y) {
    xx[nn == -1L] = xx[nn == -1L] + y
    xx[nn == 0L] = xx[nn == 0L]
    xx[nn > 0L] = xx[nn > 0L] - 1L
    xx
  },
  lapply(parse_tables, getElement, "x"),
  lapply(parse_tables, getElement, "n"),
  literal_offsets
), use.names = FALSE)
## hack to switch p_table_x to zero-based indexing.
## todo: fix properly in package r code
#parse_table$p_table_x[parse_table$p_table_n > 0L] = parse_table$p_table_x[parse_table$p_table_n > 0L] - 1L



expr_index = list(
  expr_output_count = rep(1L, length(parse_tables)),
  expr_output_id = apply(
    outer(names(parse_tables), names(mats), "=="),
    1L,
    which
  ) - 1L,
  expr_sim_block = rep(0L, length(parse_tables)),
  expr_num_p_table_rows = vapply(
    parse_tables,
    nrow,
    integer(1L),
    USE.NAMES = FALSE
  )
)

## zero expressions before sims
## five expressions during sims
## zero expressions after sims
eval_schedule = c(0, 5, 0)

mats_config = list(
  mats_save_hist = c(
    state = TRUE,
    beta = FALSE, gamma = FALSE,
    N = TRUE, foi = TRUE,
    ratemat = FALSE, flowmat = FALSE
  ),
  mats_return = c(
    state = TRUE,
    beta = FALSE, gamma = FALSE,
    N = TRUE, foi = TRUE,
    ratemat = FALSE, flowmat = FALSE
  )
)

params = c(0.3)
random = numeric(0L)

params_index = list(
  p_par_id = 0,
  p_mat_id = 1,
  p_row_id = 0,
  p_col_id = 0
)

random_index = list(
  r_par_id = integer(0L),
  r_mat_id = integer(0L),
  r_row_id = integer(0L),
  r_col_id = integer(0L)
)

time_steps = c(2) #2L
TMBObjectiveFunction = function(parse_table, parsed_literals, existing_literals) {
  parse_table$i = parse_table$i - 1L
  parse_table$x[parse_table$n == -1L] = parse_table$x[parse_table$n == -1L] + length(existing_literals)
  parse_table$x[parse_table$n > 0L] = parse_table$x[parse_table$n > 0L] - 1L
  list(
    parse_table = as.list(parse_table),
    literals = c(existing_literals, parsed_literals)
  )
}
obj_fn_expr = ~ sum(state)
obj_fn = parse_expr(obj_fn_expr)
obj_fn_obj = TMBObjectiveFunction(obj_fn$parse_table, obj_fn$valid_literals, literals)
literals = obj_fn_obj$literals
obj_fn_parse_table = obj_fn_obj$parse_table

data_args = c(
  list(mats = unname(mats)),
  expr_index,
  nlist(eval_schedule),
  parse_table,
  nlist(literals),
  mats_config,
  params_index,
  random_index,
  nlist(time_steps),
  list(o_table_x = obj_fn_parse_table$x),
  list(o_table_n = obj_fn_parse_table$n),
  list(o_table_i = obj_fn_parse_table$i)
)

parameter_args = nlist(params, random)

print("data args ...")
print(data_args)

print("parameter args ...")
print(parameter_args)

# foi_expr
# cc = c(0, cumsum(data_args$expr_num_p_table_rows))
# j = 2L
# ii = (cc[j] + 1L):cc[j + 1L]
# as.data.frame(parse_table)[ii,]
# which(names(mats) == "beta") - 1L
# names(mats)[1 + 1]
# #    p_table_x p_table_n p_table_i
# # 3          4         2         3  ## division
# # 4          3         2         5  ## multiplication
# # 5          3         0        -1  ## N
# # 6          1         0        -1  ## beta
# # 7         14         3         7  ## extraction
# # 8          0         0        -1  ## state
# # 9          0        -1        -1  ## 0
# # 10         1        -1        -1  ## 1


tmb_function = try(TMB::MakeADFun(
  data_args, parameter_args,
  DLL = 'macpan2'
))

print("correct answer ...")
correct_answer()  ## expected report

print("actual answer ...")
tmb_output = try(tmb_function$report())  ## actual report

tmb_function$fn() ## should be 1
tmb_function$gr() ## should be 0
tmb_function$he() ## should be 0

# correct_answer(0.1)
# tmb_function$report(0.1)
