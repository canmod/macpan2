library(macpan2)
library(TMB)

compile('macpan2.cpp')
dyn.load(dynlib("macpan2"))

correct_answer = function(beta = 0.3) {

  ## matrices
  state = c(1 - 1e-2, 1e-2, 0)
  gamma = 0.2
  N = 1
  foi = 1
  recrate = 0
  ratemat = matrix(0, 3, 3)
  flowmat = matrix(0, 3, 3)

  state_hist = list(as.matrix(state))
  N_hist = list(as.matrix(N))
  foi_hist = list(as.matrix(foi))
  recrate_hist = list(as.matrix(recrate))
  for (i in 1:2) {
    N = sum(state)
    foi = beta * state[2] / N
    recrate = gamma/foi_hist[[i]]
    ratemat = matrix(
      c(
        0, foi, 0,
        0, 0,   recrate,
        0, 0,   0
      ), 3, 3, byrow = TRUE
    )
    flowmat = sweep(ratemat, 1, state, "*")
    state = state - row_sums(flowmat) + col_sums(flowmat)
    state_hist = c(state_hist, list(as.matrix(state)))
    N_hist = c(N_hist, list(as.matrix(N)))
    foi_hist = c(foi_hist, list(as.matrix(foi)))
    recrate_hist = c(recrate_hist, list(as.matrix(recrate)))
  }
  correct_obj_fn_output = sum(state)
  state = state_hist
  N = N_hist
  foi = foi_hist
  recrate = recrate_hist
  return(
    nlist(state, N, foi, recrate, correct_obj_fn_output)
  )
}

input_mats = list(
  state = c(1 - 1e-2, 1e-2, 0),
  beta = 0.3,
  gamma = 0.2
)

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
valid_vars = c(valid_vars, list(foi = 1))
literals_list = c(literals_list, list(foi$valid_literals))
#valid_literals = c(valid_literals, foi$valid_literals)

recrate_expr = ~ gamma / rbind_lag(foi, 1)
recrate = parse_expr(recrate_expr)
valid_vars = c(valid_vars, list(recrate = 0))
literals_list = c(literals_list, list(recrate$valid_literals))

ratemat_expr = ~ matrix(
  c(
    0,   0,     0,
    foi, 0,     0,
    0,   recrate, 0
  ), 3, 3
)
ratemat = parse_expr(ratemat_expr)
valid_vars = c(valid_vars, list(ratemat = matrix(0, 3, 3)))
literals_list = c(literals_list, list(ratemat$valid_literals))

flowmat_expr = ~ ratemat * state ## col_multiply
flowmat = parse_expr(flowmat_expr)
valid_vars = c(valid_vars, list(flowmat = matrix(0, 3, 3)))
literals_list = c(literals_list, list(flowmat$valid_literals))

state_update_expr = ~ state - row_sums(flowmat) + t(col_sums(flowmat))
state_update = parse_expr(state_update_expr)
literals_list = c(literals_list, list(state_update$valid_literals))

literal_offsets = c(0, cumsum(vapply(literals_list, length, integer(1L)))[-length(literals_list)])
literals = unlist(literals_list)

mats = lapply(valid_vars, as.matrix)

parse_tables = list(
  N = N$parse_table,
  foi = foi$parse_table,
  recrate = recrate$parse_table,
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


eval_schedule = c(0, 6, 0)

mats_config = list(
  mats_save_hist = c(
    state = TRUE,
    beta = FALSE, gamma = FALSE,
    N = TRUE, foi = TRUE,
    recrate = TRUE,
    ratemat = FALSE, flowmat = FALSE
  ),
  mats_return = c(
    state = TRUE,
    beta = FALSE, gamma = FALSE,
    N = TRUE, foi = TRUE,
    recrate = TRUE,
    ratemat = FALSE, flowmat = FALSE
  )
)

params = c(0.3, 0.2)
random = numeric(0L)

params_index = list(
  p_par_id = c(0L, 1L),
  p_mat_id = c(1L, 2L),
  p_row_id = c(0L, 0L),
  p_col_id = c(0L, 0L)
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
obj_fn_expr = ~ sum(rbind_time(foi, 1:2))
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

tmb_function = try(TMB::MakeADFun(
  data_args, parameter_args,
  DLL = 'macpan2'
))

print("correct answer ...")
correct_answer()  ## expected result

print("actual answer ...")
tmb_output = try(tmb_function$report())  ## actual result
tmb_output
tmb_function$fn()
tmb_function$gr()
tmb_function$he()
