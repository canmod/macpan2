library(macpan2)
library(TMB)

compile('macpan2.cpp')
dyn.load(dynlib("macpan2"))

correct_answer = function(beta = 0.3) {

  S = 1 - 1e-2
  I = 1e-2
  R = 0
  gamma = 0.2
  N = 1
  foi = 0

  S_hist = list(as.matrix(S))
  I_hist = list(as.matrix(I))
  R_hist = list(as.matrix(R))
  N_hist = list(as.matrix(N))
  foi_hist = list(as.matrix(foi))
  for (i in 1:2) {
    N = S + I + R
    foi = beta * I / N

    S = S - S * foi
    I = I + S * foi - I * gamma
    R = R + I * gamma

    S_hist = c(S_hist, list(as.matrix(S)))
    I_hist = c(I_hist, list(as.matrix(I)))
    R_hist = c(R_hist, list(as.matrix(R)))
    N_hist = c(N_hist, list(as.matrix(N)))
    foi_hist = c(foi_hist, list(as.matrix(foi)))
  }
  S = S_hist
  I = I_hist
  R = R_hist
  N = N_hist
  foi = foi_hist
  return(
    nlist(S, I, R, N, foi)
  )
}


input_mats = list(
  S = 1 - 1e-2,
  I = 1e-2,
  R = 0,
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

N_expr = ~ S+I+R
N = parse_expr(N_expr)
valid_vars = c(valid_vars, list(N = 0))
literals_list = c(literals_list, list(N$valid_literals))

foi_expr = ~ beta * I / N
foi = parse_expr(foi_expr)  # zero-based indexing row 1, column 0
valid_vars = c(valid_vars, list(foi = 0))
literals_list = c(literals_list, list(foi$valid_literals))

S_update_expr = ~S - S*foi
S_update = parse_expr(S_update_expr)
literals_list = c(literals_list, list(S_update$valid_literals))

I_update_expr = ~I +S*foi - I*gamma
I_update = parse_expr(I_update_expr)
literals_list = c(literals_list, list(I_update$valid_literals))

R_update_expr = ~R + I*gamma
R_update = parse_expr(R_update_expr)
literals_list=c(literals_list, list(R_update$valid_literals))

literal_offsets = c(0, cumsum(vapply(literals_list, length, integer(1L)))[-length(literals_list)])
literals = unlist(literals_list)

mats = lapply(valid_vars, as.matrix)

parse_tables = list(
  N = N$parse_table,
  foi = foi$parse_table,
  S = S_update$parse_table,
  I = I_update$parse_table,
  R = R_update$parse_table
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

eval_schedule = c(0, 5, 0)

mats_config = list(
  mats_save_hist = c(
    S = TRUE, I=TRUE, R=TRUE,
    beta = FALSE, gamma = FALSE,
    N = TRUE, foi = TRUE
  ),
  mats_return = c(
    S = TRUE, I = TRUE, R=TRUE,
    beta = FALSE, gamma = FALSE,
    N = TRUE, foi = TRUE
  )
)

params = c(0.3)
random = numeric(0L)

params_index = list(
  p_par_id = 0,
  p_mat_id = 3,
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



obj_fun_args = ObjectiveFunction(~S + I + R)$data_arg(
  names(mats),
  .existing_literals = literals
)

data_args = c(
  list(mats = unname(mats)),
  expr_index,
  nlist(eval_schedule),
  parse_table,
  mats_config,
  params_index,
  random_index,
  nlist(time_steps),
  obj_fun_args
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
correct_answer()$S  ## expected result

print("actual answer ...")
tmb_output = try(tmb_function$report())  ## actual result
print(tmb_output)

#correct_answer(0.1)
#tmb_function$report(0.1)


















