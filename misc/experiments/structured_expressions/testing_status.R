library(macpan2)
library(TMB)

compile('macpan2.cpp')
dyn.load(dynlib("macpan2"))

correct_answer = function(){
  state = c(1, 0, 0, 0, 0)
  testing_rate = 0.05
  positive_rate = 0.65
  negitive_rate = 1-positive_rate
  time_till_result = 2
  time_till_expiration = 7
  
  result_rate = 1/time_till_result
  expiration_rate = 1/time_till_expiration
  
  state_hist = list(as.matrix(state))
  
  ratemat = matrix(
    c(
      0, testing_rate, 0, 0, testing_rate,
      0, 0, result_rate*positive_rate, result_rate*negitive_rate, 0,
      expiration_rate, 0, 0, 0, 0,
      expiration_rate, 0, 0, 0, 0,
      0, 0, 0, 0, 0
    ), 5, 5, byrow = TRUE
  )
  
  for(i in 1:2){
    flowmat = sweep(ratemat, 1, state, "*")
    state = state - row_sums(flowmat) + flowmat[,5] + col_sums(flowmat)
    state_hist = c(state_hist, list(as.matrix(state)))
  }
  state = state_hist
  return(nlist(state))
}

input_mats = list(
  state = c(1, 0, 0, 0, 0),
  testing_rate = 0.05,
  positive_rate = 0.65,
  negative_rate = 0.35, #1-positive_rate
  result_rate = 1/2,
  expiration_rate = 1/7
)

# valid_funcs = nlist(
#   `+`, `-`, `*`, `/`, `^`,
#   `(`,
#   `c`, `matrix`,
#   `%*%`, `sum`, `rep`,
#   `row_sums`, `col_sums`,
#   `[`, `t`
# )

valid_funcs = macpan2:::valid_funcs

valid_vars = lapply(input_mats, as.matrix)
valid_literals = numeric(0L)
literals_list = list()
parse_expr = make_expr_parser(finalizer = finalizer_index)

ratemat_in_expr = ~t(matrix(
  c(
    0, testing_rate, 0, 0, testing_rate,
    0, 0, result_rate*positive_rate, result_rate*negative_rate, 0,
    expiration_rate, 0, 0, 0, 0,
    expiration_rate, 0, 0, 0, 0,
    0, 0, 0, 0, 0
  ), 5, 5
))
ratemat_in = parse_expr(ratemat_in_expr)
valid_vars = c(valid_vars, list(ratemat_in=matrix(0, 5, 5)))
literals_list = c(literals_list, list(ratemat_in$valid_literals))

ratemat_out_expr = ~t(matrix(
  c(
    0, testing_rate, 0, 0, 0,
    0, 0, result_rate*positive_rate, result_rate*negative_rate, 0,
    expiration_rate, 0, 0, 0, 0,
    expiration_rate, 0, 0, 0, 0,
    0, 0, 0, 0, 0
  ), 5, 5
))
ratemat_out = parse_expr(ratemat_out_expr)
valid_vars = c(valid_vars, list(ratemat_out=matrix(0, 5, 5)))
literals_list = c(literals_list, list(ratemat_out$valid_literals))

flowmat_in_expr = ~ratemat_in * state
flowmat_in = parse_expr(flowmat_in_expr)
valid_vars = c(valid_vars, list(flowmat_in = matrix(0, 5, 5)))
literals_list = c(literals_list, list(flowmat_in$valid_literals))

flowmat_out_expr = ~ratemat_out * state
flowmat_out = parse_expr(flowmat_out_expr)
valid_vars = c(valid_vars, list(flowmat_out = matrix(0, 5, 5)))
literals_list = c(literals_list, list(flowmat_out$valid_literals))

state_update_expr = ~state - row_sums(flowmat_out) + t(col_sums(flowmat_in))
state_update = parse_expr(state_update_expr)
literals_list = c(literals_list, list(state_update$valid_literals))

literal_offsets = c(0, cumsum(vapply(literals_list, length, integer(1L)))[-length(literals_list)])
literals = unlist(literals_list)

mats = lapply(valid_vars, as.matrix)

parse_tables = list(
  ratemat_in = ratemat_in$parse_table,
  ratemat_out = ratemat_out$parse_table,
  flowmat_in = flowmat_in$parse_table,
  flowmat_out = flowmat_out$parse_table,
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
  mats_save_hist = logical(length(valid_vars)),
  mats_return = logical(length(valid_vars))
)
mats_config$mats_save_hist[match(c("state"), names(valid_vars))] = TRUE
mats_config$mats_return[match(c("state"), names(valid_vars))] = TRUE

params = 1.0
random = numeric(0L)

params_index = list(
  p_par_id = integer(0L),
  p_mat_id = integer(0L),
  p_row_id = integer(0L),
  p_col_id = integer(0L)
)

random_index = list(
  r_par_id = integer(0L),
  r_mat_id = integer(0L),
  r_row_id = integer(0L),
  r_col_id = integer(0L)
)

time_steps = c(2) #2L

data_args = c(
  list(mats = unname(mats)),
  expr_index,
  nlist(eval_schedule),
  parse_table,
  nlist(literals),
  mats_config,
  params_index,
  random_index,
  nlist(time_steps)
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
print(tmb_output)
