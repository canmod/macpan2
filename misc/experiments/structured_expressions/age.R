library(macpan2)
library(TMB)

compile('macpan2.cpp')
dyn.load(dynlib("macpan2"))

correct_answer = function(){
  number_of_groups = 5
  life_expectancy = 95
  population = 1
  
  ageing_rate = 5/(95*365)
  birth_rate = 0.01
  death_rate = 0.01
  
  state = rep(population/number_of_groups, number_of_groups)
  state_hist = list(as.matrix(state))
  
  ratemat = matrix(0, number_of_groups, number_of_groups)
  for(i in 1:(number_of_groups-1)){
    ratemat[i, i+1] = ageing_rate
  }
  
  for(i in 1:2){
  flowmat = sweep(ratemat, 1, state, "*")
  state = state - rowSums(flowmat) + colSums(flowmat)
  state[1] = state[1]+sum(state)*birth_rate
  state[number_of_groups] = state[number_of_groups] - state[number_of_groups]*death_rate
  
  state_hist = c(state_hist, list(as.matrix(state)))
  }
  
  return(nlist(state_hist))
}

population = 1
number_of_groups = 5# minimum of 3
life_expectancy = 95
ageing_rate = number_of_groups/(life_expectancy*365)
birth_rate = 0.01
death_rate = 0.01
state = rep(population/number_of_groups, number_of_groups)

input_mats = list(
  number_of_groups = number_of_groups,#minimum of 3
  life_expectancy = life_expectancy,
  population = population,
  ageing_rate = ageing_rate,
  birth_rate = birth_rate,
  death_rate = death_rate,
  state = state
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


ratemat_rhs = matrix(0, number_of_groups, number_of_groups)
for(i in 1:(number_of_groups-1)){
  ratemat_rhs[i, i+1] = ageing_rate
}
valid_vars = c(valid_vars, list(ratemat_rhs=as.matrix(ratemat_rhs)))

ratemat_expr = ~ratemat_rhs
ratemat = parse_expr(ratemat_expr)
valid_vars = c(valid_vars, list(ratemat = matrix(0, number_of_groups, number_of_groups)))
literals_list = c(literals_list, list(ratemat$valid_literals))

flowmat_expr = ~ratemat * state
flowmat = parse_expr(flowmat_expr)
valid_vars = c(valid_vars, list(flowmat = matrix(0, number_of_groups, number_of_groups)))
literals_list = c(literals_list, list(flowmat$valid_literals))

vital_dynamics_rhs = c(birth_rate, rep(0, number_of_groups-1))*sum(state) - c(rep(0, number_of_groups-1), death_rate)*state[number_of_groups-1]
valid_vars = c(valid_vars, list(vital_dynamics_rhs = vital_dynamics_rhs))

vital_dynamics_expr = ~vital_dynamics_rhs
vital_dynamics = parse_expr(vital_dynamics_expr)
valid_vars = c(valid_vars, list(vital_dynamics = rep(0, number_of_groups)))
literals_list = c(literals_list, list(vital_dynamics$valid_literals))

state_update_expr = ~ state - rowSums(flowmat) + t(colSums(flowmat)) + vital_dynamics
state_update = parse_expr(state_update_expr)
literals_list = c(literals_list, list(state_update$valid_literals))

literal_offsets = c(0, cumsum(vapply(literals_list, length, integer(1L)))[-length(literals_list)])
literals = unlist(literals_list)

mats = lapply(valid_vars, as.matrix)

parse_tables = list(
  ratemat = ratemat$parse_table,
  flowmat = flowmat$parse_table,
  vital_dynamics = vital_dynamics$parse_table,
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

eval_schedule = c(0, 4, 0)

mats_config = list(
  mats_save_hist = logical(length(valid_vars)),
  mats_return = logical(length(valid_vars))
)
mats_config$mats_save_hist[match(c("state"), names(valid_vars))] = TRUE
mats_config$mats_return[match(c("state"), names(valid_vars))] = TRUE

params = 0.1
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

