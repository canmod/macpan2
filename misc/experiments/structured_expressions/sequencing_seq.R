library(macpan2)
library(TMB)

compile('macpan2.cpp')
dyn.load(dynlib("macpan2"))

correct_answer = function(){
  state = rep(1/7, 7)
  # absvct = c(1:25, seq(3, 24, 2))
  absvct = seq(1, 98, 2)
  ratevct = absvct/sum(absvct)
  ratemat = matrix(ratevct, 7, 7)
  
  state_hist = list(as.matrix(state))
  for(i in 1:2){
    flowmat = sweep(ratemat, 1, state, "*")
    state = state - rowSums(flowmat) +t(colSums(flowmat))
    state_hist = c(state_hist, list(as.matrix(state)))
  }
  state = state_hist
  return(nlist(state))
}

input_mats = list(
  state = rep(1/7, 7)
)

valid_funcs = macpan2:::valid_funcs

valid_vars = lapply(input_mats, as.matrix)
valid_literals = numeric(0L)
literals_list = list()
parse_expr = make_expr_parser(finalizer = finalizer_index)

# absvct_expr = ~c(1:25, seq(3, 24, 2))
absvct_expr = ~seq(1, 49, 2)
absvct = parse_expr(absvct_expr)
valid_vars = c(valid_vars, list(absvct = rep(0, 49)))
literals_list = c(literals_list, list(absvct$valid_literals))

ratevct_expr = ~absvct/sum(absvct)
ratevct = parse_expr(ratevct_expr)
valid_vars = c(valid_vars, list(ratevct = rep(0, 49)))
literals_list = c(literals_list, list(ratevct$valid_literals))

ratemat_expr = ~matrix(ratevct, 7, 7)
ratemat = parse_expr(ratemat_expr)
valid_vars = c(valid_vars, list(ratemat = matrix(0, 7, 7)))
literals_list = c(literals_list, list(ratemat$valid_literals))

flowmat_expr = ~ratemat*state
flowmat = parse_expr(flowmat_expr)
valid_vars = c(valid_vars, list(flowmat = matrix(0, 7, 7)))
literals_list = c(literals_list, list(flowmat$valid_literals))

state_update_expr = ~state - rowSums(flowmat) + t(colSums(flowmat))
state_update = parse_expr(state_update_expr)
literals_list = c(literals_list, list(state_update$valid_literals))

literal_offsets = c(0, cumsum(vapply(literals_list, length, integer(1L)))[-length(literals_list)])
literals = unlist(literals_list)

mats = lapply(valid_vars, as.matrix)

parse_tables = list(
  absvct = absvct$parse_table,
  ratevct = ratevct$parse_table,
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

eval_schedule = c(3, 2, 0)

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
