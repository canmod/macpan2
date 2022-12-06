library(macpan2)
library(TMB)

compile('macpan2.cpp')
dyn.load(dynlib("macpan2"))

correct_answer = function(){

  # S = 1-1e-2
  # E = 1e-2
  # Ia = 0
  # Ip = 0
  # Im = 0
  # Is = 0
  # ICUs = 0
  # ICUd = 0
  # H = 0
  # H2 = 0
  # R = 0
  # D = 0
  state = c(1-1e-2, 1e-2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

  alpha = 0.39
  beta0 = 1
  Ca = 2/3
  Cp = 1
  Cm = 1
  Cs = 1
  gamma_a = 1/7
  gamma_p = 1/7
  gamma_m = 1/7
  gamma_s = 1/7
  phi1 = 0.76
  phi2 = 0.26
  psi1 = 1/20
  psi2 = 1/8
  psi3 = 1/5
  rho = 1/10
  mu = 0.956
  sigma = 1/3
  iso_m = 0
  iso_s = 0
  nonhosp_mort = 0

  N = 1
  foi = 0

  state_hist = list(as.matrix(state))
  N_hist = list(as.matrix(N))
  foi_hist = list(as.matrix(foi))


  ratemat = matrix(0, 12, 12)
  flowmat = matrix(0, 12, 12)

  EIa = alpha*sigma
  EIp = (1-alpha)*sigma
  IaR = gamma_a
  IpIm = mu*gamma_p
  IpIs = (1-mu)*gamma_p
  ImR = gamma_m
  IsH = (1-nonhosp_mort)*phi1*gamma_s
  IsICUs = (1-nonhosp_mort)*(1-phi1)*(1-phi2)*gamma_s
  IsICUd = (1-nonhosp_mort)*(1-phi1)*phi2*gamma_s
  IsD = nonhosp_mort*gamma_s
  ICUsH2 = psi1
  ICUdD = psi2
  H2R = psi3
  HR = rho


  for(i in 1:2){
    N = sum(head(state, -1))
    foi = state[3] * (beta0) * (1 / N) * (Ca) +
          state[4] * (beta0) * (1 / N) * (Cp) +
          state[5] * (beta0) * (1 / N) * (Cm) * (1 - iso_m) +
          state[6] * (beta0) * (1 / N) * (Cs) * (1 - iso_s)
    ratemat = matrix(
      c(
        0, foi, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, EIa, EIp, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, IaR, 0,
        0, 0, 0, 0, IpIm, IpIs, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ImR, 0,
        0, 0, 0, 0, 0, 0, IsICUs, IsICUd, IsH, 0, 0, IsD,
        0, 0, 0, 0, 0, 0, 0, 0, 0, ICUsH2, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ICUdD,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, HR, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, H2R, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
      ), 12, 12, byrow = TRUE
    )

    flowmat = sweep(ratemat, 1, state, "*")
    state = state - rowSums(flowmat) + colSums(flowmat)
    state_hist = c(state_hist, list(as.matrix(state)))
    N_hist = c(N_hist, list(as.matrix(N)))
    foi_hist = c(foi_hist, list(as.matrix(foi)))

  }

  state = state_hist
  N = N_hist
  foi = foi_hist
  return(
    nlist(state, N, foi)
  )
}

input_mats = list(
  state = c(1-1e-2, 1e-2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  alpha = 0.39,
  beta0 = 1,
  Ca = 2/3,
  Cp = 1,
  Cm = 1,
  Cs = 1,
  gamma_a = 1/7,
  gamma_p = 1/7,
  gamma_m = 1/7,
  gamma_s = 1/7,
  phi1 = 0.76,
  phi2 = 0.26,
  psi1 = 1/20,
  psi2 = 1/8,
  psi3 = 1/5,
  rho = 1/10,
  mu = 0.956,
  sigma = 1/3,
  iso_m = 0,
  iso_s = 0,
  nonhosp_mort = 0
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

EIa_expr = ~alpha*sigma
EIa = parse_expr(EIa_expr)
valid_vars = c(valid_vars, list(EIa = 0))
literals_list = c(literals_list, list(EIa$valid_literals))

EIp_expr = ~(1-alpha)*sigma
EIp = parse_expr(EIp_expr)
valid_vars = c(valid_vars, list(EIp = 0))
literals_list = c(literals_list, list(EIp$valid_literals))

IaR_expr = ~gamma_a
IaR = parse_expr(IaR_expr)
valid_vars = c(valid_vars, list(IaR = 0))
literals_list = c(literals_list, list(IaR$valid_literals))

IpIm_expr = ~mu*gamma_p
IpIm = parse_expr(IpIm_expr)
valid_vars = c(valid_vars, list(IpIm = 0))
literals_list = c(literals_list, list(IpIm$valid_literals))

IpIs_expr = ~(1-mu)*gamma_p
IpIs = parse_expr(IpIs_expr)
valid_vars = c(valid_vars, list(IpIs = 0))
literals_list = c(literals_list, list(IpIs$valid_literals))

ImR_expr = ~gamma_m
ImR = parse_expr(ImR_expr)
valid_vars = c(valid_vars, list(ImR = 0))
literals_list = c(literals_list, list(ImR$valid_literals))


IsH_expr = ~(1-nonhosp_mort)*phi1*gamma_s
IsH = parse_expr(IsH_expr)
valid_vars = c(valid_vars, list(IsH = 0))
literals_list = c(literals_list, list(IsH$valid_literals))

IsICUs_expr = ~(1-nonhosp_mort)*(1-phi1)*(1-phi2)*gamma_s
IsICUs = parse_expr(IsICUs_expr)
valid_vars = c(valid_vars, list(IsICUs = 0))
literals_list = c(literals_list, list(IsICUs$valid_literals))

IsICUd_expr = ~(1-nonhosp_mort)*(1-phi1)*phi2*gamma_s
IsICUd = parse_expr(IsICUd_expr)
valid_vars = c(valid_vars, list(IsICUd = 0))
literals_list = c(literals_list, list(IsICUd$valid_literals))

IsD_expr = ~nonhosp_mort*gamma_s
IsD = parse_expr(IsD_expr)
valid_vars = c(valid_vars, list(IsD = 0))
literals_list = c(literals_list, list(IsD$valid_literals))

ICUsH2_expr = ~psi1
ICUsH2 = parse_expr(ICUsH2_expr)
valid_vars = c(valid_vars, list(ICUsH2 = 0))
literals_list = c(literals_list, list(ICUsH2$valid_literals))

ICUdD_expr = ~psi2
ICUdD = parse_expr(ICUdD_expr)
valid_vars = c(valid_vars, list(ICUdD = 0))
literals_list = c(literals_list, list(ICUdD$valid_literals))

H2R_expr = ~psi3
H2R = parse_expr(H2R_expr)
valid_vars = c(valid_vars, list(H2R = 0))
literals_list = c(literals_list, list(H2R$valid_literals))

HR_expr = ~rho
HR = parse_expr(HR_expr)
valid_vars = c(valid_vars, list(HR = 0))
literals_list = c(literals_list, list(HR$valid_literals))

# --- simulation loop

# expr 15 (1-based)
N_expr = ~ sum(state)-state[11, 0]
N = parse_expr(N_expr)
valid_vars = c(valid_vars, list(N = 0))
literals_list = c(literals_list, list(N$valid_literals))

# expr 16 (1-based)
foi_expr = ~ state[2, 0] * beta0 * Ca / N  +
  state[3, 0] * beta0 * Cp / N +
  state[4, 0] * beta0 * Cm / N * (1 - iso_m) +
  state[5, 0] * beta0 * Cs / N  * (1 - iso_s)
foi = parse_expr(foi_expr)  # zero-based indexing row 1, column 0
valid_vars = c(valid_vars, list(foi = 0))
literals_list = c(literals_list, list(foi$valid_literals))

ratemat_expr = ~t(matrix(
  c(
    0, foi, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, EIa, EIp, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, IaR, 0,
    0, 0, 0, 0, IpIm, IpIs, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ImR, 0,
    0, 0, 0, 0, 0, 0, IsICUs, IsICUd, IsH, 0, 0, IsD,
    0, 0, 0, 0, 0, 0, 0, 0, 0, ICUsH2, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ICUdD,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, HR, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, H2R, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  ), 12, 12
))
ratemat = parse_expr(ratemat_expr)
valid_vars = c(valid_vars, list(ratemat = matrix(0, 12, 12)))
literals_list = c(literals_list, list(ratemat$valid_literals))

flowmat_expr = ~ratemat * state
flowmat = parse_expr(flowmat_expr)
valid_vars = c(valid_vars, list(flowmat = matrix(0, 12, 12)))
literals_list = c(literals_list, list(flowmat$valid_literals))

state_update_expr = ~ state - rowSums(flowmat) + t(colSums(flowmat))
state_update = parse_expr(state_update_expr)
literals_list = c(literals_list, list(state_update$valid_literals))

literal_offsets = c(0, cumsum(vapply(literals_list, length, integer(1L)))[-length(literals_list)])
literals = unlist(literals_list)

mats = lapply(valid_vars, as.matrix)

parse_tables = list(
  EIa = EIa$parse_table,
  EIp = EIp$parse_table,
  IaR = IaR$parse_table,
  IpIm = IpIm$parse_table,
  IpIs = IpIs$parse_table,
  ImR = ImR$parse_table,
  IsH = IsH$parse_table,
  IsICUs = IsICUs$parse_table,
  IsICUd = IsICUd$parse_table,
  IsD = IsD$parse_table,
  ICUsH2 = ICUsH2$parse_table,
  ICUdD = ICUdD$parse_table,
  H2R = H2R$parse_table,
  HR = HR$parse_table,
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

eval_schedule = c(14, 5, 0)

mats_config = list(
  mats_save_hist = logical(length(valid_vars)),
  mats_return = logical(length(valid_vars))
)
mats_config$mats_save_hist[match(c("N", "foi", "state"), names(valid_vars))] = TRUE
mats_config$mats_return[match(c("N", "foi", "state"), names(valid_vars))] = TRUE

params = 1.0
random = numeric(0L)

params_index = list(
  p_par_id = 0L,
  p_mat_id = 2L,
  p_row_id = 0L,
  p_col_id = 0L
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

