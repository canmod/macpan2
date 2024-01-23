library(macpan2)
dynamic_model = mp_dynamic_model(
  expr_list = ExprList(
    during = list(
        infection ~ beta * S * I / N
      , S ~ S - infection
      , I ~ I + infection
    )
  ),
  unstruc_mats = list(S = 99, I = 1, beta = 0.25, N = 100)
)
