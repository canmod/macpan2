spec = mp_tmb_model_spec(
    during = list(x ~ A %*% x, fibonacci ~ x[1])
  , after = list(golden_ratio ~ 1 + x[1] / x[0])
  , default = list(
        A = matrix(c(1, 1, 1, 0), 2, 2)
      , x = c(1, 0)
    )
)
