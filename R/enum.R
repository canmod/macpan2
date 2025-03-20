## Auto-generated - do not edit by hand
valid_func_sigs = c(
        "binop,null: `+`(x, y)"
      , "binop,null: `-`(x, y)"
      , "binop,null: `*`(x, y)"
      , "binop,null: `/`(x, y)"
      , "binop,null: `^`(x, y)"
      , "fwrap,null: exp(x)"
      , "fwrap,null: log(x)"
      , "null,null: `(`(...)"
      , "null,null: c(...)"
      , "fwrap,null: matrix(x, i, j)"
      , "binop,null: `%*%`(x, y)"
      , "null,null: sum(...)"
      , "fwrap,null: rep(x, times)"
      , "fwrap,null: row_sums(x)"
      , "fwrap,null: col_sums(x)"
      , "fwrap,null: group_sums(x, f, n)"
      , "null,null: `[`(x, i, j)"
      , "fwrap,fail: block(x, i, j, n, m)"
      , "fwrap,null: t(x)"
      , "fwrap,fail: rbind_time(x, t, t_min)"
      , "fwrap,fail: rbind_lag(x, lag, t_min)"
      , "fwrap,fail: cbind_time(x, t, t_min)"
      , "fwrap,fail: cbind_lag(x, lag, t_min)"
      , "null,null: `:`(from, to)"
      , "fwrap,fail: seq(from, length, by)"
      , "fwrap,fail: convolution(x, k)"
      , "fwrap,null: cbind(...)"
      , "fwrap,null: rbind(...)"
      , "fwrap,fail: time_step(lag)"
      , "fwrap,null: assign(x, i, j, v)"
      , "fwrap,fail: unpack(x, ...)"
      , "fwrap,null: recycle(x, rows, cols)"
      , "fwrap,null: clamp(x, eps)"
      , "fwrap,fail: dpois(observed, simulated)"
      , "fwrap,fail: dnbinom(observed, simulated, over_dispersion)"
      , "fwrap,fail: dnorm(observed, simulated, standard_deviation)"
      , "fwrap,fail: rpois(mean)"
      , "fwrap,fail: rnbinom(mean, over_dispersion)"
      , "fwrap,fail: rnorm(mean, standard_deviation)"
      , "binop,null: `%x%`(x, y)"
      , "fwrap,fail: to_diag(x)"
      , "fwrap,fail: from_diag(x)"
      , "fwrap,fail: time_group(i, change_points)"
      , "fwrap,null: cos(x)"
      , "fwrap,null: print(x)"
      , "fwrap,fail: time_var(x, change_points)"
      , "fwrap,fail: rbinom(size, probability)"
      , "fwrap,fail: reulermultinom(size, rate, delta_t)"
      , "fwrap,null: round(x)"
      , "fwrap,fail: pgamma(q, shape, scale)"
      , "fwrap,null: mean(x)"
      , "fwrap,null: sd(x)"
      , "fwrap,null: proportions(x)"
      , "fwrap,null: last(x)"
      , "fwrap,null: check_finite(x)"
      , "fwrap,fail: dbinom(observed, size, probability)"
)
process_enum = function(x) {
  RE = "(null|fail|binop|fwrap|bwrap|pwrap)[ ]*,[ ]*(null|fail|binop|fwrap|bwrap|pwrap)[ ]*:[ ]*\\`?([^`]*)\\`?\\((.*)(\\,.*)*\\)"
  valid_ids = grepl(RE, x)
  if (!all(valid_ids)) {
    stop("Developer error: Malformed enum in misc/dev/dev.cpp.")
  }
  list(
    symb = sub(RE, "\\1", x),
    num = sub(RE, "\\2", x),
    func = sub(RE, "\\3", x),
    args = lapply(strsplit(sub(RE, "\\4", x), ","), trimws)
  )
}
processed = process_enum(valid_func_sigs)
valid_funcs = setNames(as.list(processed$func), processed$func)
valid_func_args = setNames(processed$args, processed$func)
valid_symb_type = setNames(processed$symb, processed$func)
valid_num_type = setNames(processed$num, processed$func)
valid_funcs = setNames(as.list(valid_funcs), valid_funcs)
