## Auto-generated - do not edit by hand
valid_func_sigs = c(
        "binop: `+`(x, y)"
      , "binop: `-`(x, y)"
      , "binop: `*`(x, y)"
      , "binop: `/`(x, y)"
      , "binop: `^`(x, y)"
      , "fwrap: exp(x)"
      , "fwrap: log(x)"
      , "paren: `(`(...)"
      , "fwrap: c(...)"
      , "fwrap: matrix(x, i, j)"
      , "binop: `%*%`(x, y)"
      , "fwrap: sum(...)"
      , "fwrap: rep(x, times)"
      , "fwrap: row_sums(x)"
      , "fwrap: col_sums(x)"
      , "fwrap: group_sums(x, f, n)"
      , "paren: `[`(x, i, j)"
      , "fwrap: block(x, i, j, n, m)"
      , "fwrap: t(x)"
      , "fwrap: rbind_time(x, t, t_min)"
      , "fwrap: rbind_lag(x, lag, t_min)"
      , "fwrap: cbind_time(x, t, t_min)"
      , "fwrap: cbind_lag(x, lag, t_min)"
      , "binop: `:`(from, to)"
      , "fwrap: seq(from, length, by)"
      , "fwrap: convolution(x, k)"
      , "fwrap: cbind(...)"
      , "fwrap: rbind(...)"
      , "fwrap: time_step(lag)"
      , "fwrap: recycle(x, rows, cols)"
      , "fwrap: clamp(x, eps)"
      , "fwrap: dpois(observed, simulated)"
      , "fwrap: dnbinom(observed, simulated, over_dispersion)"
      , "fwrap: dnorm(observed, simulated, standard_deviation)"
      , "fwrap: rpois(mean)"
      , "fwrap: rnbinom(mean, over_dispersion)"
      , "fwrap: rnorm(mean, standard_deviation)"
      , "binop: `%x%`(x, y)"
      , "fwrap: to_diag(x)"
      , "fwrap: from_diag(x)"
      , "fwrap: time_group(i, change_points)"
      , "fwrap: cos(x)"
      , "fwrap: print(x)"
      , "fwrap: time_var(x, change_points)"
      , "fwrap: rbinom(size, probability)"
      , "fwrap: reulermultinom(size, rate, delta_t)"
      , "fwrap: round(x)"
      , "fwrap: pgamma(q, shape, scale)"
      , "fwrap: mean(x)"
      , "fwrap: sd(x)"
      , "fwrap: proportions(x)"
      , "fwrap: last(x)"
      , "fwrap: check_finite(x)"
      , "fwrap: dbinom(observed, size, probability)"
      , "fwrap: sin(x)"
      , "fwrap: sqrt(x)"
      , "fwrap: pnorm(q, mean, sd)"
      , "fwrap: invlogit(x)"
      , "fwrap: logit(x)"
      , "fwrap: stop_if_lt(x, y)"
      , "fwrap: cumsum(x)"
      , "fwrap: assign(x, i, j, v)"
      , "fwrap: unpack(x, ...)"
)
bail_out_log_file = ".macpan2/bail-out/log.txt"
process_enum = function(x) {
  RE = "(paren|binop|fwrap)[ ]*:[ ]*\\`?([^`]*)\\`?\\((.*)(\\,.*)*\\)"
  valid_ids = grepl(RE, x)
  if (!all(valid_ids)) {
    stop("Developer error: Malformed enum in misc/dev/dev.cpp.")
  }
  list(
    symb = sub(RE, "\\1", x),
    func = sub(RE, "\\2", x),
    args = lapply(strsplit(sub(RE, "\\3", x), ","), trimws)
  )
}
processed = process_enum(valid_func_sigs)
valid_funcs = setNames(as.list(processed$func), processed$func)
valid_func_args = setNames(processed$args, processed$func)
valid_symb_type = setNames(processed$symb, processed$func)
valid_funcs = setNames(as.list(valid_funcs), valid_funcs)
