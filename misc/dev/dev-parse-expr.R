library(macpan2)
# pe = make_expr_parser(parser_name = "pe", finalizer = finalizer_index)
# valid_funcs = macpan2:::valid_funcs
# valid_vars = initial_valid_vars(letters)
# valid_literals = numeric(0L)
# offset = 0L
expr_list = list(
  ~ a + b + 3.14,
  ~ c(t - 2 * 2.18),
  ~ t:e
)
pe_list = list()
for (i in seq_along(expr_list)) {
  pe_list[[i]] = pe(expr_list[[i]])
  valid_literals = pe_list[[i]]$valid_literals
  offset = offset + nrow(pe_list[[i]]$parse_table)
}
do.call(rbind, lapply(pe_list, getElement, "parse_table"))
valid_literals


# > do.call(rbind, lapply(pe_list, getElement, "parse_table"))
#     x  n  i offset
# 1   1  2  1      0  `+`(`+`(a,b),3.14)
# 2   1  2  3      0  `+`(a,b)
# 3   0 -1  0      0  literal = 3.14
# 4   0  0  0      0  matrix = a
# 5   1  0  0      0  matrix = b
# 6   7  1  6      5  c(`-`())
# 7   2  2  7      5  `-`(t,`*`(2,2.18))
# 8  19  0  0      5  matrix = t
# 9   3  2  9      5  `*`
# 10  1 -1  0      5  literal = 2
# 11  2 -1  0      5  literal = 2.18
# 12 20  2 12     11  `:`(t,e)
# 13 19  0  0     11  matrix = t
# 14  4  0  0     11  matrix = e
# > valid_literals
# [1] 3.14 2.00 2.18


parse_expr_list(
  list(
    ~ a + b + 3.14,
    ~ c(t - 2 * 2.18),
    ~ t:e
  ),
  valid_vars = initial_valid_vars(letters)
)


