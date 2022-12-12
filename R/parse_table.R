
TMBExpressions = function(parse_tables, valid_vars, literals_list) {
  self = Base()
  self$parse_tables = lapply(parse_tables, getElement, "parse_table")
  self$valid_vars = valid_vars
  self$literals_list = literals_list
  self$literal_offsets = c(0, cumsum(vapply(self$literals_list, length, integer(1L)))[-length(self$literals_list)])
  self$parse_table_offsets = c(
    0,
    cumsum(
      lapply(
        self$parse_tables[-length(self$parse_tables)],
        nrow
      )
    )
  )
  self$parse_table = setNames(
    as.list(do.call(rbind, self$parse_tables)),
    c("p_table_x", "p_table_n", "p_table_i")
  )
  self$parse_table$p_table_i = unlist(mapply(
    function(ii, y) {
      ii[ii != -1L] = ii[ii != -1L] + y
      ii
    },
    lapply(lapply(self$parse_tables, getElement, "i"), function(x) x - 1L),
    self$parse_table_offsets,
    SIMPLIFY = FALSE
  ), use.names = FALSE)
  self$parse_table$p_table_x = unlist(mapply(
    function(xx, nn, y) {
      xx[nn == -1L] = xx[nn == -1L] + y
      xx[nn == 0L] = xx[nn == 0L]
      xx[nn > 0L] = xx[nn > 0L] - 1L
      xx
    },
    lapply(self$parse_tables, getElement, "x"),
    lapply(self$parse_tables, getElement, "n"),
    self$literal_offsets,
    SIMPLIFY = FALSE
  ), use.names = FALSE)
## hack to switch p_table_x to zero-based indexing.
## todo: fix properly in package r code
#parse_table$p_table_x[parse_table$p_table_n > 0L] = parse_table$p_table_x[parse_table$p_table_n > 0L] - 1L

  self$expr_output_count = rep(1L, length(self$parse_tables))
  self$expr_output_id = apply(
    outer(names(self$parse_tables), names(self$valid_vars), "=="),
    1L,
    which
  ) - 1L
  self$expr_sim_block = rep(0L, length(self$parse_tables))
  self$expr_num_p_table_rows = vapply(
    self$parse_tables,
    nrow,
    integer(1L),
    USE.NAMES = FALSE
  )
  return_object(self, "TMBExpressions")
}


TMBObjectiveFunction = function(parse_table, parsed_literals, existing_literals) {
  self = Base()
  parse_table$i = parse_table$i - 1L
  parse_table$x[parse_table$n == -1L] = parse_table$x[parse_table$n == -1L] + length(existing_literals)
  parse_table$x[parse_table$n > 0L] = parse_table$x[parse_table$n > 0L] - 1L
  self$parse_table = as.list(parse_table)
  self$literals = c(existing_literals, parsed_literals)
  return_object(self, "TMBObjectiveFunction")
}
