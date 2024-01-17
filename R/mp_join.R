#' Join Indexes
#'
#' Join two or more index tables (see \code{\link{mp_index}}) to produce a
#' ledger (see \code{\link{LedgerDefinition}}).
#'
#' When two index tables are passed to `...`, `mp_join` behaves very much like
#' an ordinary [inner join](https://en.wikipedia.org/wiki/Join_(SQL)).
#' When more than two tables are passed to `...`, `mp_join` iteratively joins
#' pairs of tables to produce a final ledger. For example, if index tables `A`
#' `B`, and `C` are passed to `mp_join`, an inner join of `A` and `B` is
#' performed and the result is joined with `C`. In each of these successive
#' internal joins. The properties of inner
#' joins ensures that the order of tables does not affect the set of rows in
#' the final table (SW states without proof!).
#'
#' When two index tables are passed to `...`, the `by` argument is just a
#' character vector of column names on which to join (as in standard R functions
#' for joining data frames), or the dot-concatenation of these column names.
#' For example,
#' ```{r, echo = TRUE, eval = TRUE}
#' state = mp_index(
#'   Epi = c("S", "I", "S", "I"),
#'   Age = c("young", "young", "old", "old")
#' )
#' mp_join(
#'   from = mp_subset(state, Epi = "S"),
#'   to = mp_subset(state, Epi = "I"),
#'   by = "Age"
#' )
#' ```
#' If there are more than two tables then the `by` argument must be a named
#' list of character vectors, each describing how to join the columns of
#' a pair of tables in `...`. The names of this list are dot-concatenations
#' of the names of pairs of tables in `...`. For example,
#' ```{r, echo = TRUE, eval = TRUE}
#' rates = mp_index(
#'   Epi = c("lambda", "lambda"),
#'   Age = c("young", "old")
#' )
#' mp_join(
#'   from = mp_subset(state, Epi = "S"),
#'   to = mp_subset(state, Epi = "I"),
#'   rate = mp_subset(rates, Epi = "lambda"),
#'   by = list(
#'     from.to = "Age",
#'     from.rate = "Age"
#'   )
#' )
#' ```
#' If the `by` columns have different names in two tables, then you can
#' specify these using formula notation where the left-hand-side
#' is a dot-concatenation of columns in the first table and the
#' right-hand-side is a dot-concatenation of the columns in the second
#' table. For example,
#' ```{r}
#' contact = mp_index(
#'   AgeSusceptible = c("young", "young", "old", "old"),
#'   AgeInfectious = c("young", "old", "young", "old")
#' )
#' mp_join(
#'   sus = mp_subset(state, Epi = "S"),
#'   inf = mp_subset(state, Epi = "I"),
#'   con = contact,
#'   by = list(
#'     sus.con = "Age" ~ "AgeSusceptible",
#'     inf.con = "Age" ~ "AgeInfectious"
#'   )
#' )
#' ```
#'
#' @param ... Named arguments giving indexes created by
#' \code{\link{mp_index}} or another function that manipulates indexes.
#' Each argument will become a position vector used to subset
#' or expand numeric vectors in archetype formulas.
#' @param by What columns to use to join the indexes. See below on
#' how to specify this argument.
#'
#' @family ledgers
#' @export
mp_join = function(..., by = empty_named_list()) {
  table_list = valid$named_list$assert(list(...))
  table_nms = names(table_list)

  if (length(table_nms) < 2L) stop("cannot join fewer than two index objects.")
  if (is.character(by)) {
    if (length(table_nms) != 2L) {
      stop("joining more than one index requires a list-valued by argument.")
    }
    by = setNames(list(by), to_name(table_nms))
  }

  by_list = valid$named_list$assert(by)
  if (length(by_list) > 1L) {
    table_order = (by_list
      |> names()
      |> lapply(to_names)
      |> unlist()
      |> unique()
      |> union(names(table_list))
    )
  } else {
    table_order = names(table_list)
  }
  ordered_table_list = table_list[table_order]

  by_nms = names(by_list) |> strsplit(".", fixed = TRUE)
  good_by_nms = (by_nms
    |> lapply(`%in%`, table_nms)
    |> vapply(all, logical(1L))
  )
  if (any(!good_by_nms)) {
    msg_break(
      msg_colon(
        "Indices to join were given the following names for the join output",
        table_nms
      ),
      msg_colon(
        msg(
          "But the names of arguments that specify what columns will be",
          "The following arguments were supplied to",
          "determine what columns to join on"
        ),
        msg_indent_break(by_nms[!good_by_nms])
      ),
    ) |> stop()
  }
  if (!is.null(table_nms)) {
    for (nm in names(ordered_table_list)) {
      if (nm != "" & inherits(ordered_table_list[[nm]], "Index")) {
        ordered_table_list[[nm]] = mp_choose(ordered_table_list[[nm]], nm)
      }
    }
  }
  orig_tab_nms = (ordered_table_list
    |> method_apply("table_names")
    |> unname()
    |> unlist(recursive = FALSE)
  )
  table_nm_diffs = (by_nms
    |> lapply(factor, levels = orig_tab_nms)
    |> lapply(as.integer)
    |> vapply(diff, integer(1L))
  )

  bad_by_args = table_nm_diffs < 1L ## table names in the wrong order
  if (any(bad_by_args)) {
    fixed_by_nms = (by_nms[bad_by_args]
      |> lapply(rev)
      |> lapply(paste0, collapse = ".")
    )
    fixed_by_args = (by_list[bad_by_args]
      |> lapply(swap_sides)
    )
    by_list[bad_by_args] = fixed_by_args
    names(by_list)[bad_by_args] = fixed_by_nms
  }

  z = ordered_table_list[[1L]]
  for (i in 2:length(ordered_table_list)) {
    args = c(
      list(
        x = z,
        y = ordered_table_list[[i]]
      ),
      by_list
    )
    z = do.call(merge_generic_by_util, args)
  }
  z$reorder(names(table_list))
}
