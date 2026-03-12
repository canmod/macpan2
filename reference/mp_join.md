# Join Indexes

Join two or more index tables (see
[`mp_index`](https://canmod.github.io/macpan2/reference/mp_index.md)) to
produce a ledger (see
[`LedgerDefinition`](https://canmod.github.io/macpan2/reference/LedgerDefinition.md)).

## Usage

``` r
mp_join(..., by = empty_named_list())
```

## Arguments

- ...:

  Named arguments giving indexes created by
  [`mp_index`](https://canmod.github.io/macpan2/reference/mp_index.md)
  or another function that manipulates indexes. Each argument will
  become a position vector used to subset or expand numeric vectors in
  archetype formulas.

- by:

  What columns to use to join the indexes. See below on how to specify
  this argument.

## Details

When two index tables are passed to `...`, `mp_join` behaves very much
like an ordinary [inner join](https://en.wikipedia.org/wiki/Join_(SQL)).
When more than two tables are passed to `...`, `mp_join` iteratively
joins pairs of tables to produce a final ledger. For example, if index
tables `A` `B`, and `C` are passed to `mp_join`, an inner join of `A`
and `B` is performed and the result is joined with `C`. In each of these
successive internal joins. The properties of inner joins ensures that
the order of tables does not affect the set of rows in the final table
(SW states without proof!).

## See also

Other functions that return ledgers
[`mp_aggregate()`](https://canmod.github.io/macpan2/reference/mp_aggregate.md)

## Examples

``` r
# When two index tables are passed to `...`, the `by` argument is just a
# character vector of column names on which to join (as in standard R functions
# for joining data frames), or the dot-concatenation of these column names.
# For example,
state = mp_index(
  Epi = c("S", "I", "S", "I"),
  Age = c("young", "young", "old", "old")
)
mp_join(
  from = mp_subset(state, Epi = "S"),
  to = mp_subset(state, Epi = "I"),
  by = "Age"
)
#>     from      to
#>  S.young I.young
#>    S.old   I.old
# If there are more than two tables then the `by` argument must be a named
# list of character vectors, each describing how to join the columns of
# a pair of tables in `...`. The names of this list are dot-concatenations
# of the names of pairs of tables in `...`. For example,
rates = mp_index(
  Epi = c("lambda", "lambda"),
  Age = c("young", "old")
)
mp_join(
  from = mp_subset(state, Epi = "S"),
  to = mp_subset(state, Epi = "I"),
  rate = mp_subset(rates, Epi = "lambda"),
  by = list(
    from.to = "Age",
    from.rate = "Age"
  )
)
#>     from      to         rate
#>  S.young I.young lambda.young
#>    S.old   I.old   lambda.old
# If the `by` columns have different names in two tables, then you can
# specify these using formula notation where the left-hand-side
# is a dot-concatenation of columns in the first table and the
# right-hand-side is a dot-concatenation of the columns in the second
# table. For example,
contact = mp_index(
  AgeSusceptible = c("young", "young", "old", "old"),
  AgeInfectious = c("young", "old", "young", "old")
)
mp_join(
  sus = mp_subset(state, Epi = "S"),
  inf = mp_subset(state, Epi = "I"),
  con = contact,
  by = list(
    sus.con = "Age" ~ "AgeSusceptible",
    inf.con = "Age" ~ "AgeInfectious"
  )
)
#>      sus     inf         con
#>  S.young I.young young.young
#>    S.old I.young   old.young
#>  S.young   I.old   young.old
#>    S.old   I.old     old.old
```
