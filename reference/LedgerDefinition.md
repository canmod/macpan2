# Ledgers

A ledger is a table with rows that identify specific instances of a
functional form used to define a
[`mp_dynamic_model`](https://canmod.github.io/macpan2/reference/mp_dynamic_model.md).
Ledgers are most commonly created using the
[`mp_join`](https://canmod.github.io/macpan2/reference/mp_join.md)
function as in the examples.

## Examples

``` r
age = mp_index(Age = c("young", "old"))
state = mp_cartesian(
  mp_index(Epi = c("S", "I", "R")),
  age
)
mp_join(
  from = mp_subset(state, Epi = "S"),
  to = mp_subset(state, Epi = "I"),
  by = list(from.to = "Age")
)
#>     from      to
#>  S.young I.young
#>    S.old   I.old
```
