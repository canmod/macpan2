# Aggregate an Index

Create a one-column ledger (see
[`LedgerDefinition`](https://canmod.github.io/macpan2/reference/LedgerDefinition.md))
with rows identifying instances of an aggregation.

## Usage

``` r
mp_aggregate(index, by = "Group", ledger_column = "group")
```

## Arguments

- index:

  An index to aggregate over.

- by:

  A column set label to group by. By default a dummy and constant
  `"Group"` column is created.

- ledger_column:

  Name of the column in the output ledger that describes the groups.

## See also

Other functions that return ledgers
[`mp_join()`](https://canmod.github.io/macpan2/reference/mp_join.md)
