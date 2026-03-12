# Structured Vectors

This documentation was originally in
[`mp_index()`](https://canmod.github.io/macpan2/reference/mp_index.md)
and should be cleaned up See issue \#131. Also this is an experimental
feature.

## Usage

``` r
mp_structured_vector(x, ...)

mp_set_numbers(vector, ...)
```

## Arguments

- x:

  An index.

- ...:

  Passed on to S3 methods.

- vector:

  An index.

## Functions

- `mp_set_numbers()`: Update numerical values of a structured vector.
  TODO: details on syntax.

## Examples

``` r
state = mp_index(
  Epi = c("S", "I", "S", "I"),
  Age = c("young", "young", "old", "old")
)
state_vector = (state
  |> mp_structured_vector()
  |> mp_set_numbers(Epi = c(S = 1000))
  |> mp_set_numbers(Epi = c(I = 1), Age = "old")
)
print(state_vector)
#> S.young I.young   S.old   I.old 
#>    1000       0    1000       1 
```
