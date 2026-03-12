# Cartesian Product of Index Tables

Produce a new index table by taking all possible pairwise combinations
of the input tables. This is useful for producing product models that
expand model components through stratification.

## Usage

``` r
mp_cartesian(...)
```

## Arguments

- ...:

  Index tables (see
  [`mp_index`](https://canmod.github.io/macpan2/reference/mp_index.md)).

## See also

Other functions that return index tables
[`mp_index()`](https://canmod.github.io/macpan2/reference/mp_index.md),
[`mp_rename()`](https://canmod.github.io/macpan2/reference/mp_rename.md),
[`mp_subset()`](https://canmod.github.io/macpan2/reference/mp_subset.md),
[`mp_union()`](https://canmod.github.io/macpan2/reference/mp_union.md)

Other functions that take products of index tables and return one index
tables
[`mp_linear()`](https://canmod.github.io/macpan2/reference/mp_linear.md),
[`mp_square()`](https://canmod.github.io/macpan2/reference/mp_square.md),
[`mp_symmetric()`](https://canmod.github.io/macpan2/reference/mp_symmetric.md),
[`mp_triangle()`](https://canmod.github.io/macpan2/reference/mp_triangle.md)

## Examples

``` r
mp_cartesian(
  mp_index(Epi = c("S", "I")),
  mp_index(Age = c("young", "old"))
)
#>  Epi   Age
#>    S young
#>    I young
#>    S   old
#>    I   old

si = mp_index(Epi = c("S", "I"))
age = mp_index(Age = c("young", "old"))
loc = mp_index(City = c("hamilton", "toronto"))
vax = mp_index(Vax = c("unvax", "vax"))
(si
  |> mp_cartesian(age)
  |> mp_cartesian(loc)
  |> mp_cartesian(vax)
)
#>  Epi   Age     City   Vax
#>    S young hamilton unvax
#>    I young hamilton unvax
#>    S   old hamilton unvax
#>    I   old hamilton unvax
#>    S young  toronto unvax
#>    I young  toronto unvax
#>    S   old  toronto unvax
#>    I   old  toronto unvax
#>    S young hamilton   vax
#>    I young hamilton   vax
#>    S   old hamilton   vax
#>    I   old hamilton   vax
#>    S young  toronto   vax
#>    I young  toronto   vax
#>    S   old  toronto   vax
#>    I   old  toronto   vax

flow_rates = mp_index(Epi = c("infection", "recovery"))
mp_union(
  mp_cartesian(
    mp_subset(flow_rates, Epi = "infection"),
    age
  ),
  mp_subset(flow_rates, Epi = "recovery")
)
#>        Epi   Age
#>  infection young
#>  infection   old
#>   recovery      
```
