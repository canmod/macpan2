# Extract Sparse Matrix Notation from a Dense Matrix

Converts a dense matrix to a sparse representation by extracting its
non-zero entries and their indices. Entries with absolute value less
than or equal to `tol` are treated as zeros.

## Usage

``` r
sparse_matrix_notation(M, zero_based = TRUE, tol = 1e-04)
```

## Arguments

- M:

  A numeric matrix or object coercible to a matrix.

- zero_based:

  Logical; if `TRUE` (default), the returned row and column indices are
  zero-based (starting at 0). If `FALSE`, indices are one-based (as in
  standard R matrices).

- tol:

  Numeric tolerance (default `1e-4`). Entries with absolute value less
  than or equal to `tol` are treated as zero.

## Value

A named list with components:

- `row_index`:

  Integer vector of row indices for non-zero entries (adjusted by
  `zero_based`).

- `col_index`:

  Integer vector of column indices for non-zero entries (adjusted by
  `zero_based`).

- `values`:

  Numeric vector of the non-zero entries.

- `M`:

  The original input matrix, coerced to a dense matrix.

- `Msparse`:

  A copy of `M` with near-zero entries (as determined by `tol`) replaced
  by exact zeros.

## Examples

``` r
M <- matrix(c(5, 0, 0,
              0, 0, 3,
              0, 2, 0), nrow = 3, byrow = TRUE)
sparse_matrix_notation(M)
#> $row_index
#> [1] 0 1 2
#> 
#> $col_index
#> [1] 0 2 1
#> 
#> $values
#> [1] 5 3 2
#> 
#> $M
#>      [,1] [,2] [,3]
#> [1,]    5    0    0
#> [2,]    0    0    3
#> [3,]    0    2    0
#> 
#> $Msparse
#>      [,1] [,2] [,3]
#> [1,]    5    0    0
#> [2,]    0    0    3
#> [3,]    0    2    0
#> 
```
