# String Data

Create objects for representing names and labels in a dynamical model.

## Usage

``` r
StringDataFromFrame(data)

StringDataFromDotted(labels, name)

# S3 method for class 'StringData'
print(x, ...)
```

## Arguments

- data:

  Data frame with names given by column names and labels by the elements
  of the columns.

- labels:

  Character vector with (dot-separated) partition labels.

- name:

  Character scalar with (dot-separated) partition name.

- x:

  `StringData` object

- ...:

  Not used but present for S3 method consistency.

## Methods (by generic)

- `print(StringData)`: Print out a `StringData` object.

## Functions

- `StringDataFromFrame()`: Construct object from a data frame without
  any dots in either the names or the values.

- `StringDataFromDotted()`: Construct object from a character scalar
  with (dot-separated) partition names and a character vector with
  (dot-separated) partition labels.

## Examples

``` r
vars = (mp_cartesian(
      mp_index(Epi = c("S", "I", "R"))
    , mp_index(Age = c("young", "old"))
  )
  |> as.data.frame()
  |> StringDataFromFrame()
)
vars
#> String data object with the following $frame():
#>   Epi   Age
#> 1   S young
#> 2   I young
#> 3   R young
#> 4   S   old
#> 5   I   old
#> 6   R   old
vars$dot()
#> String data object with the following $frame():
#>   Epi.Age
#> 1 S.young
#> 2 I.young
#> 3 R.young
#> 4   S.old
#> 5   I.old
#> 6   R.old
```
