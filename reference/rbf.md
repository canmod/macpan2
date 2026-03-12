# Radial Basis Functions

Compute a set of radial basis functions (`dimension` of them).

## Usage

``` r
rbf(time_steps, dimension, scale = time_steps/dimension)
```

## Arguments

- time_steps:

  number of time steps in the model

- dimension:

  number of gaussians in the basis

- scale:

  width of the gaussians

## Examples

``` r
matplot(rbf(100, 5), type = "l")

```
