Fibonacci Numbers
================
Steve Walker

-   <a href="#packages-used-and-settings"
    id="toc-packages-used-and-settings">Packages Used and Settings</a>
-   <a href="#model-specification" id="toc-model-specification">Model
    Specification</a>
-   <a href="#dynamics" id="toc-dynamics">Dynamics</a>
-   <a href="#simulation" id="toc-simulation">Simulation</a>

This example demonstrates that `macpan2` can simulate any discrete-time
dynamical model—not just compartmental models. Use the `~` operator in
the `during` list to define how model variables are updated at each time
step. For example in this article, we generate the [Fibonacci
sequence](https://en.wikipedia.org/wiki/Fibonacci_sequence) using its
[matrix-based dynamical
form](https://en.wikipedia.org/wiki/Fibonacci_sequence#Matrix_form).

# Packages Used and Settings

The code in this article uses the following packages.

``` r
library(dplyr)
library(macpan2)
```

# Model Specification

This model has been specified in the `fibonacci` directory
[here](https://github.com/canmod/macpan2/blob/main/inst/starter_models/fibonacci/tmb.R)
and is accessible from the `macpan2` model library (see [Example
Models](https://canmod.github.io/macpan2/articles/example_models.html)
for details). We can read in the model specification using the
`mp_tmb_library` command.

``` r
spec = mp_tmb_library(
    "starter_models"
  , "fibonacci"
  , package = "macpan2"
)
```

This model contains two variables – a matrix `A` and vector `x`.

``` r
mp_default_list(spec)
#> $x
#> [1] 1 0
#> 
#> $A
#>      [,1] [,2]
#> [1,]    1    1
#> [2,]    1    0
```

At every iteration of the simulation we multiply `A` by `x` to get a new
value of `x`. Then we pick out the next Fibonacci number as the second
(zero-based) element of `x`.

``` r
mp_print_during(spec)
#> ---------------------
#> At every iteration of the simulation loop (t = 1 to T):
#> ---------------------
#> 1: x ~ A %*% x
#> 2: fibonacci ~ x[1]
```

By the end of the simulation loop, if we run enough iterations, we
compute a very good approximation to the golden ratio.

``` r
mp_print_after(spec)
#> ---------------------
#> After the simulation loop (t = T + 1):
#> ---------------------
#> 1: golden_ratio ~ 1 + x[1]/x[0]
```

# Dynamics

Here is the mathematical description of this model.

$$
\begin{pmatrix} 
x_0(t+1) \\ 
x_1(t+1) 
\end{pmatrix}
= \begin{pmatrix} 
1 & 1 \\ 
1 & 0 
\end{pmatrix}
\begin{pmatrix} 
x_0(t) \\ 
x_1(t) 
\end{pmatrix}
$$

# Simulation

The Fibonacci numbers are given by the following simulation.

``` r
(spec 
  |> mp_simulator(
      time_steps = 10
    , output = "fibonacci"
  ) 
  |> mp_trajectory()
  |> pull(value)
)
#>  [1]  1  1  2  3  5  8 13 21 34 55
```

And by running the simulation for longer we can get a very good
approximation to the golden ratio.

``` r
(spec 
  |> mp_simulator(
      time_steps = 100
    , output = "golden_ratio"
  ) 
  |> mp_final()
  |> pull(value)
)
#> [1] 1.618034
```
