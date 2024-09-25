HIV
================
Steve Walker

- [States](#states)
- [Parameters](#parameters)
- [Force-of-Infection](#force-of-infection)
- [Packages Used](#packages-used)
- [Dynamics](#dynamics)
- [Solving the ODEs](#solving-the-odes)
- [Model Specification](#model-specification)
- [References](#references)

This is a description of a `macpan2` implementation of the HIV model
proposed by ([Granich et al. 2009](#ref-granich2009universal)). Default
parameter estimates
[here](https://github.com/canmod/macpan2/blob/main/inst/starter_models/hiv/tmb.R)
are inspired by ([Kretzschmar et al.
2013](#ref-kretzschmar2013prospects)).

# States

| Variable | Description                                                                                                        |
|----------|--------------------------------------------------------------------------------------------------------------------|
| S        | Number of susceptible individuals.                                                                                 |
| I1 - I4  | Numbers of infectious individuals who are not treated. The number gives the state of disease progression from 1-4. |
| A1 - A4  | Numbers of infectious individuals who are treated. The number gives the state of disease progression from 1-4.     |
| D        | Number of individuals who have died from the disease.                                                              |
| D_bg     | Number of individuals who have died from other causes.                                                             |

# Parameters

| Variable    | Description                                                                                                     |
|-------------|-----------------------------------------------------------------------------------------------------------------|
| $\lambda_0$ | Baseline transmission rate.                                                                                     |
| $\alpha$    | Constant in non-linear transmission rate, accounting for heterogeneity in sexual behaviour.                     |
| $n$         | Constant in non-linear transmission rate, accounting for heterogeneity in sexual behaviour.                     |
| $\epsilon$  | Constant, in non-linear transmission rate, measuring the relative decrease in transmission caused by treatment. |
| $\beta$     | Per-capita birth rate.                                                                                          |
| $\mu$       | Per-capita (background) death rate.                                                                             |
| $\rho$      | Per-capita rate of disease progression for non-treated individuals.                                             |
| $\sigma$    | Per-capita rate of disease progression for treated individuals.                                                 |
| $\tau$      | Per-capita rate at which individuals become protected.                                                          |
| $\phi$      | Per-capita rate at which individuals become unprotected.                                                        |

# Force-of-Infection

This model has the somewhat non-standard functional form for the
force-of-infection (per-capita transition rate from `S` to `I1`),
$\frac{\lambda J}{N}$ where $\lambda = \lambda_0 e^{-\alpha P^n}$,
$P = I/N$, $I = \sum_i(I_i + A_i)$, $J = \sum_i(I_i + \epsilon A_i)$,
and $N$ is the total number of alive boxes.

The rest of the transition rates are constant per-capita rates.

# Packages Used

The code in this article uses the following packages.

``` r
library(ggplot2)
library(dplyr)
library(tidyr)
library(macpan2)
library(ggraph)
library(tidygraph)
```

# Dynamics

We can read in the model specification using the `mp_tmb_library`
command.

``` r
spec = mp_tmb_library(
    "starter_models", "hiv"
  , package = "macpan2"
)
```

The specification can be used to draw the following flow diagram using
code found in the source of this \[article\](.

![](./figures/flow_diagram-1.png)<!-- -->

# Solving the ODEs

Here is an example trajectory from this model with defaults using the
Runge-Kutta ODE solver.

``` r
outputs = c(sprintf("I%s", 1:4), sprintf("A%s", 1:4))
I0 = 1/400
A0 = 0
sim = (spec
  |> mp_tmb_update(default = list(
      lambda0 = 0.36, n = 0.2
    , I1 = I0, I2 = I0, I3 = I0, I4 = I0
    , A1 = A0, A2 = A0, A3 = A0, A4 = A0
  ))
  |> mp_rk4()
  |> mp_simulator(time_steps = 50L, outputs)
)
(sim
  |> mp_trajectory()
  |> mutate(matrix = sub("^A([1-4])$", "Infectious and treated, stage \\1", matrix))
  |> mutate(matrix = sub("^I([1-4])$", "Infectious and untreated, stage \\1", matrix))
  |> ggplot()
  + geom_line(aes(time, value))
  + facet_wrap(~ matrix, ncol = 2, scales = 'free', dir = "v")
  + scale_y_continuous(limits = c(0, NA), expand = c(0, 0))
)
```

![](./figures/simulations-1.png)<!-- -->

# Model Specification

This model has been specified in the `hiv` directory
[here](https://github.com/canmod/macpan2/blob/main/inst/starter_models/hiv/tmb.R)
and is accessible from the `macpan2` model library (see [Example
Models](https://canmod.github.io/macpan2/articles/example_models.html)
for details).

# References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-granich2009universal" class="csl-entry">

Granich, Reuben M, Charles F Gilks, Christopher Dye, Kevin M De Cock,
and Brian G Williams. 2009. “Universal Voluntary HIV Testing with
Immediate Antiretroviral Therapy as a Strategy for Elimination of HIV
Transmission: A Mathematical Model.” *The Lancet* 373 (9657): 48–57.
<https://www.thelancet.com/journals/lancet/article/PIIS0140-6736(08)61697-9/abstract>.

</div>

<div id="ref-kretzschmar2013prospects" class="csl-entry">

Kretzschmar, Mirjam E, Maarten F Schim van der Loeff, Paul J Birrell,
Daniela De Angelis, and Roel A Coutinho. 2013. “Prospects of Elimination
of HIV with Test-and-Treat Strategy.” *Proceedings of the National
Academy of Sciences* 110 (39): 15538–43.

</div>

</div>
