<!-- Auto-generated - do not edit by hand -->
# macpan2

<!-- badges: start -->

[![R-CMD-check](https://github.com/canmod/macpan2/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/canmod/macpan2/actions/workflows/R-CMD-check.yaml)
[![test
coverage](https://byob.yarr.is/canmod/macpan2/coverage)](https://github.com/canmod/macpan2/actions/workflows/test-coverage.yaml)
[![commit
activity](https://img.shields.io/github/commit-activity/m/canmod/macpan2)](https://github.com/canmod/macpan2/commits)
[![contributors](https://img.shields.io/github/contributors/canmod/macpan2)](https://github.com/canmod/macpan2/graphs/contributors)
[![release](https://img.shields.io/github/v/release/canmod/macpan2?include_prereleases)](https://github.com/canmod/macpan2/releases/latest)

[McMasterPandemic](https://github.com/mac-theobio/McMasterPandemic) was
developed to provide forecasts and insights to Canadian public health
agencies throughout the COVID-19 pandemic. [Much was
learned](https://canmod.github.io/macpan-book/index.html#vision-and-direction)
about developing general purpose compartmental modelling software during
this experience, but the pressure to deliver regular forecasts made it
difficult to focus on the software itself. The goal of this `macpan2`
project is to re-imagine `McMasterPandemic`, building it from the ground
up with architectural and technological decisions that address the many
lessons that we learned from COVID-19 about software.

Although `macpan2` is designed as a compartmental modelling tool that is
agnostic about the underlying computational engine, it currently makes
use of [template model builder](https://github.com/kaskr/adcomp).
Template model builder (TMB) is an `R` modelling package based on a
`C++` framework incorporating mature [automatic
differentiation](https://cppad.readthedocs.io/en/latest/user_guide.html)
and [matrix
algebra](http://eigen.tuxfamily.org/index.php?title=Main_Page)
libraries.

The [Public Health Risk Sciences
Division](https://github.com/phac-nml-phrsd) at the [Public Health
Agency of Canada](https://www.canada.ca/en/public-health.html) uses
`macpan2` (for example,
[here](https://phac-nml-phrsd.github.io/EPACmodel/)).

## Documentation

-   [Package reference](https://canmod.github.io/macpan2/)
-   [Quick-start
    guide](https://canmod.github.io/macpan2/articles/quickstart)
-   [Representation of compartmental
    models](https://canmod.github.io/macpan2/articles/model_definitions)
    \[specification document\]
-   [`C++` engine](https://canmod.github.io/macpan2/articles/cpp_side)
    \[specification document\]
-   [Project history and
    trajectory](https://canmod.net/misc/macpan2_presentation) \[slides\]

## Installation

If you’re on a Windows system, please install `Rtools` matching your R
version from [here](https://cran.r-project.org/bin/windows/Rtools/).
This ensures you have a C++ compiler, which is required to install
`macpan2` from source (as below).

Then, install the `macpan2` package with the following R command.

    remotes::install_github("canmod/macpan2")

For projects in production one should install a specific version, as in
the following command.

    remotes::install_github("canmod/macpan2@v0.0.3")

## Hello World

This [quick-start
guide](https://canmod.github.io/macpan2/articles/quickstart) describes
the following hello-world SIR model.

    library(macpan2)
    sir = Compartmental(system.file("starter_models", "sir", package = "macpan2"))
    N = 100
    simulator = sir$simulators$tmb(time_steps = 100
      , state = c(S = N - 1, I = 1, R = 0)
      , flow = c(foi = 0, gamma = 0.1)
      , N = N
      , beta = 0.2
    )
    sir_sims = simulator$report()

## Product Management

The [project board](https://github.com/orgs/canmod/projects/2) tracks
the details of bugs, tasks, and feature development. But the following
narrative will provide context on product development themes, their
current state, and plans for improvement and implementation.

### General Dynamic Simulation

One can define a generic set of update steps that are iterated to
produce a dynamic simulation.

    library(macpan2)
    si = mp_dynamic_model(
      expr_list = ExprList(
        during = list(
            infection ~ beta * S * I / N
          , S ~ S - infection
          , I ~ I + infection
        )
      ),
      unstruc_mats = list(S = 99, I = 1, beta = 0.25, N = 100)
    )
    print(si)

    ## ---------------------
    ## At every iteration of the simulation loop (t = 1 to T):
    ## ---------------------
    ## 1: infection ~ beta * S * I/N
    ## 2: S ~ S - infection
    ## 3: I ~ I + infection

Simulating from this model takes the following steps.

    getwd()

    ## [1] "/Users/stevenwalker/Development/macpan2"

    (si
     |> mp_tmb_simulator(time_steps = 10, mats_to_return = "I")
     |> mp_report()
    )

    ##    matrix time row col    value
    ## 1       I    1   0   0 1.247500
    ## 2       I    2   0   0 1.555484
    ## 3       I    3   0   0 1.938307
    ## 4       I    4   0   0 2.413491
    ## 5       I    5   0   0 3.002301
    ## 6       I    6   0   0 3.730342
    ## 7       I    7   0   0 4.628139
    ## 8       I    8   0   0 5.731624
    ## 9       I    9   0   0 7.082401
    ## 10      I   10   0   0 8.727601

This part of the package is general, stable, and flexible. It also meets
many modellers where they are, which is with the ability to write down a
set of transitions/state updates.

But it is not convenient if you would just like to simulate from it,
which is what the model library is for.

### Model Library

    ("unstructured/si"
     |> mp_library()
     |> mp_tmb_simulator(time_steps = 10, mats_to_return = "I")
     |> mp_report()
    )

    ##    matrix time row col    value
    ## 1       I    1   0   0 1.247500
    ## 2       I    2   0   0 1.555484
    ## 3       I    3   0   0 1.938307
    ## 4       I    4   0   0 2.413491
    ## 5       I    5   0   0 3.002301
    ## 6       I    6   0   0 3.730342
    ## 7       I    7   0   0 4.628139
    ## 8       I    8   0   0 5.731624
    ## 9       I    9   0   0 7.082401
    ## 10      I   10   0   0 8.727601

TODO: - \[ \] Reuse the tools for the older concept of starter models. -
\[ \] Establish a specification

### Calibration

### Model Structure and Bookkeeping

### Alternative Engines

### Combining Expression Lists

Because expression lists are really just lists of expressions, they can
be combined as lists would normally be combined. In this example we keep
the dynamics of the si model separate from under-reporting and reporting
delay corrections to the raw prevalence (TODO: should really use
incidence).

    library(macpan2)
    si_dynamics = list(
        transition_rate = infection ~ beta * S * I / N
      , state_update = S ~ S - infection
      , state_update = I ~ I + infection
    )
    reporting_correction = list(
      post_processing = reports ~ convolution(I, c(0.5, 0.25, 0.25))
    )
    si = mp_dynamic_model(
      expr_list = ExprList(during = c(si_dynamics, reporting_correction)),
      unstruc_mats = list(S = 99, I = 1, beta = 0.25, N = 100)
    )
    (si
      |> mp_tmb_simulator(time_steps = 10, mats_to_return = "reports")
      |> mp_report()
    )

    ##     matrix time row col     value
    ## 1  reports    1   0   0 0.6237500
    ## 2  reports    2   0   0 0.7777422
    ## 3  reports    3   0   0 0.9691533
    ## 4  reports    4   0   0 1.2067453
    ## 5  reports    5   0   0 1.5011505
    ## 6  reports    6   0   0 1.8651709
    ## 7  reports    7   0   0 2.3140693
    ## 8  reports    8   0   0 2.8658120
    ## 9  reports    9   0   0 3.5412006
    ## 10 reports   10   0   0 4.3638003
