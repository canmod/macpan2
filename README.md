<!-- Auto-generated - do not edit by hand -->
<!-- Edit misc/build/README.Rmd instead -->
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

TODO:

-   ☐ Reuse the tools for the older concept of starter models
-   ☐ Establish a specification

### Calibration

We will build a function, `mp_calibrate`, which takes (1) an object for
simulating model trajectories and (2) other information for calibrating
certain quantities of this model. This second type of information is
detailed in the following sections. The output of `mp_calibrate` should
be another object for simulating model trajectories that contains new
default parameter values given by fits and additional stochasticity
resulting from parameter estimation uncertainty.

#### Specifying Data to Fit

A data frame (or data frames) containing observed (possibly uneven) time
series to compare with model simulations. What form should this data
frame take?

One option is the same format as the output of `mp_report`. This would
have several benefits.

    * Consistency with input and output formats, making it a little easier to learn.
    * Easy to manipulate output into input for testing calibration functionality.
    * Possibly simpler argument list to `mp_calibrate` because we would just relate the observed data to simulated data with the same name, of course we would still need an interface for distributional assumptions.
    * Naturally handles missing values

The main disadvantage of this is that format could differ from the
indexed vectors discussed below.

#### Specifying Distributional Assumptions

Probably should be a few ways to do this depending on how many different
assumptions need to be made. At one extreme every observation gets the
same distribution, which is easily specified in an argument to
`mp_calibrate`. At the other extreme each observation gets its own
distribution (including hyperparameters like spread and shape), which
could be specified by adding additional columns to the data frame with
observed values. Designs for interfaces for use cases that are somewhere
between these two extremes seem less obvious.

#### Specifying Parameters to Fit

There are two kinds of parameters to fit.

-   Existing quantities to be fitted (e.g. `beta`, initial number of
    susceptible individuals `S`).
-   Creating new quantities to be fitted (e.g. distributional scale
    parameters declared along with (#distributional-assumptions).

and the scale (e.g. log, logit) on which to fit these parameters. The
new distributional parameters should go into a new indexed vector called
something like `distributional_parameters`. (TODO: more general name for
new parameters that are part of the observation model).

### Time-Varying Parameters

TODO

### Model Structure and Bookkeeping

Structured models are combinations of simpler modular model components.
For example one might combine an SIR model with an age-group contact
model to produce an age structured model. The modular model components
are called atomic models.

#### Structure in Expressions

Models are composed of expression lists. Each expression in an
unstructured model can be converted into a structured expression to
create a structured model. For example, the following unstructured
expression defines the rate at which new infections emerge.

    infection ~ beta * S * I / N

Each symbol in this expression has a certain type within a structured
model, and this type determines how it gets translated into a structured
expression. The simplest structured model is one that collects `S` and
`I` into a `state` vector with elements `S` and `I`. With this
interpretation of the `S` and `I` symbols, the structured infection
expression gets translated internally to the following.

    infection ~ beta * state[S] * state[I] / N

Here `S` and `I` become symbols for extracting subsets of the `state`
vector. In this case the expression itself remains a scalar expression
but two of the scalars are obtained by extracting subsets of the `state`
vector. It won’t take much imagination to think of examples where
multiple paths to infection are required, and therefore the single
scalar-valued infection expression will be insufficient.

We will have a vector-valued expression, for example, in a model with an
expanded state vector that tracks the geographic location of `S` and `I`
individuals. For example, a two patch model with an `east` and `west`
patch would involve a four-dimensional state vector with the following
elements: `S.east`, `S.west`, `I.east`, and `I.west`. In this case we
now have two scalar-valued infection expressions.

    infection[east] ~ beta * state[S.east] * state[I.east] / N
    infection[west] ~ beta * state[S.west] * state[I.west] / N

With two patches it is fine to write out all scalar-valued infection
expressions, but with more patches and with different types of structure
(e.g. age groups, symptom status, hospitalization, immunity status, etc
…) it will become crucial to have software that handles the bookkeeping
internally.

To see how easy this can be, note that this two-patch infection
expression can be powerfully and compactly expressed as our original
unstructured expression, `infection ~ beta * S * I / N`, where
`S = c(state[S.east], state[S.west])` and
`I = c(state[I.east], state[I.west])`.

Why is this powerful? Because it separates the math of the dynamic
mechanism, `infection ~ beta * S * I / N`, from the bookkeeping required
in structured models where the same mechanism is applied again and again
to different model strata. This is often how modellers think. For
example, I might have a location-structured SIR model that I need to
expand to be both age- and location-structured. In this case, infection
is still the same process, whereby a susceptible individual contacts an
infectious individual to create a flow from susceptible individuals to
infectious individuals. The same math applies to all strata of the
model. The boring but necessary part is to connect the math to the
bookkeeping associated with the model structure, and so software should
focus on making these bookkeeping changes as easy as possible and with
minimal changes required to the underlying mathematical expressions.

Let’s look at more examples of infection, and watch the bookkeeping get
more annoying. In an age-stratified model with two age groups, we now
get four scalar-valued infection expressions of the form
`infection ~ beta * S * I / N`.

    infection[young.young] ~ beta[young.young] * state[S.young] * state[I.young] / N[young]
    infection[young.old]   ~ beta[young.old]   * state[S.young] * state[I.old]   / N[old]
    infection[old.young]   ~ beta[old.young]   * state[S.old]   * state[I.young] / N[young]
    infection[old.old]     ~ beta[old.old]     * state[S.old]   * state[I.old]   / N[old]

Here the first expression is for a young individual infecting an old
individual, the second is for an old individual infecting a young
individual, etc … Things get worse if we have two age groups in two
patches.

    infection[young.young.east] ~ beta[young.young.east] * state[S.young.east] * state[I.young.east] / N[young.east]
    infection[young.old.east]   ~ beta[young.old.east]   * state[S.young.east] * state[I.old.east]   / N[old.east]
    infection[old.young.east]   ~ beta[old.young.east]   * state[S.old.east]   * state[I.young.east] / N[young.east]
    infection[old.old.east]     ~ beta[old.old.east]     * state[S.old.east]   * state[I.old.east]   / N[old.east]
    infection[young.young.west] ~ beta[young.young.west] * state[S.young.west] * state[I.young.west] / N[young.west]
    infection[young.old.west]   ~ beta[young.old.west]   * state[S.young.west] * state[I.old.west]   / N[old.west]
    infection[old.young.west]   ~ beta[old.young.west]   * state[S.old.west]   * state[I.young.west] / N[young.west]
    infection[old.old.west]     ~ beta[old.old.west]     * state[S.old.west]   * state[I.old.west]   / N[old.west]

This still isn’t so bad, as we just have the first four expressions for
`east` and the last four for `west`. But now let’s introduce two symptom
status categories: `mild` and `severe`.

    infection[young.young.east.mild.mild]     ~ beta[young.young.east.mild.mild]     * state[S.young.east] * state[I.young.east.mild]   / N[young.east]
    infection[young.young.east.mild.severe]   ~ beta[young.young.east.mild.severe]   * state[S.young.east] * state[I.young.east.severe] / N[young.east]
    infection[young.young.east.severe.mild]   ~ beta[young.young.east.severe.mild]   * state[S.young.east] * state[I.young.east.mild]   / N[young.east]
    infection[young.young.east.severe.severe] ~ beta[young.young.east.severe.severe] * state[S.young.east] * state[I.young.east.severe] / N[young.east]
    infection[young.old.east.mild.mild]       ~ beta[young.old.east.mild.mild]       * state[S.young.east] * state[I.old.east.mild]     / N[old.east]
    infection[young.old.east.mild.severe]     ~ beta[young.old.east.mild.severe]     * state[S.young.east] * state[I.old.east.severe]   / N[old.east]
    infection[young.old.east.severe.mild]     ~ beta[young.old.east.severe.mild]     * state[S.young.east] * state[I.old.east.mild]     / N[old.east]
    infection[young.old.east.severe.severe]   ~ beta[young.old.east.severe.severe]   * state[S.young.east] * state[I.old.east.severe]   / N[old.east]
    infection[old.young.east.mild.mild]       ~ beta[old.young.east.mild.mild]       * state[S.old.east]   * state[I.young.east.mild]   / N[young.east]
    infection[old.young.east.mild.severe]     ~ beta[old.young.east.mild.severe]     * state[S.old.east]   * state[I.young.east.severe] / N[young.east]
    infection[old.young.east.severe.mild]     ~ beta[old.young.east.severe.mild]     * state[S.old.east]   * state[I.young.east.mild]   / N[young.east]
    infection[old.young.east.severe.severe]   ~ beta[old.young.east.severe.severe]   * state[S.old.east]   * state[I.young.east.severe] / N[young.east]
    infection[old.old.east.mild.mild]         ~ beta[old.old.east.mild.mild]         * state[S.old.east]   * state[I.old.east.mild]     / N[old.east]
    infection[old.old.east.mild.severe]       ~ beta[old.old.east.mild.severe]       * state[S.old.east]   * state[I.old.east.severe]   / N[old.east]
    infection[old.old.east.severe.mild]       ~ beta[old.old.east.severe.mild]       * state[S.old.east]   * state[I.old.east.mild]     / N[old.east]
    infection[old.old.east.severe.severe]     ~ beta[old.old.east.severe.severe]     * state[S.old.east]   * state[I.old.east.severe]   / N[old.east]
    infection[young.young.west.mild.mild]     ~ beta[young.young.west.mild.mild]     * state[S.young.west] * state[I.young.west.mild]   / N[young.west]
    infection[young.young.west.mild.severe]   ~ beta[young.young.west.mild.severe]   * state[S.young.west] * state[I.young.west.severe] / N[young.west]
    infection[young.young.west.severe.mild]   ~ beta[young.young.west.severe.mild]   * state[S.young.west] * state[I.young.west.mild]   / N[young.west]
    infection[young.young.west.severe.severe] ~ beta[young.young.west.severe.severe] * state[S.young.west] * state[I.young.west.severe] / N[young.west]
    infection[young.old.west.mild.mild]       ~ beta[young.old.west.mild.mild]       * state[S.young.west] * state[I.old.west.mild]     / N[old.west]
    infection[young.old.west.mild.severe]     ~ beta[young.old.west.mild.severe]     * state[S.young.west] * state[I.old.west.severe]   / N[old.west]
    infection[young.old.west.severe.mild]     ~ beta[young.old.west.severe.mild]     * state[S.young.west] * state[I.old.west.mild]     / N[old.west]
    infection[young.old.west.severe.severe]   ~ beta[young.old.west.severe.severe]   * state[S.young.west] * state[I.old.west.severe]   / N[old.west]
    infection[old.young.west.mild.mild]       ~ beta[old.young.west.mild.mild]       * state[S.old.west]   * state[I.young.west.mild]   / N[young.west]
    infection[old.young.west.mild.severe]     ~ beta[old.young.west.mild.severe]     * state[S.old.west]   * state[I.young.west.severe] / N[young.west]
    infection[old.young.west.severe.mild]     ~ beta[old.young.west.severe.mild]     * state[S.old.west]   * state[I.young.west.mild]   / N[young.west]
    infection[old.young.west.severe.severe]   ~ beta[old.young.west.severe.severe]   * state[S.old.west]   * state[I.young.west.severe] / N[young.west]
    infection[old.old.west.mild.mild]         ~ beta[old.old.west.mild.mild]         * state[S.old.west]   * state[I.old.west.mild]     / N[old.west]
    infection[old.old.west.mild.severe]       ~ beta[old.old.west.mild.severe]       * state[S.old.west]   * state[I.old.west.severe]   / N[old.west]
    infection[old.old.west.severe.mild]       ~ beta[old.old.west.severe.mild]       * state[S.old.west]   * state[I.old.west.mild]     / N[old.west]
    infection[old.old.west.severe.severe]     ~ beta[old.old.west.severe.severe]     * state[S.old.west]   * state[I.old.west.severe]   / N[old.west]

This is intense. The names in square brackets get much less clear in
several ways as the model gets more structured. This lack of clarity
makes it difficult to see a variety of model assumptions by looking at
scalar-valued expressions. The `infection` and `beta` vectors depend on
two age categories and two symptom statuses, but only one location. This
is because young people can infect old people (and vice versa), because
mildly infectious people can cause severe infection (and vice versa),
and because infectious people in the east cannot infect people in the
west (and vice versa). For labels associated with two ages, what does
the first age mean, relative to the second age? To discover this you
need to know to look at the ages associated with the `S` and `I` states,
and once you do this you can see that the first age category is
associated with the susceptible individual and the second with the
infectious individual. There is a related issue with symptom status, but
it is expressed differently because `S` individuals are not structured
by symptom status. In this case we match the second symptom status
associated with `infection` and `beta` to the symptom status of the `I`
states, which means that the first symptom status implicitly refers to
the status of the newly infected individuals and not the infectious
individuals. Another way to look at this last issue is that `I` boxes
play two different roles. The first role is as an individual that
infects an `S` individual, and the second is as the individual that that
`S` individual becomes after it is infected. None of this is obvious
from the scalar-valued expressions above, and it is difficult to imagine
a clearer way to explicitly write each expression.

Our approach is to do the bookkeeping in a different way. In particular
we believe that a constructive approach to structure provides a more
comprehensible description, as we describe next. In brief, we believe
that a grammar for specifying the steps associated with adding structure
can be clearer than a description of the final structured model.

#### Constructive Descriptions of Model Structure

The first step to being more constructive is to have a better
representation of the structured vectors. Above we used
dot-concatenation to represent the model strata. For example, in the
two-patch SI model we have both epidemiological status and geographic
location in the state variable names: `S.east`, `S.west`, `I.east`, and
`I.west`. But as the state vector gets more structured it becomes more
convenient to describe its variables using an index table, the rows of
which describe each state variable.

    state = mp_cartesian(
      mp_index(Epi = c("S", "I")),
      mp_index(Loc = c("east", "west"))
    )
    state

    ##  Epi  Loc
    ##    S east
    ##    I east
    ##    S west
    ##    I west

    beta = mp_group(state, "Epi")

With this representation we can get subsets of the state vector that
represent each epidemiological status.

    mp_subset(state, Epi = "S")

    ##  Epi  Loc
    ##    S east
    ##    S west

    mp_subset(state, Epi = "I")

    ##  Epi  Loc
    ##    I east
    ##    I west

#### Structured Vectors

These are column vectors, the rows of which

### Structure Encourages Reparameterization

### Alternative Engines

TODO

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
