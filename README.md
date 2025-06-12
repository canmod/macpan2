<!-- Auto-generated - do not edit by hand -->
<!-- Edit README.Rmd instead -->
macpan2
================

<!-- badges: start -->

[![macpan2 status
badge](https://canmod.r-universe.dev/badges/macpan2)](https://canmod.r-universe.dev/macpan2)
[![R-CMD-check](https://github.com/canmod/macpan2/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/canmod/macpan2/actions/workflows/R-CMD-check.yaml)
[![test
coverage](https://byob.yarr.is/canmod/macpan2/coverage)](https://github.com/canmod/macpan2/actions/workflows/test-coverage.yaml)
[![commit
activity](https://img.shields.io/github/commit-activity/m/canmod/macpan2)](https://github.com/canmod/macpan2/commits)
[![contributors](https://img.shields.io/github/contributors/canmod/macpan2)](https://github.com/canmod/macpan2/graphs/contributors)

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

The [Public Health Risk Sciences
Division](https://github.com/phac-nml-phrsd) at the [Public Health
Agency of Canada](https://www.canada.ca/en/public-health.html) uses
`macpan2` (for example,
[here](https://phac-nml-phrsd.github.io/EPACmodel/)).

## Documentation

-   [Package website](https://canmod.github.io/macpan2/)
-   [Package reference and function
    documentation](https://canmod.github.io/macpan2/reference)
-   [Quick-start
    guide](https://canmod.github.io/macpan2/articles/quickstart)
-   [Articles describing the
    package](https://canmod.github.io/macpan2/articles)
-   [Frequently asked
    questions](https://canmod.github.io/macpan2/articles/FAQs)
-   [`TMB` engine](https://canmod.github.io/macpan2/articles/cpp_side)
    \[specification document\]
-   [Project history and trajectory](https://canmod.net/misc/pyRC)
    \[slides\]
-   [Instructional
    videos](https://drive.google.com/drive/folders/1NEQf2sy6QLxMiiWBN5Yn9U7wiPMs1jiS)

## Installation

Here is the recommended way to install `macpan2` (from within an R
session):

``` r
repos = c('https://canmod.r-universe.dev', 'https://cloud.r-project.org')
install.packages('macpan2', repos = repos)
```

To get the latest development version of `macpan2`, or if the
incantation above command fails for some reason, try:

``` r
remotes::install_github("canmod/macpan2")
```

This command requires the `remotes` package and assumes that your R
environment is set up to build package that include compiled C++ code
(e.g. see details for
[Windows](https://cran.r-project.org/bin/windows/base/howto-R-devel.html)
or [MacOS](https://mac.r-project.org/tools/) \[you probably only need
Xcode from this page\]).

Many `macpan2` workflows also use these four popular packages:

``` r
install.packages(c("dplyr", "ggplot2", "tidyr", "broom.mixed"))
```

The `Rgraphviz` package is useful for plotting flow diagrams of models
(see `?dot_layout`). To install it:

``` r
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("Rgraphviz")
```

## Reproducibility

The [r-universe](https://r-universe.dev), which we use to distribute
`macpan2`, suggests two approaches for projects in production that need
to keep track of specific versions of `macpan2`:
[snapshots](https://ropensci.org/blog/2023/05/31/runiverse-snapshots/)
or [`renv`](https://ropensci.org/blog/2022/01/06/runiverse-renv/).

To take the first approach, snapshots of `macpan2` (and its dependency
`oor`) can be obtained using the following download link.

    https://canmod.r-universe.dev/api/snapshot/zip?packages=macpan2,macpan2helpers,oor

Please see [this documentation](https://canmod.r-universe.dev/apis) for
instructions on customizing this download link.

The benefit of the first approach is that it doesn’t require users to be
able to compile C++ code, whereas the second does. The benefit of the
second approach is that it can be used to manage dependencies on all
packages in your workflows. It might be possible to combine the two
approaches to get the best of both worlds, but this isn’t tested.

## Hello World

The following code specifies an [SI
model](https://github.com/canmod/macpan2/blob/main/inst/starter_models/si/README.md),
which is the simplest model of epidemiological transmission.

``` r
library(macpan2)
si = mp_tmb_model_spec(
    before = S ~ 1 - I
  , during = mp_per_capita_flow(
        from      = "S"         ## compartment from which individuals flow
      , to        = "I"         ## compartment to which individuals flow
      , rate      = "beta * I"  ## expression giving _per-capita_ flow rate
      , flow_name = "infection" ## name for _absolute_ flow rate = beta * I * S
    )
  , default = list(I = 0.01, beta = 0.2)
)
print(si)
```

    ## ---------------------
    ## Default values:
    ##  quantity value
    ##         I  0.01
    ##      beta  0.20
    ## ---------------------
    ## 
    ## ---------------------
    ## Before the simulation loop (t = 0):
    ## ---------------------
    ## 1: S ~ 1 - I
    ## 
    ## ---------------------
    ## At every iteration of the simulation loop (t = 1 to T):
    ## ---------------------
    ## 1: mp_per_capita_flow(from = "S", to = "I", rate = "beta * I", flow_name = "infection")

See [this
article](https://canmod.github.io/macpan2/articles/example_models.html)
for more example models with documentation.

Simulating from this model requires choosing the number of time-steps to
run and the model outputs to generate. Syntax for simulating `macpan2`
models is [designed to combine with standard data prep and plotting
tools in
R](https://canmod.github.io/macpan2/articles/quickstart.html#processing-results),
as we demonstrate with the following code.

``` r
library(ggplot2)
library(dplyr)
(si
 |> mp_simulator(time_steps = 50, outputs = c("I", "infection"))
 |> mp_trajectory()
 |> mutate(quantity = case_match(matrix
    , "I" ~ "Prevalence"
    , "infection" ~ "Incidence"
  ))
 |> ggplot() 
 + geom_line(aes(time, value)) 
 + facet_wrap(~ quantity, scales = "free")
 + theme_bw()
)
```

![](man/figures/plot-tmb-si-1.png)<!-- -->

## Product Management

The [project board](https://github.com/orgs/canmod/projects/2/views/6)
tracks the details of bugs, tasks, and feature development.
