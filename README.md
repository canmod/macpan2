# macpan2

<!-- badges: start -->
[![R-CMD-check](https://github.com/canmod/macpan2/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/canmod/macpan2/actions/workflows/R-CMD-check.yaml)
[![test coverage](https://byob.yarr.is/canmod/macpan2/coverage)](https://github.com/canmod/macpan2/actions/workflows/test-coverage.yaml)
[![commit activity](https://img.shields.io/github/commit-activity/m/canmod/macpan2)](https://github.com/canmod/macpan2/commits)
[![contributors](https://img.shields.io/github/contributors/canmod/macpan2)](https://github.com/canmod/macpan2/graphs/contributors)
[![release](https://img.shields.io/github/v/release/canmod/macpan2?include_prereleases)](https://github.com/canmod/macpan2/releases/latest)

[McMasterPandemic](https://github.com/mac-theobio/McMasterPandemic) was developed to provide forecasts and insights to Canadian public health agencies throughout the COVID-19 pandemic. [Much was learned](https://canmod.github.io/macpan-book/index.html#vision-and-direction) about developing general purpose compartmental modelling software during this experience, but the pressure to deliver regular forecasts made it difficult to focus on the software itself. The goal of this `macpan2` project is to re-imagine `McMasterPandemic`, building it from the ground up with architectural and technological decisions that address the many lessons that we learned from COVID-19 about software.

Although `macpan2` is designed as a compartmental modelling tool that is agnostic about the underlying computational engine, it currently makes use of [template model builder](https://github.com/kaskr/adcomp). Template model builder (TMB) is an `R` modelling package based on a `C++` framework incorporating mature [automatic differentiation](https://cppad.readthedocs.io/en/latest/user_guide.html) and [matrix algebra](http://eigen.tuxfamily.org/index.php?title=Main_Page) libraries.

The [Public Health Risk Sciences Division](https://github.com/phac-nml-phrsd) at the [Public Health Agency of Canada](https://www.canada.ca/en/public-health.html) uses `macpan2` (for example, [here](https://phac-nml-phrsd.github.io/EPACmodel/)).

## Documentation

* [Package reference](https://canmod.github.io/macpan2/)
* [Quick-start guide](https://canmod.github.io/macpan2/articles/quickstart)
* [Representation of compartmental models](https://canmod.github.io/macpan2/articles/model_definitions) [specification document]
* [`C++` engine](https://canmod.github.io/macpan2/articles/cpp_side) [specification document]
* [Project history and trajectory](https://canmod.net/misc/macpan2_presentation) [slides]

## Installation

If you're on a Windows system, please install `Rtools` matching your R version from [here](https://cran.r-project.org/bin/windows/Rtools/). This ensures you have a C++ compiler, which is required to install `macpan2` from source (as below).

Then, install the `macpan2` package with the following R command.

```
remotes::install_github("canmod/macpan2")
```

For projects in production one should install a specific version, as in the following command.
```
remotes::install_github("canmod/macpan2@v0.0.3")
```

## Hello World

This [quick-start guide](https://canmod.github.io/macpan2/articles/quickstart) describes the following hello-world SIR model.

```
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
```

## Product Management

* [Roadmap](https://github.com/orgs/canmod/projects/2)
