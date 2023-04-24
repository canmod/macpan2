# macpan2

<!-- badges: start -->
[![R-CMD-check](https://github.com/canmod/macpan2/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/canmod/macpan2/actions/workflows/R-CMD-check.yaml)
[![test coverage](https://byob.yarr.is/canmod/macpan2/coverage)](https://github.com/canmod/macpan2/actions/workflows/test-coverage.yaml)
[![commit activity](https://img.shields.io/github/commit-activity/m/canmod/macpan2)](https://github.com/canmod/macpan2/commits)
[![contributors](https://img.shields.io/github/contributors/canmod/macpan2)](https://github.com/canmod/macpan2/graphs/contributors)

[McMasterPandemic](https://github.com/mac-theobio/McMasterPandemic) was developed to provide forecasts and insights to Canadian public health agencies throughout the COVID-19 pandemic. [Much was learned](https://canmod.github.io/macpan-book/index.html#vision-and-direction) about developing general purpose compartmental modelling software during this experience, but the pressure to deliver regular forecasts made it difficult to focus on the software itself. The goal of this `macpan2` project is to re-imagine `McMasterPandemic`, building it from the ground up with architectural and technological decisions that address the many lessons that we learned from COVID-19 about software.

Although `macpan2` is designed as a compartmental modelling tool that is agnostic about the underlying computational engine, it currently makes use of [template model builder](https://github.com/kaskr/adcomp). Template model builder (TMB) is a blazing fast :rocket: `R` modelling package based on a `C++` framework that sits on top of mature [automatic differentiation](https://cppad.readthedocs.io/en/latest/user_guide.html) and [matrix algebra](http://eigen.tuxfamily.org/index.php?title=Main_Page) libraries.

## Documentation

* [Package reference](https://canmod.github.io/macpan2)
* [Quick-start guide](https://canmod.github.io/macpan2/articles/quickstart)
* [Representation of compartmental models](https://canmod.github.io/macpan2/articles/model_definitions) [specification document]
* [`C++` engine](https://canmod.github.io/macpan2/articles/cpp_side) [specification document]
* [Project history and trajectory](https://canmod.net/misc/macpan2_presentation) [slides]

## Installation

Users should install with the following R commands.

```
remotes::install_github("canmod/oor")
remotes::install_github("canmod/macpan2")
```

Developers and contributors should clone this repository and call `make` at the command-line in the top level directory. The following `make` rules are available for getting more control over the build process.

```
make quick-install     # for changes that only modify R source
make quick-doc-install # for changes that modify R source and roxygen comments
make quick-test        # quick-doc-install + run-examples + run-tests
make run-examples      # help file checks only (without package rebuild)
make run-tests         # run scripts in tests (without package rebuild)
make full-install      # for all changes, including changes to C++ source
make src-update        # push changes to dev.cpp to macpan2.cpp
make enum-update       # register new C++ engine functions on the R-side
make engine-doc-update # generate roxygen comments from comments in dev.cpp
make doc-update        # roxygenize
make pkg-build         # build the R package
make pkg-install       # install the R package from the build
make pkg-check         # R package checks
```
