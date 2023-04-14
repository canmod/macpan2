# macpan2

<!-- badges: start -->
[![R-CMD-check](https://github.com/canmod/macpan2/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/canmod/macpan2/actions/workflows/R-CMD-check.yaml)
[![test coverage](https://byob.yarr.is/canmod/macpan2/coverage)](https://github.com/canmod/macpan2/actions/workflows/test-coverage.yaml)
[![commit activity](https://img.shields.io/github/commit-activity/m/canmod/macpan2)](https://github.com/canmod/macpan2/commits)
[![contributors](https://img.shields.io/github/contributors/canmod/macpan2)](https://github.com/canmod/macpan2/graphs/contributors)

[McMasterPandemic](https://github.com/mac-theobio/McMasterPandemic) was developed to provide forecasts and insights to Canadian public health agencies throughout the COVID-19 pandemic. [Much was learned](https://canmod.github.io/macpan-book/index.html#vision-and-direction) about developing general purpose compartmental modelling software during this experience, but the pressure to deliver regular forecasts made it difficult to focus on the software itself. The goal of this `macpan2` project is to re-imagine `McMasterPandemic`, building it from the ground up with architectural and technological decisions that address the many lessons that we learned from COVID-19 about software.

## Documentation

[https://canmod.github.io/macpan2/](https://canmod.github.io/macpan2/)

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
make full-install      # for all changes, including changes to C++ source
make src-update        # push changes to dev.cpp to macpan2.cpp
make enum-update       # register new C++ engine functions on the R-side
make engine-doc-update # generate roxygen comments from comments in dev.cpp
make doc-update        # roxygenize
make pkg-build         # build the R package
make pkg-install       # install the R package from the build
make pkg-check         # R package checks
```

## Specification Documents

* [Model definition specs](https://canmod.net/misc/model_definitions)
* [Engine specs](https://canmod.net/misc/cpp_side)
* [Binary operator specs](https://canmod.net/misc/elementwise_binary_operators)
* [Notes on the composition of simulation models](https://canmod.net/misc/composing_simulation_models)