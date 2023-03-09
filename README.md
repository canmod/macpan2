# macpan2

<!-- badges: start -->
[![R-CMD-check](https://github.com/canmod/macpan2/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/canmod/macpan2/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/canmod/macpan2/branch/main/graph/badge.svg)](https://app.codecov.io/gh/canmod/macpan2?branch=main)
![test coverage](https://byob.yarr.is/canmod/macpan2/coverage)

```
remotes::install_github("canmod/oor")
remotes::install_github("canmod/macpan2")
```

[Model definition specs](https://canmod.net/misc/model_definitions)

[Engine specs](https://canmod.net/misc/cpp_side)

[Binary operator specs](https://canmod.net/misc/elementwise_binary_operators)

[Notes on the composition of simulation models](https://canmod.net/misc/composing_simulation_models)


## Development Plan

### Components

- C++ Engine
    - Done: Draft code
    - In Progress: Documentation
- R-Side Data Structure for the Engine (TMBModel, TMBSimulator)
    - Done: Draft code
    - In Progress: Documentation
- Model Definition Files
    - Done: Draft code
    - In Progress: 
- Translating Model Definition Files to an Engine Data Structure
    - Done: Prototype
    - In Progress: Draft code
- Operations on Model Space
- Calibration
- Ensemble Forecasts


### Common Requirements for each Component

- Prototype code -- works but usually breaks
- Draft code -- friendly and indoctrinated users can collaborate with us
- Testing
- Documentation (for contributors and users)
- Alpha tests -- ready for friendly and indoctrinated users to use on their own
- Beta tests -- ready for external users to use on their own
- Release -- successful beta tests


### Milestones

1. Create an SV-E-IH-R model for Michael Li using the C++ Engine and TMBModel (https://github.com/canmod/macpan2/issues/20)
2. Ready for initial engagement with friendly and indoctrinated users:
    - Draft code or better for C++ Engine, R-Side Engine Data Structure, Model Definition Files, Files-to-Engine Translation
3. 
