# macpan2 v.v.v

## Breaking Changes

* `simple_sims` no longer returns outputs for the zeroth time-step.
* Using `last.par.best` to finalize the TMB objective function following optimization.

## New Features

* `mp_tmb_library` can return a list of model specs with `alternative_specs`, if the model makes alternatives available.
* `time_var` engine function is better than the old `time_group`, which required two expressions rather than one to do the same thing. `time_group` will remain but is softly deprecated.

## Bug Fixes

* Segfaults for out-of-range assignment

# macpan2 1.0.0

## Breaking Changes

* No more `flows.csv`, `derivations.json` files in the library, and instead `tmb.R` files
* No more `Compartmental` function, and instead `mp_tmb_model_spec` and `mp_tmb_library`
* `{group|row|col}Sums` are now called `{group|row|col}_sums`
* `TMBModel` and associated classes (`ExprList`) are no longer exported, in favour of `mp_...` functions for doing similar things
* Sort simulation output by time step

## New Features

* Constant integer vectors can now be passed to `C++`
* Integer vectors can be used to subset assignment matrices (i.e. integer subsetting on the left-hand-side)
* `mp_trajectory()` function, which is roughly equivalent to `model$report(..., .phases = "during")`
* New `print` function in the TMB engine
* No need to declare empty matrices when creating TMB model objects, and matrices that are derived are automatically detected

## Experimental Features

* Model structure grammar (`mp_index()`, `mp_join()`, etc.)
* Log files

# macpan2 0.0.3

* Optimize C++ simulation history storage by avoiding unnecessary allocations
* Use state and flow names in expression inserters
* Chattier validity checking
* Radial basis functions
* New starter models (thanks @mayaearn and @Flynn-Primrose )
    * `macpan-base` -- re-implementation of the McMaster group's COVID-19 model in `macpan2`
    * `ww` -- wastewater model (doesn't yet have a readme)
    * new readme for and clean up of previous models
* Report what expression broke on the C++ side
* Developer tools for switching between different C++ files and working directories
* Package reference organization cleanup (thanks @bbolker )
* Time-varying parameters vignette

# macpan2 0.0.2

* Interface for optimization of TMB simulation objects
* TMB simulation model updating with caching
* Parameter transformations
* Get initial values of matrices in TMB simulation objects
* State and flow variable names can be used in expressions in some contexts
* Example model indexing (thank you @bbolker!)
* Engine function rbind_time defaults to row binding the full simulation history
* Fix bug when the entire model has no inflows or no outflows
* Fix bugs in symbolic R-side manipulation of expressions
* Fix previously broken argument_dots option in model definition files (thank you @Flynn-Primrose )

# macpan2 0.0.1

* Initial release.
