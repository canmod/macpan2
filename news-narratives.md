## 1.17.0

### New Features

* More robust handling of C++ log files, even if simulators are loaded from disk to a file system that does not have (or no longer has) the expected path to the log file.

## 1.16.11

### New Features

* Transform parameters when making a calibration by prefixing parameter names with the name of the transformation (e.g., `log_beta` instead of `beta`).

## 1.5.0

### New Features

* Delete elements from model specifications with `mp_tmb_delete`.

## 1.4.0

### Breaking Changes

* Repeated calls of an optimizer now start from the previous best parameter
vector
* No longer fit full covariance matrix in `sdreport`s

### Bug Fixes

* Row vectors with names no longer break spec print methods

## 1.3.1

### Breaking Changes

* Removing in-place modifications to model specification objects in `mp_tmb_insert` and `mp_tmb_update`.

### New Features

* Define explicit state variable updates, with the choice of applying `mp_euler`,
`mp_rk4`, or `mp_euler_multinomial` update methods, the latter generating 
process error.
* New stochasticity engine functions `rbinom` and `reulermultinom`.
* `mp_tmb_fixef_cov` function for getting the covariance matrix of fixed effects.
* `mp_trajectory_ensemble` and `mp_trajectory_sim` functions for summarizing random variation in trajectories.
* Unpacking assignment in the engine. This means that you can do things like `c(x, y) ~ z`, where the values in `z` are assigned to elements in `x`, `y`, etc... in row-major order.
* `to_name_pairs` function for returning all pairwise dot-concatenations of the elements of a character vector without dots.
* `to_values` function for extracting the column from a data frame with only a single numerical column and turning numeric matrices and arrays with dimnames into a flattened numeric vector with labels produced by appropriately dot-concatenating the dimnames.

### Bug Fixes

* Several bugs related to input handling in `mp_tmb_calibrator` (#176).

### Doc Fixes and Updates

* New installation instructions for installing from `r-universe`.
* New vignette: `real_data`.
* Document `to_name`, `to_names`, `to_labels`, which handle naming of structured objects.
* Document `print` function in the `?engine_functions`.
* `simple_sims` example in `?engine_functions` now runs without error.
* Help file examples for `mp_tmb_insert` and `mp_tmb_update`.
* Readme/vignette examples better expose calls to `library` for the user.

## 1.1.0

### Breaking Changes

* `simple_sims` no longer returns outputs for the zeroth time-step.
* Using `last.par.best` to finalize the TMB objective function following optimization.
* `group_sums` now checks for bad group indexes.
* `rbind_lag` now throws an error when `lag > 1` because there are conceptual errors with this case. We will get back to this when we can to allow these important cases by adding an argument with initial conditions associated with negative time steps.

### New Features

* Streamlined installation via `r-universe`.
* `mp_tmb_calibrator` and `mp_tmb_optimize` functions for calibration using a simple and restrictive trajectory matching model.  Future releases will allow more functionality.
* `mp_tmb_coef` and `mp_tmbstan_coef` for tables of statistical parameters used in calibration.
* `mp_trajectory_sd` and `mp_trajectory_ensemble` for getting information about fitted trajectory uncertainty.
* `mp_tmb_update|insert` functions for creating new model specs from existing ones.
* Parameters specified as a data frame can now place default values in columns with any of the following names: `"default", "Default", "value", "Value", "val", "Val"`
* `mp_tmb_library` can return a list of model specs with `alternative_specs`, if the model makes alternatives available.
* `time_var` engine function is better than the old `time_group`, which required two expressions rather than one to do the same thing. `time_group` will remain but is softly deprecated.
* Fixed effects extractor and formatter.
* `mp_default` function for extracting spec and simulator defaults in long-format.
* `rbind_time` allows integer vectors for subsetting times
* `options(macpan2_verbose = FALSE)` will turn off the flood of information provided by `TMB`. Note that this only takes effect if set before creating a TMB simulator.

### Bug Fixes

* Segfaults for out-of-range assignment.

### Doc Fixes and Updates

* (in progress) [Calibration vignette](https://github.com/canmod/macpan2/blob/HEAD/vignettes/calibration.Rmd) is updated to be a simpler quick-start, and previous advanced material is moved to an advanced vignette.
* `mp_tmb_model_spec` documentation is filled out.
* `group_sums` TMB engine function third argument updated from old pre-1.0.0 behaviour.

## 1.0.0

### Breaking Changes

* No more `flows.csv`, `derivations.json` files in the library, and instead `tmb.R` files.
* No more `Compartmental` function, and instead `mp_tmb_model_spec` and `mp_tmb_library`.
* `{group|row|col}Sums` are now called `{group|row|col}_sums`.
* Final argument of `group_sums` used to be the length of the output vector, but now it is a vector of the desired output length.
* `TMBModel` and associated classes (`ExprList`) are no longer exported, in favour of `mp_...` functions for doing similar things.
* Sort simulation output by time step.

### New Features

* Constant integer vectors can now be passed to `C++`.
* Integer vectors can be used to subset assignment matrices (i.e. integer subsetting on the left-hand-side).
* `mp_trajectory()` function, which is roughly equivalent to `model$report(..., .phases = "during")`.
* New `print` function in the TMB engine.
* No need to declare empty matrices when creating TMB model objects, and matrices that are derived are automatically detected.

### Experimental Features

* Model structure grammar (`mp_index()`, `mp_join()`, etc.)
* Log files

## 0.0.3

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

## 0.0.2

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

## 0.0.1

* Initial release.
