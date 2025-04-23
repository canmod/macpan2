## 1.16.13

[change list](https://github.com/canmod/macpan2/compare/4cf3617..71a289a9)

## 1.16.12

[change list](https://github.com/canmod/macpan2/compare/1b60500..4cf3617)

## 1.16.11

[change list](https://github.com/canmod/macpan2/compare/f7151b3..1b60500)

## 1.16.10

[change list](https://github.com/canmod/macpan2/compare/732affd..f7151b3)

## 1.16.9

[change list](https://github.com/canmod/macpan2/compare/0981955..732affd)

## 1.16.8

[change list](https://github.com/canmod/macpan2/compare/0838e7e..0981955)

## 1.16.7

[change list](https://github.com/canmod/macpan2/compare/93ba2f9..0838e7e)

## 1.16.6

[change list](https://github.com/canmod/macpan2/compare/ee7d473..93ba2f9)

## 1.16.5

[change list](https://github.com/canmod/macpan2/compare/d8e83a0..ee7d473)

## 1.16.4

[change list](https://github.com/canmod/macpan2/compare/78afd3d..d8e83a0)

## 1.16.3

[change list](https://github.com/canmod/macpan2/compare/79fa954..78afd3d)

## 1.16.2

[change list](https://github.com/canmod/macpan2/compare/a539c47..79fa954)

## 1.16.1

[change list](https://github.com/canmod/macpan2/compare/ae43d12..a539c47)

## 1.16.0

[change list](https://github.com/canmod/macpan2/compare/530d78b..ae43d12)

## 1.15.3

[change list](https://github.com/canmod/macpan2/compare/bbf60fc..530d78b)

## 1.15.2

[change list](https://github.com/canmod/macpan2/compare/192e19f..bbf60fc)

## 1.15.1

[change list](https://github.com/canmod/macpan2/compare/4a2cb70..192e19f)

## 1.15.0

[change list](https://github.com/canmod/macpan2/compare/9df61ac..4a2cb70)

## 1.14.4

[change list](https://github.com/canmod/macpan2/compare/1e3eab0..9df61ac)

## 1.14.3

[change list](https://github.com/canmod/macpan2/compare/811d681..1e3eab0)

## 1.14.2

[change list](https://github.com/canmod/macpan2/compare/26a5d80..811d681)

## 1.14.1

[change list](https://github.com/canmod/macpan2/compare/cd0ce1c..26a5d80)

## 1.14.0

[change list](https://github.com/canmod/macpan2/compare/3e527d5..cd0ce1c)

## 1.13.0

[change list](https://github.com/canmod/macpan2/compare/c9e4edf..3e527d5)

## 1.12.0

[change list](https://github.com/canmod/macpan2/compare/22a5583..c9e4edf)

## 1.11.1

[change list](https://github.com/canmod/macpan2/compare/3b5cea9..22a5583)

## 1.11.0

[change list](https://github.com/canmod/macpan2/compare/2479418..3b5cea9)

## 1.10.0

[change list](https://github.com/canmod/macpan2/compare/e3a400c..2479418)

## 1.9.1

[change list](https://github.com/canmod/macpan2/compare/e35c139..e3a400c)

## 1.9.0

[change list](https://github.com/canmod/macpan2/compare/2c6325d..e35c139)

## 1.8.1

[change list](https://github.com/canmod/macpan2/compare/36e08b5..2c6325d)

## 1.8.0

[change list](https://github.com/canmod/macpan2/compare/8e72725..36e08b5)

## 1.7.2

[change list](https://github.com/canmod/macpan2/compare/544eada..8e72725)

## 1.6.1

[change list](https://github.com/canmod/macpan2/compare/220a0fd..544eada)

## 1.7.0

[change list](https://github.com/canmod/macpan2/compare/14da8f1..220a0fd)

## 1.6.0

[change list](https://github.com/canmod/macpan2/compare/b52d387..14da8f1)

## 1.5.6

[change list](https://github.com/canmod/macpan2/compare/e780f91..b52d387)

## 1.5.5

[change list](https://github.com/canmod/macpan2/compare/ab997b5..e780f91)

## 1.5.4

[change list](https://github.com/canmod/macpan2/compare/427412d..ab997b5)

## 1.5.3

[change list](https://github.com/canmod/macpan2/compare/61402b3..427412d)

## 1.5.2

[change list](https://github.com/canmod/macpan2/compare/819a504..61402b3)

## 1.5.1

[change list](https://github.com/canmod/macpan2/compare/b9d5858..819a504)

## 1.5.0

[change list](https://github.com/canmod/macpan2/compare/05554e3..b9d5858)


### New Features

* Delete elements from model specifications with `mp_tmb_delete`.

## 1.4.1

[change list](https://github.com/canmod/macpan2/compare/e3c5606..05554e3)

## 1.4.0

[change list](https://github.com/canmod/macpan2/compare/c11a596..e3c5606)


### Breaking Changes

* Repeated calls of an optimizer now start from the previous best parameter
vector
* No longer fit full covariance matrix in `sdreport`s

### Bug Fixes

* Row vectors with names no longer break spec print methods

## 1.3.3

[change list](https://github.com/canmod/macpan2/compare/284485d..c11a596)

## 1.3.2

[change list](https://github.com/canmod/macpan2/compare/ba7c10a..284485d)

## 1.3.1

[change list](https://github.com/canmod/macpan2/compare/b8c918a..ba7c10a)


### Breaking Changes

* No longer exporting `nlist`.
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

* Several bugs related to input handling in `mp_tmb_calibrator` (#176)

### Doc Fixes and Updates

* New installation instructions for installing from `r-universe`.
* New vignette: `real_data`.
* Document `to_name`, `to_names`, `to_labels`, which handle naming of structured objects.
* Document `print` function in the `?engine_functions`.
* `simple_sims` example in `?engine_functions` now runs without error.
* Help file examples for `mp_tmb_insert` and `mp_tmb_update`.
* Readme/vignette examples better expose calls to `library` for the user.

## 1.2.1

[change list](https://github.com/canmod/macpan2/compare/d9334c1..b8c918a)

## 1.2.0

[change list](https://github.com/canmod/macpan2/compare/5eac914..d9334c1)

## 1.1.3

[change list](https://github.com/canmod/macpan2/compare/a10f98b..5eac914)

## 1.1.2

[change list](https://github.com/canmod/macpan2/compare/bb27d2c..a10f98b)

## 1.1.1

[change list](https://github.com/canmod/macpan2/compare/6664f4c..bb27d2c)

## 1.0.1

[change list](https://github.com/canmod/macpan2/compare/c876f7a..6664f4c)

## 1.1.0

[change list](https://github.com/canmod/macpan2/compare/6884145..c876f7a)


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

[change list](https://github.com/canmod/macpan2/compare/f5362d3..6884145)


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

## 0.0.4

[change list](https://github.com/canmod/macpan2/compare/5f72d45..f5362d3)

## 0.0.3

[change list](https://github.com/canmod/macpan2/compare/d11a813..06c656b)


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

[change list](https://github.com/canmod/macpan2/compare/aac0442..d11a813)


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

[change list](https://github.com/canmod/macpan2/compare/92df5b3..aac0442)


* Initial release.
