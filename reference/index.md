# Package index

## Specifications

### Create Model Specifications

Functions for creating new model specification objects, which describe
dynamical models with special support for compartmental modelling. The
[`vignette("quickstart")`](https://canmod.github.io/macpan2/articles/quickstart.md)
article describes how these functions are used in context.

- Create TMB Model Specification::

  [`mp_tmb_model_spec()`](https://canmod.github.io/macpan2/reference/mp_tmb_model_spec.md)

&nbsp;

- Specify Flow Into, Out Of, and Between Compartments::

  [`mp_per_capita_flow()`](https://canmod.github.io/macpan2/reference/mp_per_capita_flow.md)
  [`mp_per_capita_inflow()`](https://canmod.github.io/macpan2/reference/mp_per_capita_flow.md)
  [`mp_per_capita_outflow()`](https://canmod.github.io/macpan2/reference/mp_per_capita_flow.md)
  [`mp_inflow()`](https://canmod.github.io/macpan2/reference/mp_per_capita_flow.md)
  [`mp_outflow()`](https://canmod.github.io/macpan2/reference/mp_per_capita_flow.md)

&nbsp;

- Specify Absolute Flow Between Compartments (Experimental)::

  [`mp_absolute_flow()`](https://canmod.github.io/macpan2/reference/mp_absolute_flow.md)

&nbsp;

- Functions Available in the Simulation Engine::

  [`engine_functions`](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`` `+` ``](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`` `-` ``](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`` `*` ``](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`` `/` ``](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`` `^` ``](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`exp`](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`log`](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`` `(` ``](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`c`](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`matrix`](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`` `%*%` ``](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`sum`](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`rep`](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`row_sums`](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`col_sums`](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`group_sums`](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`` `[` ``](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`block`](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`t`](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`rbind_time`](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`rbind_lag`](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`cbind_time`](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`cbind_lag`](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`` `:` ``](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`seq`](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`convolution`](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`cbind`](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`rbind`](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`time_step`](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`recycle`](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`clamp`](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`dpois`](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`dnbinom`](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`dnorm`](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`rpois`](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`rnbinom`](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`rnorm`](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`` `%x%` ``](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`to_diag`](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`from_diag`](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`time_group`](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`cos`](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`print`](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`time_var`](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`rbinom`](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`reulermultinom`](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`round`](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`pgamma`](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`mean`](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`sd`](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`proportions`](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`last`](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`check_finite`](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`dbinom`](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`sin`](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`sqrt`](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`pnorm`](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`invlogit`](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`logit`](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`cumsum`](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`sparse_mat_mult`](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`divide_safe`](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`assign`](https://canmod.github.io/macpan2/reference/engine_functions.md)
  [`unpack`](https://canmod.github.io/macpan2/reference/engine_functions.md)

&nbsp;

- Optimized Model Specification::

  [`mp_optimized_spec()`](https://canmod.github.io/macpan2/reference/mp_optimized_spec.md)

### Find Model Specifications

Functions for finding, copying, and loading existing model specification
objects from model libraries. The
[`vignette("example_models")`](https://canmod.github.io/macpan2/articles/example_models.md)
article describes how these functions are used in context.

- Read Item from a Model Library::

  [`mp_tmb_library()`](https://canmod.github.io/macpan2/reference/mp_tmb_library.md)
  [`mp_tmb_entire_library()`](https://canmod.github.io/macpan2/reference/mp_tmb_library.md)
  [`mp_official_library()`](https://canmod.github.io/macpan2/reference/mp_tmb_library.md)

&nbsp;

- Print a Table of Existing Models::

  [`mp_show_models()`](https://canmod.github.io/macpan2/reference/mp_show_models.md)
  [`show_models()`](https://canmod.github.io/macpan2/reference/mp_show_models.md)
  [`mp_list_models()`](https://canmod.github.io/macpan2/reference/mp_show_models.md)

&nbsp;

- Copy Existing Model as a Starting Point::

  [`mp_model_starter()`](https://canmod.github.io/macpan2/reference/mp_model_starter.md)

&nbsp;

- Browse Model Docs::

  [`mp_model_docs()`](https://canmod.github.io/macpan2/reference/mp_model_docs.md)

&nbsp;

- Read Serialized Model Specification::

  [`mp_read_rds()`](https://canmod.github.io/macpan2/reference/mp_read_rds.md)

### Transform Model Specifications

Functions that take a model specification as input and return a modified
version of that specification.

- Transform a TMB Model Specification::

  [`mp_tmb_insert()`](https://canmod.github.io/macpan2/reference/mp_tmb_insert.md)
  [`mp_tmb_update()`](https://canmod.github.io/macpan2/reference/mp_tmb_insert.md)
  [`mp_tmb_delete()`](https://canmod.github.io/macpan2/reference/mp_tmb_insert.md)

&nbsp;

- Transform a TMB Model Specification to Account for Reporting Bias::

  [`mp_tmb_insert_reports()`](https://canmod.github.io/macpan2/reference/mp_tmb_insert_reports.md)

&nbsp;

- Insert Log Linear Model of Time Variation (Experimental)::

  [`mp_tmb_insert_log_linear()`](https://canmod.github.io/macpan2/reference/mp_tmb_insert_log_linear.md)

&nbsp;

- Insert GLM Time Variation::

  [`mp_tmb_insert_glm_timevar()`](https://canmod.github.io/macpan2/reference/mp_tmb_insert_glm_timevar.md)

&nbsp;

- Insert Basic Transformations of Model Variables::

  [`mp_tmb_insert_trans()`](https://canmod.github.io/macpan2/reference/mp_tmb_insert_trans.md)
  [`mp_tmb_implicit_trans()`](https://canmod.github.io/macpan2/reference/mp_tmb_insert_trans.md)

&nbsp;

- Insert Back Transformations of Model Parameters::

  [`mp_tmb_insert_backtrans()`](https://canmod.github.io/macpan2/reference/mp_tmb_insert_backtrans.md)
  [`mp_tmb_implicit_backtrans()`](https://canmod.github.io/macpan2/reference/mp_tmb_insert_backtrans.md)

&nbsp;

- Change How State Variables are Updated::

  [`mp_euler()`](https://canmod.github.io/macpan2/reference/state_updates.md)
  [`mp_rk4()`](https://canmod.github.io/macpan2/reference/state_updates.md)
  [`mp_rk4_old()`](https://canmod.github.io/macpan2/reference/state_updates.md)
  [`mp_euler_multinomial()`](https://canmod.github.io/macpan2/reference/state_updates.md)
  [`mp_discrete_stoch()`](https://canmod.github.io/macpan2/reference/state_updates.md)
  [`mp_hazard()`](https://canmod.github.io/macpan2/reference/state_updates.md)

&nbsp;

- Expand Model::

  [`mp_expand()`](https://canmod.github.io/macpan2/reference/mp_expand.md)
  [`mp_reduce()`](https://canmod.github.io/macpan2/reference/mp_expand.md)

&nbsp;

- Version Update::

  [`mp_version_update()`](https://canmod.github.io/macpan2/reference/mp_version_update.md)

### Unpack Model Specifications

Functions that extract, print, and/or plot information contained within
model specifications.

- Print Model Specification::

  [`mp_print_spec()`](https://canmod.github.io/macpan2/reference/mp_print_spec.md)
  [`mp_print_before()`](https://canmod.github.io/macpan2/reference/mp_print_spec.md)
  [`mp_print_during()`](https://canmod.github.io/macpan2/reference/mp_print_spec.md)
  [`mp_print_after()`](https://canmod.github.io/macpan2/reference/mp_print_spec.md)

&nbsp;

- Initial Values of Variables Immediately Before the Simulation Loop::

  [`mp_initial()`](https://canmod.github.io/macpan2/reference/mp_initial.md)
  [`mp_initial_list()`](https://canmod.github.io/macpan2/reference/mp_initial.md)

&nbsp;

- Default Values::

  [`mp_default()`](https://canmod.github.io/macpan2/reference/mp_default.md)
  [`mp_default_list()`](https://canmod.github.io/macpan2/reference/mp_default.md)

&nbsp;

- Data Frame Describing Compartmental Model Flows::

  [`mp_flow_frame()`](https://canmod.github.io/macpan2/reference/mp_flow_frame.md)

&nbsp;

- Data Frame Describing Each Change to Each State Variable::

  [`mp_change_frame()`](https://canmod.github.io/macpan2/reference/mp_change_frame.md)

&nbsp;

- Dynamic Variable Names::

  [`mp_state_vars()`](https://canmod.github.io/macpan2/reference/mp_vars.md)
  [`mp_flow_vars()`](https://canmod.github.io/macpan2/reference/mp_vars.md)
  [`mp_state_flow_vars()`](https://canmod.github.io/macpan2/reference/mp_vars.md)
  [`mp_dynamic_vars()`](https://canmod.github.io/macpan2/reference/mp_vars.md)
  [`mp_other_dynamic_vars()`](https://canmod.github.io/macpan2/reference/mp_vars.md)

&nbsp;

- Data Frame Describing State Dependent Per-Capita Flow Rates::

  [`mp_state_dependence_frame()`](https://canmod.github.io/macpan2/reference/mp_state_dependence_frame.md)

&nbsp;

- Version of `macpan2`::

  [`mp_version()`](https://canmod.github.io/macpan2/reference/mp_version.md)

&nbsp;

- Create a Graph from a Model Specification::

  [`mp_dot_layout()`](https://canmod.github.io/macpan2/reference/dot_layout.md)
  [`dot_layout()`](https://canmod.github.io/macpan2/reference/dot_layout.md)

&nbsp;

- Adjacency Matrix::

  [`mp_adjacency()`](https://canmod.github.io/macpan2/reference/mp_adjacency.md)

&nbsp;

- Find all Paths Through Compartments::

  [`find_all_paths()`](https://canmod.github.io/macpan2/reference/find_all_paths.md)

## Simulators

### Create New Model Simulators

Functions for creating a model simulator object.

- Create a Simulator::

  [`mp_simulator()`](https://canmod.github.io/macpan2/reference/mp_simulator.md)

### Generate Simulations

Functions that generate simulations from a simulator object.

- Simulate Dynamical Model Trajectories::

  [`mp_trajectory()`](https://canmod.github.io/macpan2/reference/mp_trajectory.md)
  [`mp_trajectory_par()`](https://canmod.github.io/macpan2/reference/mp_trajectory.md)
  [`mp_trajectory_sd()`](https://canmod.github.io/macpan2/reference/mp_trajectory.md)
  [`mp_trajectory_ensemble()`](https://canmod.github.io/macpan2/reference/mp_trajectory.md)
  [`mp_trajectory_sim()`](https://canmod.github.io/macpan2/reference/mp_trajectory.md)
  [`mp_trajectory_replicate()`](https://canmod.github.io/macpan2/reference/mp_trajectory.md)

&nbsp;

- Initial Values of Variables Immediately Before the Simulation Loop::

  [`mp_initial()`](https://canmod.github.io/macpan2/reference/mp_initial.md)
  [`mp_initial_list()`](https://canmod.github.io/macpan2/reference/mp_initial.md)

&nbsp;

- Final Values::

  [`mp_final()`](https://canmod.github.io/macpan2/reference/mp_final.md)
  [`mp_final_list()`](https://canmod.github.io/macpan2/reference/mp_final.md)

### Unpack Model Simulators

Functions that extract or print information contained within model
simulators.

- Default Values::

  [`mp_default()`](https://canmod.github.io/macpan2/reference/mp_default.md)
  [`mp_default_list()`](https://canmod.github.io/macpan2/reference/mp_default.md)

&nbsp;

- Get Underlying TMB Object::

  [`mp_tmb()`](https://canmod.github.io/macpan2/reference/mp_tmb.md)

&nbsp;

- Version of `macpan2`::

  [`mp_version()`](https://canmod.github.io/macpan2/reference/mp_version.md)

## Calibrators

### Create Model Calibrator

Create a simulator that can be calibrated to data

- Make a Calibrator::

  [`mp_tmb_calibrator()`](https://canmod.github.io/macpan2/reference/mp_tmb_calibrator.md)

&nbsp;

- Fit Parameters::

  [`mp_par()`](https://canmod.github.io/macpan2/reference/mp_par.md)

&nbsp;

- Trajectory Specification::

  [`mp_traj()`](https://canmod.github.io/macpan2/reference/mp_traj.md)

&nbsp;

- Fit a Time-Varying Parameter with Radial Basis Functions::

  [`mp_rbf()`](https://canmod.github.io/macpan2/reference/mp_rbf.md)

&nbsp;

- Link Functions and Transformation::

  [`mp_identity`](https://canmod.github.io/macpan2/reference/transform_distr_param.md)
  [`mp_log`](https://canmod.github.io/macpan2/reference/transform_distr_param.md)
  [`mp_log1p`](https://canmod.github.io/macpan2/reference/transform_distr_param.md)
  [`mp_logit`](https://canmod.github.io/macpan2/reference/transform_distr_param.md)
  [`mp_sqrt`](https://canmod.github.io/macpan2/reference/transform_distr_param.md)

&nbsp;

- Simulation Bounds::

  [`mp_sim_bounds()`](https://canmod.github.io/macpan2/reference/mp_sim_bounds.md)

&nbsp;

- Simulation Offsets::

  [`mp_sim_offset()`](https://canmod.github.io/macpan2/reference/mp_sim_offset.md)

&nbsp;

- Time Scale::

  [`mp_time_scale()`](https://canmod.github.io/macpan2/reference/mp_time_scale.md)

### Specify Distributional Assumptions

Functions for specifying distributional assumptions for priors and
likelihoods.

- Distributions::

  [`mp_unif()`](https://canmod.github.io/macpan2/reference/distribution.md)
  [`mp_uniform()`](https://canmod.github.io/macpan2/reference/distribution.md)
  [`mp_norm()`](https://canmod.github.io/macpan2/reference/distribution.md)
  [`mp_normal()`](https://canmod.github.io/macpan2/reference/distribution.md)
  [`mp_lnorm()`](https://canmod.github.io/macpan2/reference/distribution.md)
  [`mp_log_normal()`](https://canmod.github.io/macpan2/reference/distribution.md)
  [`mp_logitnorm()`](https://canmod.github.io/macpan2/reference/distribution.md)
  [`mp_logit_normal()`](https://canmod.github.io/macpan2/reference/distribution.md)
  [`mp_pois()`](https://canmod.github.io/macpan2/reference/distribution.md)
  [`mp_poisson()`](https://canmod.github.io/macpan2/reference/distribution.md)
  [`mp_nbinom()`](https://canmod.github.io/macpan2/reference/distribution.md)
  [`mp_neg_bin()`](https://canmod.github.io/macpan2/reference/distribution.md)

&nbsp;

- Fitting Distributional Parameters::

  [`mp_fit()`](https://canmod.github.io/macpan2/reference/fit_distr_params.md)
  [`mp_nofit()`](https://canmod.github.io/macpan2/reference/fit_distr_params.md)

### Optimize Calibrator Model

Functions that take a model calibrator as input and calibrate model
parameters, as well as functions that extract underlying objects used
during the opimization/calibration process.

- Optimize Simulation Model::

  [`mp_optimize()`](https://canmod.github.io/macpan2/reference/mp_optimize.md)

&nbsp;

- Optimizer Output::

  [`mp_optimizer_output()`](https://canmod.github.io/macpan2/reference/mp_optimizer_output.md)

&nbsp;

- Optimized Model Specification::

  [`mp_optimized_spec()`](https://canmod.github.io/macpan2/reference/mp_optimized_spec.md)

&nbsp;

- Value of the Objective Function of a Model::

  [`mp_tmb_objective()`](https://canmod.github.io/macpan2/reference/mp_tmb_objective.md)

### Generate Calibrated Simulations, Parameters, and Forecasts

Functions that generate simulations and tables of fitted parameters from
a calibator object.

- Simulate Dynamical Model Trajectories::

  [`mp_trajectory()`](https://canmod.github.io/macpan2/reference/mp_trajectory.md)
  [`mp_trajectory_par()`](https://canmod.github.io/macpan2/reference/mp_trajectory.md)
  [`mp_trajectory_sd()`](https://canmod.github.io/macpan2/reference/mp_trajectory.md)
  [`mp_trajectory_ensemble()`](https://canmod.github.io/macpan2/reference/mp_trajectory.md)
  [`mp_trajectory_sim()`](https://canmod.github.io/macpan2/reference/mp_trajectory.md)
  [`mp_trajectory_replicate()`](https://canmod.github.io/macpan2/reference/mp_trajectory.md)

&nbsp;

- TMB Model Coefficient Table::

  [`mp_tmb_coef()`](https://canmod.github.io/macpan2/reference/mp_tmb_coef.md)

&nbsp;

- Describe Statistical Effects::

  [`mp_effects_descr()`](https://canmod.github.io/macpan2/reference/mp_effects_descr.md)
  [`mp_add_effects_descr()`](https://canmod.github.io/macpan2/reference/mp_effects_descr.md)

&nbsp;

- Covariance of Fixed Effect Estimates::

  [`mp_tmb_fixef_cov()`](https://canmod.github.io/macpan2/reference/mp_tmb_fixef_cov.md)

&nbsp;

- Model Coefficient Table with stan::

  [`mp_tmbstan_coef()`](https://canmod.github.io/macpan2/reference/mp_tmbstan_coef.md)

&nbsp;

- TMB Likelihood Profiling::

  [`mp_tmb_profile()`](https://canmod.github.io/macpan2/reference/mp_tmb_profile.md)

&nbsp;

- Make a Forecaster::

  [`mp_forecaster()`](https://canmod.github.io/macpan2/reference/mp_forecaster.md)

### Unpack Model Calibrators and Calibrations

Functions that extract or print information contained within model
calibrators.

- Get Underlying TMB Object::

  [`mp_tmb()`](https://canmod.github.io/macpan2/reference/mp_tmb.md)

&nbsp;

- Description of Model Parameterization::

  [`mp_parameterization()`](https://canmod.github.io/macpan2/reference/mp_parameterization.md)

&nbsp;

- Version of `macpan2`::

  [`mp_version()`](https://canmod.github.io/macpan2/reference/mp_version.md)

&nbsp;

- Optimization Attempted::

  [`mp_opt_attempted()`](https://canmod.github.io/macpan2/reference/mp_opt_attempted.md)

&nbsp;

- Uncertainty Estimated::

  [`mp_uncertainty_estimated()`](https://canmod.github.io/macpan2/reference/mp_uncertainty_estimated.md)

&nbsp;

- Print Objective Function::

  [`mp_print_obj_fn()`](https://canmod.github.io/macpan2/reference/mp_print_obj_fn.md)

## Utilities

### Lightweight Simulation Utilities

Simple functions for simulation without simulator objects, so that the
[`?engine_functions`](https://canmod.github.io/macpan2/reference/engine_functions.md)
can be explored in isolation from compartmental models.

- Engine Evaluation::

  [`engine_eval()`](https://canmod.github.io/macpan2/reference/engine_eval.md)

&nbsp;

- Simple Iterated Simulation::

  [`simple_sims()`](https://canmod.github.io/macpan2/reference/simple_sims.md)

### Creating Matrices, Vectors, and Lists

Utilities for constructing matrices, vectors, and lists for use with
`macpan2`.

- Self Naming List::

  [`nlist()`](https://canmod.github.io/macpan2/reference/nlist.md)

&nbsp;

- Empty Matrix::

  [`empty_matrix`](https://canmod.github.io/macpan2/reference/empty_matrix.md)

&nbsp;

- Empty Trajectory::

  [`empty_trajectory`](https://canmod.github.io/macpan2/reference/empty_trajectory.md)

&nbsp;

- Zero Vector::

  [`mp_zero_vector()`](https://canmod.github.io/macpan2/reference/mp_zero_vector.md)

&nbsp;

- Radial Basis Functions::

  [`rbf()`](https://canmod.github.io/macpan2/reference/rbf.md)

&nbsp;

- Extract Sparse Matrix Notation from a Dense Matrix::

  [`sparse_matrix_notation()`](https://canmod.github.io/macpan2/reference/sparse_matrix_notation.md)

&nbsp;

- Binary Operator::

  [`BinaryOperator()`](https://canmod.github.io/macpan2/reference/mp_binary_operator.md)
  [`mp_binary_operator()`](https://canmod.github.io/macpan2/reference/mp_binary_operator.md)

&nbsp;

- Kronecker Operator::

  [`mp_kronecker_operator()`](https://canmod.github.io/macpan2/reference/mp_kronecker_operator.md)

### Developer and Power-User Utilities

Utilities for `macpan2` developers and power-users.

- SI Example::

  [`si_example_object()`](https://canmod.github.io/macpan2/reference/si_example.md)
  [`si_example_code()`](https://canmod.github.io/macpan2/reference/si_example.md)

&nbsp;

- Expression List::

  [`mp_tmb_expr_list()`](https://canmod.github.io/macpan2/reference/mp_tmb_expr_list.md)

&nbsp;

- Functions Used by an Object for Communicating with a Computational
  Engine::

  [`mp_functions_used()`](https://canmod.github.io/macpan2/reference/mp_functions_used.md)
  [`mp_generates_randomness()`](https://canmod.github.io/macpan2/reference/mp_functions_used.md)

&nbsp;

- Reader::

  [`Reader()`](https://canmod.github.io/macpan2/reference/Reader.md)
  [`CSVReader()`](https://canmod.github.io/macpan2/reference/Reader.md)
  [`JSONReader()`](https://canmod.github.io/macpan2/reference/Reader.md)
  [`TXTReader()`](https://canmod.github.io/macpan2/reference/Reader.md)
  [`RReader()`](https://canmod.github.io/macpan2/reference/Reader.md)
  [`NULLReader()`](https://canmod.github.io/macpan2/reference/Reader.md)

&nbsp;

- Transform::

  [`Transform()`](https://canmod.github.io/macpan2/reference/Transform.md)
  [`Identity()`](https://canmod.github.io/macpan2/reference/Transform.md)
  [`Log()`](https://canmod.github.io/macpan2/reference/Transform.md)
  [`Logit()`](https://canmod.github.io/macpan2/reference/Transform.md)

## Not Ready

### Box Diagrams

Functions for semi-automatically laying out compartments in model
specifications as boxes on a plot.

- Flow Diagram Grid Layout (experimental)::

  [`mp_layout_grid()`](https://canmod.github.io/macpan2/reference/mp_layout_grid.md)

&nbsp;

- Flow Diagram Grid Layout (experimental)::

  [`mp_layout_paths()`](https://canmod.github.io/macpan2/reference/mp_layout_paths.md)

### Structured Populations (Not Ready)

Functions for specifying population structure (e.g., age, space,
immunity status).

- Dynamic Model::

  [`mp_dynamic_model()`](https://canmod.github.io/macpan2/reference/mp_dynamic_model.md)

&nbsp;

- TMB Simulator from Dynamic Model::

  [`mp_dynamic_simulator()`](https://canmod.github.io/macpan2/reference/mp_dynamic_simulator.md)

&nbsp;

- Ledgers::

  [`LedgerDefinition`](https://canmod.github.io/macpan2/reference/LedgerDefinition.md)

&nbsp;

- Cartesian Product of Index Tables::

  [`mp_cartesian()`](https://canmod.github.io/macpan2/reference/mp_cartesian.md)

&nbsp;

- Extract Index::

  [`mp_extract()`](https://canmod.github.io/macpan2/reference/mp_extract.md)

&nbsp;

- Model Quantity Index Table::

  [`mp_index()`](https://canmod.github.io/macpan2/reference/mp_index.md)
  [`print(`*`<Index>`*`)`](https://canmod.github.io/macpan2/reference/mp_index.md)
  [`names(`*`<Index>`*`)`](https://canmod.github.io/macpan2/reference/mp_index.md)

&nbsp;

- Join Indexes::

  [`mp_join()`](https://canmod.github.io/macpan2/reference/mp_join.md)

&nbsp;

- Linear Chain Product::

  [`mp_linear()`](https://canmod.github.io/macpan2/reference/mp_linear.md)

&nbsp;

- Position Vectors::

  [`mp_positions()`](https://canmod.github.io/macpan2/reference/mp_positions.md)

&nbsp;

- Reference Index::

  [`mp_reference()`](https://canmod.github.io/macpan2/reference/mp_reference.md)

&nbsp;

- Rename Index Columns::

  [`mp_rename()`](https://canmod.github.io/macpan2/reference/mp_rename.md)

&nbsp;

- Self Cartesian Product::

  [`mp_square()`](https://canmod.github.io/macpan2/reference/mp_square.md)

&nbsp;

- Symmetric Self Cartesian Product::

  [`mp_symmetric()`](https://canmod.github.io/macpan2/reference/mp_symmetric.md)

&nbsp;

- Self Cartesian Product Excluding One Off-Diagonal Side::

  [`mp_triangle()`](https://canmod.github.io/macpan2/reference/mp_triangle.md)

&nbsp;

- Factor an Index::

  [`mp_factors()`](https://canmod.github.io/macpan2/reference/mp_factors.md)

&nbsp;

- Group an Index::

  [`mp_group()`](https://canmod.github.io/macpan2/reference/mp_group.md)

&nbsp;

- Index Labels::

  [`mp_labels()`](https://canmod.github.io/macpan2/reference/mp_labels.md)

&nbsp;

- Bundle up Ledgers::

  [`mp_ledgers()`](https://canmod.github.io/macpan2/reference/mp_ledgers.md)

&nbsp;

- Lookup::

  [`mp_lookup()`](https://canmod.github.io/macpan2/reference/mp_lookup.md)

&nbsp;

- Slice an index::

  [`mp_slices()`](https://canmod.github.io/macpan2/reference/mp_slices.md)

&nbsp;

- Structured Vectors::

  [`mp_structured_vector()`](https://canmod.github.io/macpan2/reference/mp_structured_vector.md)
  [`mp_set_numbers()`](https://canmod.github.io/macpan2/reference/mp_structured_vector.md)

&nbsp;

- Subset of Indexes::

  [`mp_subset()`](https://canmod.github.io/macpan2/reference/mp_subset.md)
  [`mp_setdiff()`](https://canmod.github.io/macpan2/reference/mp_subset.md)

&nbsp;

- Union of Indexes::

  [`mp_union()`](https://canmod.github.io/macpan2/reference/mp_union.md)

&nbsp;

- Names and Labels::

  [`to_labels()`](https://canmod.github.io/macpan2/reference/names_and_labels.md)
  [`to_names()`](https://canmod.github.io/macpan2/reference/names_and_labels.md)
  [`to_name()`](https://canmod.github.io/macpan2/reference/names_and_labels.md)
  [`to_name_pairs()`](https://canmod.github.io/macpan2/reference/names_and_labels.md)
  [`to_values()`](https://canmod.github.io/macpan2/reference/names_and_labels.md)

&nbsp;

- To Positions::

  [`to_positions()`](https://canmod.github.io/macpan2/reference/to_positions.md)

&nbsp;

- To String::

  [`to_string()`](https://canmod.github.io/macpan2/reference/to_string.md)

&nbsp;

- Aggregate an Index::

  [`mp_aggregate()`](https://canmod.github.io/macpan2/reference/mp_aggregate.md)

&nbsp;

- String Data::

  [`StringDataFromFrame()`](https://canmod.github.io/macpan2/reference/StringData.md)
  [`StringDataFromDotted()`](https://canmod.github.io/macpan2/reference/StringData.md)
  [`print(`*`<StringData>`*`)`](https://canmod.github.io/macpan2/reference/StringData.md)

&nbsp;

- Comparison Functions::

  [`all_equal()`](https://canmod.github.io/macpan2/reference/comparison.md)
  [`all_consistent()`](https://canmod.github.io/macpan2/reference/comparison.md)
  [`not_all_equal()`](https://canmod.github.io/macpan2/reference/comparison.md)
  [`all_not_equal()`](https://canmod.github.io/macpan2/reference/comparison.md)
