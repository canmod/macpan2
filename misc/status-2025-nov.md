## State of `macpan2`

SW brain dump on 2025-11-21.

* Solid capabilities -- with limitations but that are reasonably solid
  * [Topic-based function reference](https://canmod.github.io/macpan2/reference/index.html)
  * [General discrete time dynamical model simulation](https://github.com/canmod/macpan2/tree/main/inst/starter_models/fibonacci)
  * [Scalar state variable compartmental models](https://canmod.github.io/macpan2/articles/quickstart.html) (e.g., [sir](https://github.com/canmod/macpan2/tree/main/inst/starter_models/sir), [shiver](https://github.com/canmod/macpan2/tree/main/inst/starter_models/shiver), [macpan_base](https://github.com/canmod/macpan2/tree/main/inst/starter_models/macpan_base))
  * [Calibrating scalar state variable models to data](https://canmod.github.io/macpan2/articles/calibration.html) -- if you are good at setting starting values
  * Topological sorting, plotting, printing flows and state variables, printing dependence of flows on state variables, adjacency matrices, etc . (e.g., [mp_flow_frame](https://canmod.github.io/macpan2/reference/mp_flow_frame.html), [Dynamic variable names](https://canmod.github.io/macpan2/reference/mp_vars.html), [mp_state_dependence_frame](https://canmod.github.io/macpan2/reference/mp_state_dependence_frame.html), [plot box diagrams](https://canmod.github.io/macpan2/reference/dot_layout.html) other stuff listed [here](https://canmod.github.io/macpan2/reference/index.html#unpack-model-specifications))
  * Easy to extend 'engine functions' used to put together simulation models
  * [Several state-variable updaters for simulating scalar models](https://canmod.github.io/macpan2/reference/state_updates.html), including stochastic models 'choosing' poisson vs multinomial appropriately
  * Calibrating with RK4 for scalar state-variable models, as long as exogenous variables do not change within a time step and you [don't use intermediate variables](https://github.com/canmod/macpan2/issues/288)
  * [Read models saved as RDS files, ensuring consistency with current version](https://canmod.github.io/macpan2/reference/mp_read_rds.html)
* Unfinished / poorly implemented / shelved
  * Likelihoods and priors
    * [Bad docs](https://github.com/canmod/macpan2/issues/343)
    * Looking at example usage only real option
    * You are always allowed to specify your own objective function
  * Cluster of time-related issues
    * Stitching together simulation phases
      * Start-date offsets / warmup
      * Going forward I recommend handling this all yourself
      * But I'm worried that handling it yourself will currently be confusing
    * Time-varying parameters as a first class citizen
    * Non-time-step indexed models
      * e.g., datetime columns in observed trajectories and/or time-varying parameters
    * The biggest priority for me, if I had time, would be to strip out all the completely introduced to make this area 'just work' so that it is at least simple to understand and therefore handle yourself
  * Models with structure
    * Several attempts made
    * The first attempt at complete generality failed
    * [Unfinished plan to do simple product models](https://github.com/canmod/macpan2/issues/288), kind of works already as long as you don't what to use stochastic state-updaters (as shown by [Irena's age model](https://github.com/canmod/macpan2/tree/main/inst/starter_models/sir_age))
* Potentially finish-able branches -- low hanging fruit that makes me sad for not finishing / merging
  * Early stopping #339
  * Test handling of missing time, matrix cols #344
  * Delta-t #314
* Never attempted / often discussed
  * Next-generation matrix approach to R0 etc
  * Fitting models with process error
