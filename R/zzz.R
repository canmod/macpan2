.onLoad <- function(lib, pkg) {
  options(
      macpan2_dll = "macpan2"
    , macpan2_verbose = TRUE
    , macpan2_default_loss = c("clamped_poisson", "poisson", "sum_of_squares", "neg_bin")
    , macpan2_tmb_type = "ADFun"
    , macpan2_tmb_check = TRUE
      ## FIXME: macpan2_vec_by is old and not relevant i think
    , macpan2_vec_by = c("state", "flow_rates", "trans_rates") |> self_named_vector()
    #, macpan2_memoise = TRUE
    
    
    # tolerances
    , macpan2_tol_hazard_div = 1e-8
    
    # functions that cannot be called unless their 
    # first argument has a saved simulation history
    , macpan2_time_dep_funcs = c(
         "convolution"
        ,"rbind_lag"
        ,"rbind_time"
        ,"cbind_lag"
        ,"cbind_time"
      )
    
    # functions that cannot be called repeatedly 
    # _within_ a single time-step (as would
    # happen for example with RK4 state updates)
    , macpan2_non_iterable_funcs = c(
          "time_var"
        , "rbinom"
        , "rpois"
        , "rnorm"
        , "rnbinom"
        , "reulermultinom"
    )
  )
}
