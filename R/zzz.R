.onLoad <- function(lib, pkg) {
  options(
      macpan2_dll = "macpan2"
    , macpan2_verbose = TRUE
    , macpan2_default_loss = c("clamped_poisson", "poisson", "sum_of_squares", "neg_bin")
    , macpan2_tmb_adfun_args = list()
    
      ## FIXME: macpan2_vec_by is old and not relevant i think
    , macpan2_vec_by = c("state", "flow_rates", "trans_rates") |> self_named_vector()
    
    # functions that cannot be called unless their 
    # first argument has a saved simulation history
    # (TODO: read this off the c++ file)
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
    # (TODO: read this off the c++ file)
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
