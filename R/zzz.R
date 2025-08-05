#' @importFrom tools R_user_dir
.onLoad <- function(lib, pkg) {
  
  default = dirname(bail_out_log_file)
  if (!dir.exists(default)) {
    dir.create(default, showWarnings = FALSE, recursive = TRUE)
  }
  
  ## document these in vignettes/options.Rmd
  options(
      macpan2_dll = "macpan2"
    , macpan2_verbose = FALSE
    , macpan2_default_loss = c("clamped_poisson", "poisson", "sum_of_squares", "neg_bin")
    , macpan2_tmb_type = NULL
    , macpan2_tmb_check = TRUE
    , macpan2_saving_conflict_msg_fn = base::message
    , macpan2_traj_tmb_macro = c("simulate", "report")
      
    ## FIXME: macpan2_vec_by is old and not relevant i think
    , macpan2_vec_by = c("state", "flow_rates", "trans_rates") |> self_named_vector()
    
    # where the log files go (e.g. `{macpan2_log_dir}/{macpan2_session_name}/log.txt`)
    , macpan2_session_name = "default"
    , macpan2_log_dir = tools::R_user_dir("macpan2")
    
    # tolerances
    , macpan2_tol_hazard_div = 1e-8
    , macpan2_tol_singular_cov = 1e-6
    
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
    # happen for example with RK4 state updates).
    # randomness and time-variation are the only
    # examples we have now.
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
