.onLoad <- function(lib, pkg) {
  options(
      macpan2_dll = "macpan2"
    , macpan2_verbose = TRUE
    , macpan2_tmb_derivs = TRUE
    , macpan2_default_loss = c("clamped_poisson", "poisson", "sum_of_squares")
      ## FIXME: macpan2_vec_by is old and not relevant i think
    , macpan2_vec_by = c("state", "flow_rates", "trans_rates") |> self_named_vector()
    #, macpan2_memoise = TRUE
  )
}
