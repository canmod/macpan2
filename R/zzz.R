.onLoad <- function(lib, pkg) {
  options(
      macpan2_dll = "macpan2"
    , macpan2_vec_by = c("state", "flow_rates", "trans_rates") |> self_named_vector()
    #, macpan2_memoise = TRUE
  )
}
