#' @param internal_names Named list of character vectors giving the names of
#' internal variables. The names of the list identify types of internal
#' variables (e.g. transformed variables, dispersion parameters, observed
#' trajectories). The character vectors themselves identify the global-version
#' of those internal names (``)
#' @importFrom utils relist
#' @noRd
map_names = function(
        existing_names = character()
      , local_names = list()
  ) {
  local_name_vector = (local_names
    |> unlist(use.names = FALSE)
  )
  all_names = (existing_names
    |> c(local_name_vector)
    |> make.unique(sep = "__")
  )
  (all_names[-seq_along(existing_names)]
    |> relist(local_names)
  )
}


