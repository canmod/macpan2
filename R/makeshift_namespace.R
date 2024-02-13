#' @param internal_names Named list of character vectors giving the names of
#' internal variables. The names of the list identify types of internal
#' variables (e.g. transformed variables, dispersion parameters, observed
#' trajectories). The character vectors themselves identify the external-version
#' of those internal names (``)
#' @noRd
map_names = function(
        external_names = character()
      , internal_name_list = list()
  ) {
  internal_name_vector = (internal_name_list
    |> unlist(use.names = FALSE)
  )
  all_names = (external_names
    |> c(internal_name_vector)
    |> make.unique(sep = "__")
  )
  (all_names[-seq_along(external_names)]
    |> relist(internal_name_list)
  )
}


