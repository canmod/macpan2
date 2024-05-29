#' @param existing_names Character vector of names that are already global
#' in a model.
#' @param local_names A list with names giving local versions of the names
#' in the sub-model within a broader model (e.g. convolution within SIR),
#' and with values giving the 'proposed' global names.
#' @return A version of `local_names` with the values modified to not conflict
#' with names in `existing_names`.
#' @examples
#' map_names("g", list(g = "g")) # equal to list(g = "g__1")
#' map_names("g", list(g = "G")) # equal to list(g = "G")
#' 
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
  if (length(existing_names) > 0L) {
    local_fixed_names = all_names[-seq_along(existing_names)]
  } else {
    local_fixed_names = all_names
  }
  relist(local_fixed_names, local_names)
}
