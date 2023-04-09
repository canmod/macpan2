#' Compartmental Model
#'
#' Create an object for containing a compartmental model.
#'
#' @param model_directory String giving a path to a directory containing
#' the following files, `variables.csv`, `derivations.json`, `flows.csv`,
#' and `settings.json`, described by
#' [this spec](https://canmod.github.io/macpan2/articles/model_definitions.html).
#'
#' @export
Compartmental = function(model_directory) {
  self = Model(ModelFiles(model_directory))
  return_object(self, "Compartmental")
}
