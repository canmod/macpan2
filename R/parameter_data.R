#' @export
read_example <- function(
    example = c(
      "seir", "twostrain"
    ),
    component = c(
      "state", "param"
    )
  ) {
  example = match.arg(example)
  example_components = match.arg(component)
  example_csv = paste0(tools::file_path_sans_ext(example), '.csv')
  file_nms = paste(example_components, example_csv, sep = "_")
  file_paths = system.file('param_examples', file_nms, package = 'macpan2')
  d = read.csv(file_paths, stringsAsFactors = FALSE, colClasses = 'character')
  d[names(d) != "notes"]
}
