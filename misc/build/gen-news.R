narratives = readLines("news-narratives.md")
version_heading = grep("^## [0-9]+\\.[0-9]+\\.[0-9]+", narratives)
versions_with_narratives = sub("^## ([0-9]+\\.[0-9]+\\.[0-9]+)", "\\1", narratives[version_heading])
narrative_list = mapply(\(start, end) narratives[start:end], version_heading + 1, c(version_heading[-1] - 1, length(narratives)), SIMPLIFY = FALSE) |> setNames(versions_with_narratives)
links = system("misc/build/gen-compare-versions-link.sh", intern = TRUE)
link_parts = strsplit(links, " : ")
just_links = setNames(
    vapply(link_parts, getElement, character(1L), 2L)
  , vapply(link_parts, getElement, character(1L), 1L)
)
versions_all = names(just_links)

output_list = list()
for (version in versions_all) {
  output_list[[version]] = c(
      sprintf("## %s", version), ""
    , just_links[[version]], ""
  )
  if (version %in% versions_with_narratives) {
    output_list[[version]] = c(output_list[[version]], narrative_list[[version]])
  }
}
output = unlist(output_list, use.names = FALSE)
writeLines(output, "NEWS.md")
