md5 = function(x) {
  f = tempfile()
  x |> as.character() |> writeLines(f)
  tools::md5sum(f) |> unname()
}
md5(x ~ y + 2)
