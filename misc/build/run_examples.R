library(macpan2)
run_eg = function(f) {
  tmp = tempfile()
  tools::Rd2ex(f, tmp)
  if (file.exists(tmp)) {
    r = try(source(tmp), silent = TRUE)
    if (inherits(r, "try-error")) {
      print(tools::Rd2ex(f))
      stop(r)
    }
  }
}
.trash = lapply(list.files("man", full.names = TRUE), run_eg)
print("success!")
