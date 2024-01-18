library(macpan2)
run_eg = function(f) {
  print(f)
  tmp = tempfile()
  tools::Rd2ex(f, tmp)
  if (file.exists(tmp)) {
    print("Found examples ...")
    r = try(source(tmp), silent = TRUE)
    if (inherits(r, "try-error")) {
      print("FAIL")
      print(tools::Rd2ex(f))
      stop(r)
    }
  }
}
rd_files = list.files("man"
  , full.names = TRUE
  , pattern = "*.Rd"
  , include.dirs = FALSE
)
.trash = lapply(rd_files, run_eg)
print("success!")
