dev_in_root = function() "inst" %in% list.dirs(full.names = FALSE)
dev_in_dev = function() "dev.cpp" %in% list.files(full.names = FALSE)
dev_in_test = function() "testthat" %in% list.dirs("..", full.names = FALSE)
## TODO: dev_in_test or something like that

dev_file = function(suffix = "", ext = "cpp") {
  cpp = function(path) {
    pp = file.path(path, sprintf("dev%s.%s", suffix, ext))
    print("YESYYSYSE")
    print(pp)
    print(file.exists(pp))
    print(getwd())
    pp
  }
  if (dev_in_root()) return(cpp("misc/dev"))
  if (dev_in_dev()) return(cpp(""))
  if (dev_in_test()) return(cpp("../../misc/dev"))
  stop(
    "\n------",
    "\nYou are developing here:\n", getwd(), ",",
    "\nwhich is not where you should be developing.",
    "\nThe current options are in the root of macpan2,",
    "\nor in misc/dev or tests within a macpan2 project.",
    "\n------"
  )
}

dev_obj = function(suffix = "", ext = "cpp") {
  tools::file_path_sans_ext(dev_file(suffix = suffix, ext = ext))
}

dev_choose_cpp = function(suffix = "", ext = "cpp") {
  if (interactive()) return("macpan2")
  dev_compile(suffix = suffix, ext = ext)
  dev_obj(suffix = suffix, ext = ext)
}

dev_compile = function(suffix = "", ext = "cpp") {
  ff = dev_file(suffix = suffix, ext = ext)
  TMB::compile(ff)
  dyn.load(TMB::dynlib(dev_obj(suffix = suffix, ext = ext)))
}
