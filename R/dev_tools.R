dev_in_root = function() "inst" %in% list.dirs(full.names = FALSE)
dev_in_dev = function() "dev.cpp" %in% list.files(full.names = FALSE)
## TODO: dev_in_test or something like that

dev_file = function() {
  cpp = "misc/dev/dev.cpp"
  if (dev_in_root()) return(cpp)
  if (dev_in_dev()) return(basename(cpp))
  stop(
    "\n------",
    "\nYou are not developing where you should be developing.",
    "\nThe current options are in the root of macpan2 or in",
    "\nmisc/dev within a macpan2 project.",
    "\n------"
  )
}

dev_obj = function() {
  tools::file_path_sans_ext(dev_file())
}

dev_choose_cpp = function() {
  if (interactive()) return("macpan2")
  dev_compile()
  return("dev")
}

dev_compile = function() {
  TMB::compile(dev_file())
  dyn.load(TMB::dynlib(dev_obj()))
}
