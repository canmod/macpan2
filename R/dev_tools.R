dev_choose_cpp = function() {
  if (interactive()) {
    cpp = "macpan2"
  } else {
    TMB::compile('dev.cpp')
    dyn.load(dynlib("dev"))
    cpp = "dev"
  }
  cpp
}
