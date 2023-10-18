library(macpan2)
alt_cpp = FALSE
use_dev = FALSE
if (use_dev) {
  if (alt_cpp) {
    #TMB::compile("misc/dev/dev_pre_alloc_hist.cpp")
    dyn.load(TMB::dynlib("misc/dev/dev_pre_alloc_hist"))
  } else {
    #TMB::compile("misc/dev/dev.cpp")
    dyn.load(TMB::dynlib("misc/dev/dev"))
  }
}
#dyn.load("/usr/local/lib/libprofiler.0.dylib")
# export R_HOME=/usr/local/Cellar/r/4.2.3/lib/R
# CPUPROFILE="prealloc.log" /usr/local/Cellar/r/4.2.3/lib/R/bin/exec/R -f misc/dev/dev-pre-alloc-hist.R
# pprof --dot /usr/local/Cellar/r/4.2.3/lib/R/bin/exec/R prealloc.log > prealloc.dot
# dot -Tpng prealloc.dot -o prealloc.png

sir = Compartmental(system.file("starter_models", "sir", package = "macpan2"))
N = 100
time_steps = 1000 ## Increase this number to cause allocation problems

print("-----------")
cat("\n")
print("simulator constructor")
left = Sys.time()
simulator = sir$simulators$tmb(time_steps = time_steps
  , state = c(S = N - 1, I = 1, R = 0)
  , flow = c(foi = 0, gamma = 0.2)
  , N = N
  , beta = 0.4
  , .mats_to_return = c("state", "total_inflow")
  #, .mats_to_return = character()
  #, .initialize_ad_fun = FALSE
)
simulator$replace$obj_fn(~sum((state / N)^2))
simulator$replace$params(default = 0.41, mat = "beta")
right = Sys.time()
print(right - left)

simulator$report()

if (use_dev) {
  if (alt_cpp) {
    aa = simulator$tmb_model$make_ad_fun_arg(tmb_cpp = "dev_pre_alloc_hist")
    mm = rep(list(empty_matrix), length(aa$data$mats))
    mm[[1L]] = aa$data$mats[[1L]]
    aa$data$mats_shape = mm
  } else {
    aa = simulator$tmb_model$make_ad_fun_arg(tmb_cpp = "dev")
  }
} else {
  aa = simulator$tmb_model$make_ad_fun_arg(tmb_cpp = "macpan2")
}

cat("\n")
print("-----------")
print("make ad fun")
left = Sys.time()
ff = do.call(TMB::MakeADFun, aa)
right = Sys.time()
print(right - left)

# cat("\n")
# print("-----------")
# print("simulations")
# left = Sys.time()
# rr = ff$report()
# print(dim(rr$value))
# right = Sys.time()
# print(right - left)
#
# cat("\n")
# print("-----------")
# print("objective function")
# left = Sys.time()
# ff$fn()
# right = Sys.time()
# print(right - left)
#
# cat("\n")
# print("-----------")
# print("gradient")
# left = Sys.time()
# ff$gr()
# right = Sys.time()
# print(right - left)
#
# cat("\n")
# print("-----------")
# print("hessian")
# left = Sys.time()
# ff$he()
# right = Sys.time()
# print(right - left)

cat("\n")
print("-----------")
print("simulations new pars")
left = Sys.time()
rr = ff$report(0.45)
print(dim(rr$value))
right = Sys.time()
print(right - left)

# cat("\n")
# print("-----------")
# print("objective function new pars")
# left = Sys.time()
# ff$fn(0.5)
# right = Sys.time()
# print(right - left)
#
# cat("\n")
# print("-----------")
# print("gradient new pars")
# left = Sys.time()
# ff$gr(0.2)
# right = Sys.time()
# print(right - left)
#
# cat("\n")
# print("-----------")
# print("hessian new pars")
# left = Sys.time()
# ff$he(0.33)
# right = Sys.time()
# print(right - left)
