library(macpan2)

## choose c++ code to use:
##   1. "macpan2" to use the c++ in the installed package
##   2. "dev" to use the file for development in misc/dev/dev.cpp
tmb_cpp = "dev"

## if tmb_cpp = "dev", then you will need to compile it with this command
#print(macpan2:::dev_compile(suffix = "", ext = "cpp"))
#TMB::compile("dev.cpp")
dyn.load(TMB::dynlib("misc/dev/dev"))

## for profiling on SW's setup
dyn.load("/usr/local/lib/libprofiler.0.dylib")
# export R_HOME=/usr/local/Cellar/r/4.2.3/lib/R
# CPUPROFILE="million.log" /usr/local/Cellar/r/4.2.3/lib/R/bin/exec/R -f misc/dev/dev-dynamic-allocation.R
# pprof --dot /usr/local/Cellar/r/4.2.3/lib/R/bin/exec/R million.log > million.dot
# dot -Tpng million.dot -o million.png

sir = Compartmental(system.file("starter_models", "sir", package = "macpan2"))
N = 100
time_steps = 1000 ## Increase this number to cause allocation problems

# --------
left_make = Sys.time()
simulator = sir$simulators$tmb(time_steps = time_steps
  , state = c(S = N - 1, I = 1, R = 0)
  , flow = c(foi = 0, gamma = 0.2)
  , N = N
  , beta = 0.4
  #, .mats_to_save = c("state", "flow", "N", "beta")
  , .mats_to_save = c("state", "total_inflow")
  , .mats_to_return = c("state", "total_inflow")
  , .tmb_cpp = tmb_cpp
  , .initialize_ad_fun = TRUE
)
right_make = Sys.time()

simulator$print$matrix_dims()
simulator$print$expressions()
left_ad = Sys.time()
ff = simulator$ad_fun()
right_ad = Sys.time()

left_run = Sys.time()
sims = simulator$report(.phases = "during")
right_run = Sys.time()
head(sims)

right_make - left_make
right_ad - left_ad
right_run - left_run
