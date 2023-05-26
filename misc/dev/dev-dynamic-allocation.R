library(macpan2)

## choose c++ code to use:
##   1. "macpan2" to use the c++ in the installed package
##   2. "dev" to use the file for development in misc/dev/dev.cpp
tmb_cpp = "macpan2"

## if tmb_cpp = "dev", then you will need to compile it with this command
macpan2:::dev_compile()

## for profiling on SW's setup
#dyn.load("/usr/local/lib/libprofiler.0.dylib")

sir = Compartmental(system.file("starter_models", "sir", package = "macpan2"))
N = 100
time_steps = 1000000 ## Increase this number to cause allocation problems

# --------
simulator = sir$simulators$tmb(time_steps = time_steps
  , state = c(S = N - 1, I = 1, R = 0)
  , flow = c(foi = 0, gamma = 0.2)
  , N = N
  , beta = 0.4
  , .mats_to_save = c("state", "flow", "N", "beta")
  , .tmb_cpp = tmb_cpp
)

simulator$print$matrix_dims()
simulator$print$expressions()
sims = simulator$report(.phases = "during")
head(sims)
