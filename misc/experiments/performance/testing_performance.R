source("R/dev_tools.R")
change_points = 0:1000
x_vals = rnorm(length(change_points))
library(macpan2)

dev_compile(suffix="",ext="cpp")
options(macpan2_dll = "dev", macpan2_verbose = FALSE) 
#engine_eval( ~ time_var(x_vals, change_points),x_vals=x_vals, change_points = change_points)
start_time = Sys.time()
t1 = simple_sims(
  iteration_exprs = list(x ~ time_var(x_vals, change_points)
                         , y ~ rbind_lag(x,0))
  , time_steps = 1001
  , mats= list(x = empty_matrix, y = empty_matrix, x_vals = x_vals)
  , int_vecs = list(change_points = change_points)
)
end_time = Sys.time()
orig_rtime = end_time-start_time

dev_compile(suffix="_ptest",ext="cpp")
options(macpan2_dll = "dev_ptest", macpan2_verbose = FALSE) 
start_time = Sys.time()
t2 = simple_sims(
  iteration_exprs = list(x ~ time_var(x_vals, change_points)
                         , y ~ rbind_lag(x,0))
  , time_steps = 1001
  , mats= list(x = empty_matrix, y = empty_matrix, x_vals = x_vals)
  , int_vecs = list(change_points = change_points)
)
end_time = Sys.time()
new_rtime = end_time-start_time


