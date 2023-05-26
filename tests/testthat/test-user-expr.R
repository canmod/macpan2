library(macpan2)
library(testthat)
f = system.file("testing_models", "all_derivations_fields", package = "macpan2")
m = Compartmental(f)

s = m$simulators$tmb(time_steps = 10
  , state = macpan2:::constant_named_vector(1, m$labels$state())
  , flow = macpan2:::constant_named_vector(0.5, m$labels$flow())
  , a.n = empty_matrix
  , b.n = empty_matrix
  , c.n = empty_matrix
  , d.n = empty_matrix
)
s$report()
