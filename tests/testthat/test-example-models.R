library(macpan2)
f = system.file("starter_models", "sir_vax", package = "macpan2")
#f = "inst/starter_models/sir_vax"
m = Compartmental(f)
m$indices$flow$per_capita$from()
m$indices$flow$per_capita$to()
m$indices$flow$per_capita$flow()
m$indices$transmission$infection_flow()

s = m$simulators$tmb(
  time_steps = 100,
  state = c(
    S.unvax = 99,
    I.unvax = 1,
    R.unvax = 0,
    S.vax = 0,
    I.vax = 0,
    R.vax = 0
  ),
  flow = c(
    infection.unvax = 0,
    infection.vax = 0,
    gamma.unvax = 0.1,
    gamma.vax = 0.3,
    .vax_rate = 0.1
  ),
  beta.unvax = 0.2,
  beta.vax = 0.01,
  sigma.unvax = 1,
  sigma.vax = 0.2,
  N.unvax = empty_matrix,
  N.vax = empty_matrix,
  foi.unvax = empty_matrix,
  foi.vax = empty_matrix,
  foi. = empty_matrix
)

m$flows_expanded()
m$flows()
m$expr_list()$print_exprs()
s$report()
