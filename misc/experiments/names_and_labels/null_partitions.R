options(error = NULL)

library(macpan2)

library(ggplot2)
library(dplyr)
library(tidyr)
library(testthat)

sir = Compartmental(system.file("starter_models", "sir", package = "macpan2"))
sir_vax = Compartmental(system.file("starter_models", "sir_vax", package = "macpan2"))
v = sir_vax$variables$all()
v$filter("vax", .wrt = "Vax")
sir$expr_list()
sir$variables$infected_state()
sir$variables$all()
sir$variables$state()
sir$variables$infection_flow()

m$variables$infectious_state()
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
m$expr_list()$print_exprs()

(s$report()
  |> separate_wider_delim(cols = row, names = c("Epi", "Vax"), delim = ".")
  |> mutate(Epi = factor(Epi, levels = c("S", "I", "R")))
  |> ggplot()
  + facet_grid(Epi ~ Vax, scales = 'free')
  + geom_line(aes(time, value))
)
