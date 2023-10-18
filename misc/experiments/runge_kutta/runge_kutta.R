library(macpan2)
sir_vax = Compartmental(system.file("starter_models", "sir_vax", package = "macpan2"))
sir_vax_sim = sir_vax$simulators$tmb(time_steps = 100L
  , state = c(S.unvax = 99, I.unvax = 1, R.unvax = 0, S.vax = 0, I.vax = 0, R.vax = 0)
  , flow = c(
        infection.unvax = 0, infection.vax = 0
      , gamma.unvax = 0.1, gamma.vax = 0.1
      , .vax_rate = 0.1
    )
  , sigma.unvax = 1
  , sigma.vax = 0.01
  , beta.unvax = 0.2
  , beta.vax = 0.2
  , foi.unvax = empty_matrix
  , foi.vax = empty_matrix
  , foi. = empty_matrix
  , N.unvax = empty_matrix
  , N.vax = empty_matrix
)


sir = Compartmental(system.file("starter_models", "sir", package = "macpan2"))


simulator = sir$simulators$tmb(time_steps = 50
  , state = c(S = 99, I = 1, R = 0)
  , flow = c(foi = 0, gamma = 0.2)
  , N = empty_matrix
  , beta = 0.8
)


dd = DerivationExtractor(sir_vax)
g = dd$extract_derivations()[[2]]
length(g$arguments)

g$outputs

