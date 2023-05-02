library(macpan2)

CompartmentalAlt = function(...) {
  Files(
    file.path(...),
    reader_spec("derivations.json", JSONReader),
    reader_spec("flows.csv", CSVReader),
    reader_spec("settings.json", JSONReader),
    reader_spec("variables.csv", CSVReader),
    reader_spec("transmission.csv", CSVReader)
  )
}
si_alt = CompartmentalAlt("inst", "starter_models", "SI_products", "hello_si")
prod_alt = CompartmentalAlt("inst", "starter_models", "SI_products", "hello_products")
tr = prod_alt$get("transmission")
macpan2:::StringDottedVector(tr[tr$matrix == "beta", ]$infection_flow)
macpan2:::StringDottedVector(tr[tr$matrix == "beta", ]$infection_flow)
si_alt$get("transmission")

age = Compartmental("inst/starter_models/SI_products/hello_age/")
si = Compartmental("inst/starter_models/SI_products/hello_si/")
prod = Compartmental("inst/starter_models/SI_products/hello_products/")



library(oor)

prod$def$get("transmission_matrices")


prod$variables$all()
prod$variables$state()
prod$variables$flow()
prod$variables$infected_state()
prod$variables$infectious_state()
prod$variables$infection_flow()
prod$variables$other()
prod$labels$all()
prod$labels$state()
prod$labels$flow()
age$labels$infected_state()
prod$labels$infectious_state()
prod$labels$infection_flow()
prod$labels$other()

si$state_variables()$products$cartesian(age$state_variables())
union_vars(
  cartesian(si$state_variables(), age$state_variables()),
  cartesian(si$flow_variables(), age$state_variables()),
  cartesian(si$state_variables(), age$flow_variables())
)

y = age$variables()$frame()
x = si$variables()$frame()

merge(x, y, by = union(names(x), names(y)))

x$.partition$expand("Epi.Age")$frame()

rbind2(x, y)

age$variables()
si$variables()$products$cartesian(age$variables())

si$state_variables()

age$flows()
si$flows()



sims = simple_sims(
  iteration_exprs = list(
    j ~ time_group(j, change_points),
    hh ~ time_variation_schedule[j]
  ),
  time_steps = 10,
  j = 0,
  change_points = c(0, 4, 7),
  time_variation_schedule = c(42, pi, sqrt(2)),
  time_varying_parameter = empty_matrix
)
sims[sims$matrix == "time_varying_parameter",]
