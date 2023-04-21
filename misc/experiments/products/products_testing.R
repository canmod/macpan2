library(oor)
library(macpan2)
library(TMB)

model_path = "../../inst/starter_models/SI_products/"
si_dir = paste0(model_path, "hello_si")
age_dir = paste0(model_path, "hello_age")
prod_dir = paste0(model_path, "hello_products")



si_model = Compartmental(si_dir)
age_model = Compartmental(age_dir)
prod_model = Compartmental(prod_dir)

si_vars = si_model$variables$all()
age_vars = age_model$variables$all()

class(si_vars$names())

variables_product(si_model, age_model)

req_par_si = si_model$def$settings()$required_partitions
req_par = prod_model$def$settings()$required_partitions
paste0(req_par, collapse = ".")
paste0(req_par_si, collapse = ".")

flows_product(si_model, age_model)
