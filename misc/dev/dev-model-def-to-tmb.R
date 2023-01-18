library(macpan2)
library(TMB)

model_repo = "../../inst/starter_models"
model = function(model_name) {
  model_path = file.path(model_repo, model_name)
  ModelFiles(model_path)
}
model_paths = list.files(model_repo, full.names = TRUE)
models = setNames(
  lapply(model_paths, ModelFiles),
  tools::file_path_sans_ext(basename(model_paths))
)
m = Model(models$seir_symp_vax)

m$flow_variables()
m$state_variables()
m$variables()
m$flows()
m$flows_expanded()
m$derivations()

v = m$variables()

v$labels()
v$names()
v$name()
v$dotted()

v$select("Vax", "Epi")$labels()
v$filter("I.unvax", "S.vax"
  , .wrt = "Epi.Vax"
  , .comparison_function = all_not_equal
)

## see macpan2:::make_expression for the current work on converting deriations
## to expressions
