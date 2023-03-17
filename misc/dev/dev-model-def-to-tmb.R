library(oor)
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

m1 = Model(models$seir_symp_vax)
ue1 = UserExpr(m1)
ee1 = ue1$expand_expressions()
sv1 = Scalar2Vector(m1)
v1 = sv1$vectorize()


m2 = Model(models$seir)
ue2 = UserExpr(m2)
ee2 = ue2$evaluate_expressions()

m3 = Model(models$age)
ue3 = UserExpr(m3)
ee3 = ue3$evaluate_expressions()

m4 = Model(models$testing)
ue4 = UserExpr(m4)
ee4 = ue4$evaluate_expressions()

m5 = Model(models$vax)
ue5 = UserExpr(m5)
ee5 = ue5$evaluate_expressions()

m6 = Model(models$macpan_base)
ue6 = UserExpr(m6)
ee6 = ue6$expand_expressions()
sv6 = Scalar2Vector(m6)
v6 = sv6$vectorize()
