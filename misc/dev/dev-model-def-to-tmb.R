library(oor)
library(macpan2)
library(TMB)

model_repo = "inst/starter_models"
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
d1 = Derivations2ExprList(UserExpr(m1), StandardExpr(m1))
e1 = d1$expr_list()
e1$print_exprs()

m2 = Model(models$seir)
d2 = Derivations2ExprList(UserExpr(m2), StandardExpr(m2))
e2 = d2$expr_list()

m3 = Model(models$age)
d3 = Derivations2ExprList(UserExpr(m3), StandardExpr(m3))
e3 = d3$expr_list()

m4 = Model(models$testing)
d4 = Derivations2ExprList(UserExpr(m4), StandardExpr(m4))
e4 = d4$expr_list()

m5 = Model(models$vax)
d5 = Derivations2ExprList(UserExpr(m5), StandardExpr(m5))
e5 = d5$expr_list()

m6 = Model(models$macpan_base)
d6 = Derivations2ExprList(UserExpr(m6), StandardExpr(m6))
e6 = d6$expr_list()
e6$print_exprs()
