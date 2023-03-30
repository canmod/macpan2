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
ue1 = UserExpr(m1)
ese1 = ue1$expand_scalar_expressions()
eve1 = ue1$expand_vector_expressions()
se1 = StandardExpr(m1)
sse1 = se1$standard_expressions()



expression_phase_sorter(eve1, sse1, "before")

phases = vapply(sse1, getElement, character(1L), "simulation_phase")


ExprList(
  before = create_expr_list_phase(eve1, sse1, "before"),
  during = c(
    create_expr_list_phase(eve1, sse1, "during_pre_update"),
    create_expr_list_phase(eve1, sse1, "during_update"),
    create_expr_list_phase(eve1, sse1, "during_post_update")),
  after = create_expr_list_phase(eve1, sse1, "after")
)
unlist(lapply(eve1, getElement, "arguments"), recursive = TRUE)
unlist(lapply(sse1, getElement, "arguments"), recursive = TRUE)

m2 = Model(models$seir)
ue2 = UserExpr(m2)
ue2$expand_scalar_expressions()
ue2$expand_vector_expressions()
se2 = StandardExpr(m2)
se2$standard_expressions()

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
ese6 = ue6$expand_scalar_expressions()
eve6 = ue6$expand_vector_expressions()
se6 = StandardExpr(m6)
sse6 = se6$standard_expressions()
