library(macpan2)
library(TMB)

model_repo = "../../inst/starter_models"
model = function(model_name) {
  model_path = file.path(model_repo, model_name)
  ModelFiles(model_path)
}
model_paths = list.files(model_repo, full.names = TRUE)
models = setNames(
  lapply(model_paths[3:7], ModelFiles),
  tools::file_path_sans_ext(basename(model_paths[3:7]))
)
m = Model(models$seir_symp_vax)

d = m$derivations()

d1 = d[[1]]
d4 = d[[4]]

v = m$variables()
ue = m$user_expressions()
se = m$standard_expressions()
m_from = m$from_states()
m_to = m$to_states()
fe = m$flows_expanded()

grp_list_d4 = group_expressions(v, d4)
grp_list_d4


grp_list_d1 = group_expressions(v, d1)
grp_list_d1

grp_list_alt = group_expressions(v, d[[5]])
grp_list_alt

usr_exprs = user_expressions(v, d)
usr_exprs

m1 = Model(models$seir)
ue1 = m1$user_expressions()
m1_from = m1$from_states()
m1_to = m$to_states()
v1 = m1$variables()
v1$filter()