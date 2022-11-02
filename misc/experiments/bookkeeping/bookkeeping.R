library(macpan2)
library(dplyr)
library(tidyr)
not_all_empty <- function(x) any(!(is.na(x) | (nchar(x) == 0L)))

D_state = read_example('twostrain', 'state')
D_params = read_example('twostrain', 'param')
D_vars = bind_rows(D_state, D_params)
D_from = filter(D_state, state == "S")
D_to = filter(D_state, state == "E")

View(D_params)

make_index_set = function(data, var_label, var_value, index_labels) {
  data = data[data[[var_label]] == var_value, c(index_labels, "default_value")]
  index_set = (data[, index_labels]
    %>% lapply(unique)
    %>% lapply(keep_not, is_el_blank)
    %>% do.call(what = expand.grid)
  )
  index_set$default_value = 0
  for(i in seq_len(nrow(data))) {
    labels_i = unlist(data[i, index_labels])
    value_i = data[i, "default_value", drop = TRUE]
    if (any(is_el_blank(labels_i))) {

    }
  }
}

vec_from_index_set = function(data, index_set, )

VE_index_set = make_index_set(D_params, "param", "VE", c("vax_status", "R_history"))



matrix_from_struc = function()


D_edge = inner_join(
  D_from,
  D_to,
  by = c('recov_a', 'recov_b', 'vax'),
  suffix = c("_from", "_to")
) %>% select(where(not_all_empty))
D_full = full_join(
  D_edge,
  D_vars,
  by = c("strain_to" = "strain", "recov_a", "recov_b", "vax"),
  suffix = c("_edge", "_vars")
)

