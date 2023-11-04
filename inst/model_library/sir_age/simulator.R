## collect information into a simulator -----------------------

### pick vectors and matrices to be included in the
### simulation output.
to_return = c("state", "flows_per_time")

### here is the list of vectors and matrices in the
### model that could be included in the
### simulation output.
setdiff(
  expr_list$all_formula_vars(),
  lapply(index_data, getElement, "table_names") |> unlist()
)

sir = mp_tmb_simulator(
    expr_list
  , index_data
  , indexed_vecs
  , unstruc_mats
  , time_steps
  , mats_to_save = to_return
)
