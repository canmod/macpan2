#' @export
mp_tmb_simulator = function(expr_list = ExprList()
      , index_data = list()
      , indexed_vecs = list()
      , unstruc_mats = list()
      , time_steps = 0L
      , mats_to_save = names(indexed_vecs)
      , mats_to_return = mats_to_save
      , ...
) {
  int_vecs = (index_data
   |> method_apply("positions_frame", zero_based = TRUE)
   |> lapply(as.list)
   |> unname()
   |> unlist(recursive = FALSE)
  )
  indexed_mats = lapply(indexed_vecs, as.matrix)

  all_vars = expr_list$all_formula_vars()

  derived_nms = setdiff(all_vars, c(
    names(int_vecs), names(indexed_mats), names(unstruc_mats)
  ))

  derived_mats = (empty_matrix
    |> list()
    |> rep(length(derived_nms))
    |> setNames(derived_nms)
  )

  mats = c(indexed_mats, unstruc_mats, derived_mats)
  mats_list_options = list(
    .mats_to_save = mats_to_save,
    .mats_to_return = mats_to_return
  )
  engine_methods = EngineMethods(int_vecs = do.call(IntVecs, int_vecs))
  tmb_model = TMBModel(
      init_mats = do.call(MatsList, c(mats, mats_list_options))
    , expr_list = expr_list
    , engine_methods = engine_methods
    , time_steps = Time(time_steps)
    , ...
  )
  tmb_model$simulator()
}
