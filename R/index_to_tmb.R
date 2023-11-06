#' @importFrom oor method_apply
#' @export
mp_tmb_simulator = function(dynamic_model
      , time_steps = 0L
      , vectors = NULL
      , unstruc_mats = NULL
      , mats_to_save = names(vectors)
      , mats_to_return = mats_to_save
      , params = OptParamsList(0)
      , random = OptParamsList()
      , obj_fn = ObjectiveFunction(~0)
      , log_file = LogFile()
      , do_pred_sdreport = TRUE
      , tmb_cpp = "macpan2"
      , initialize_ad_fun = TRUE
      , ...
) {
  UseMethod("mp_tmb_simulator")
}

#' @export
mp_tmb_simulator.DynamicModel = function(dynamic_model
      , time_steps = 0L
      , vectors = NULL
      , unstruc_mats = NULL
      , mats_to_save = names(vectors)
      , mats_to_return = mats_to_save
      , params = OptParamsList(0)
      , random = OptParamsList()
      , obj_fn = ObjectiveFunction(~0)
      , log_file = LogFile()
      , do_pred_sdreport = TRUE
      , tmb_cpp = "macpan2"
      , initialize_ad_fun = TRUE
      , ...
) {
  link_data = dynamic_model$link_data
  expr_list = dynamic_model$expr_list
  int_vecs = (link_data
   |> method_apply("positions_frame", zero_based = TRUE)
   |> lapply(as.list)
   |> unname()
   |> unlist(recursive = FALSE)
  )
  if (is.null(vectors)) {
    indexed_mats = dynamic_model$init_vecs
    mats_to_save = names(indexed_mats)
  } else {
    indexed_mats = lapply(vectors, as.matrix)
  }
  if (is.null(unstruc_mats)) {
    unstruc_mats = dynamic_model$unstruc_mats
  }

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
    , params = params
    , random = random
    , obj_fn = obj_fn
    , log_file = log_file
    , do_pred_sdreport = do_pred_sdreport
  )
  tmb_model$simulator(tmb_cpp = tmb_cpp, initialize_ad_fun = initialize_ad_fun)
}

#' @export
mp_tmb_simulator.ModelDefRun = function(dynamic_model
      , time_steps = 0L
      , vectors = NULL
      , unstruc_mats = NULL
      , mats_to_save = names(vectors)
      , mats_to_return = mats_to_save
      , params = OptParamsList(0)
      , random = OptParamsList()
      , obj_fn = ObjectiveFunction(~0)
      , log_file = LogFile()
      , do_pred_sdreport = TRUE
      , tmb_cpp = "macpan2"
      , initialize_ad_fun = TRUE
      , ...
) {
  args = c(as.list(environment()), list(...))
  args$dynamic_model = dynamic_model$dynamic_model
  do.call(mp_tmb_simulator, args)
}

#' @export
mp_report = function(simulator, ...) simulator$report()
