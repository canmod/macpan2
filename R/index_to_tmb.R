#' TMB Simulator from Dynamic Model
#' 
#' This is an 'old' function that was tested out at a workshop.
#' Currently it still drives the engine-agnostic-grammar vignette,
#' but we plan to replace this function with \code{\link{mp_simulator}}.
#'
#' @param dynamic_model Object product by \code{\link{mp_dynamic_model}}.
#' @param time_steps Number of time steps to simulate.
#' @param vectors Named list of named vectors as initial values for the
#' simulations that are referenced in the expression list in the dynamic model.
#' @param unstruc_mats = Named list of objects that can be coerced to
#' numerical matrices that are used in the expression list of the
#' dynamic model.
#' @param mats_to_save TODO
#' @param mats_to_return TODO
#' @param params TODO
#' @param random TODO
#' @param obj_fn TODO
#' @param log_file TODO
#' @param do_pred_sdreport TODO
#' @param tmb_cpp TODO
#' @param initialize_ad_fun TODO
#' @param ... TODO
#'
#' @importFrom oor method_apply
#' @export
mp_dynamic_simulator = function(dynamic_model
      , time_steps = 0L
      , vectors = NULL
      , unstruc_mats = NULL
      , mats_to_save = NULL
      , mats_to_return = NULL
      , params = OptParamsList(0)
      , random = OptParamsList()
      , obj_fn = ObjectiveFunction(~0)
      , log_file = LogFile()
      , do_pred_sdreport = TRUE
      , tmb_cpp = "macpan2"
      , initialize_ad_fun = TRUE
      , ...) {
  UseMethod("mp_dynamic_simulator")
}


#' @export
mp_dynamic_simulator.DynamicModel = function(dynamic_model
      , time_steps = 0L
      , vectors = NULL
      , unstruc_mats = NULL
      , mats_to_save = NULL
      , mats_to_return = NULL
      , params = OptParamsList(0)
      , random = OptParamsList()
      , obj_fn = ObjectiveFunction(~0)
      , log_file = LogFile()
      , do_pred_sdreport = TRUE
      , tmb_cpp = "macpan2"
      , initialize_ad_fun = TRUE
      , ...
) {
  ledgers = dynamic_model$ledgers
  expr_list = dynamic_model$expr_list
  int_vecs = (ledgers
   |> method_apply("positions_frame", zero_based = TRUE)
   |> lapply(as.list)
   |> unname()
   |> unlist(recursive = FALSE)
  )
  if (is.null(int_vecs)) int_vecs = list()
  if (is.null(vectors)) {
    indexed_mats = dynamic_model$init_vecs
    if (length(indexed_mats) != 0L) {
      mats_to_save = names(indexed_mats)
    }
  } else {
    for (v in names(vectors)) {
      vectors[[v]] = StructuredVector(
        vectors[[v]],
        dynamic_model$init_vecs[[v]]$index
      )
    }
    indexed_mats = lapply(vectors, as.matrix)
  }
  if (is.null(unstruc_mats)) {
    unstruc_mats = dynamic_model$unstruc_mats
  }

  all_vars = expr_list$all_formula_vars()

  derived_nms = setdiff(all_vars, c(
    names(int_vecs), names(indexed_mats), names(unstruc_mats)
  ))

  ## FIXME: default should be anything on the left-hand-side
  ## in a during expression
  supplied_nms = c(names(indexed_mats), names(unstruc_mats))

  if (is.null(mats_to_return)) mats_to_return = supplied_nms
  if (is.null(mats_to_save)) mats_to_save = mats_to_return

  derived_mats = (empty_matrix
    |> list()
    |> rep(length(derived_nms))
    |> setNames(derived_nms)
  )

  mats = c(indexed_mats, unstruc_mats, derived_mats)
  mats_list_options = list(
    .mats_to_save = mats_to_save,
    .mats_to_return = mats_to_return,
    .structure_labels = dynamic_model$labels
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
  TMBDynamicSimulator(
    tmb_model$simulator(tmb_cpp = tmb_cpp, initialize_ad_fun = initialize_ad_fun),
    dynamic_model
  )
}

#' @export
mp_dynamic_simulator.ModelDefRun = function(dynamic_model
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
  do.call(mp_dynamic_simulator, args)
}

# reporting function -- probably will delete soon in favour
# of mp_trajectory
# @param parameter_vector Numeric vector equal in length to (?)
# @param simulator Object produced by \code{\link{tmb_simulator}}.

# mp_report = function(simulator, parameter_vector = numeric(), phases = "during") {
#   if (length(parameter_vector) == 0L) parameter_vector = 0
#   r = do.call(
#     simulator$report,
#     c(
#       as.list(parameter_vector),
#       list(.phases = phases)
#     )
#   )
#   rownames(r) = NULL
#   r
# }
