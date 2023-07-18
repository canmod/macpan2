#' Compartmental Model
#'
#' Create an object for containing a compartmental model.
#'
#' @param model_directory String giving a path to a directory containing
#' the following files, `variables.csv`, `derivations.json`, `flows.csv`,
#' and `settings.json`, described by
#' [this spec](https://canmod.github.io/macpan2/articles/model_definitions).
#'
#' @return An object with the following methods and fields.
#'
#' ## Methods
#'
#' * `$variables$all()`: \code{\link{Partition}} object of all variables.
#' * `$flows()`: Data frame with rows giving all groups of flows.
#' * `$flows_expanded()`: Data frame with rows giving all individual flows.
#' * `$variables$state()`: \code{\link{Partition}} object of all state
#' variables.
#' * `$variables$flow()`: \code{\link{Partition}} object of all flow variables.
#' * `$all_labels()`: Character vector giving the labels of all variables.
#' * `$labels$state()`: Character vector giving the labels of all state
#' variables.
#' * `$labels$flow()`: Character vector giving the labels of all flow variables.
#' * `$labels$other()`: Character vector giving the labels of all variables
#' that are neither state nor flow variables.
#' * `$expr_list()`: \code{\link{ExprList}} object containing all expressions.
#'
#' ## Fields
#'
#' * `$def`: The \code{\link{ModelFiles}} object representing the directory
#' that defines the model.
#' * `$simulators`: A \code{\link{Simulators}} instance containing methods for
#' generating simulators, which are objects that can generate simulations from
#' the model.
#'
#' @export
Compartmental = function(model_directory) {
  self = Model(ModelFiles(model_directory))
  return_object(self, "Compartmental")
}

#' Compartmental Model Simulator (experimental)
#'
#' @param model_directory Path to a set of model definition files, defaults
#' and parameterizations.
#' @param time_steps Number of simulation steps.
#' @param .mats_to_save Character vector of names of matrices to save. Defaults
#' to \code{"state"}, which is the state vector. Other useful names include
#' \code{"total_inflow"} (the incidence associated with each state variable)
#' and \code{"total_outflow"} (the total leaving each state variable at each
#' time step). One may also add any variable in `model$other_labels()`.
#' See \code{\link{MatsList}} for details.
#' @param .mats_to_return Character vector of names of matrices to return. Defaults
#' to \code{"state"}, which is the state vector. Other useful names include
#' \code{"total_inflow"} (the incidence associated with each state variable)
#' and \code{"total_outflow"} (the total leaving each state variable at each
#' time step). One may also add any variable in `model$other_labels()`.
#' See \code{\link{MatsList}} for details.
#' @param .dimnames Named list of \code{\link{dimnames}} for matrices that change
#' their dimensions over the simulation steps. These names correspond to the
#' names of the matrices. The output of the simulations will try their best
#' to honor these names, but if the shape of the matrix is too inconsistent
#' with the \code{\link{dimnames}} then numerical indices will be used instead.
#' For matrices that do not change their dimensions, set \code{\link{dimnames}}
#' by adding \code{\link{dimnames}} to the matrices passed to \code{...}.
#' @param .tmb_cpp Name of a \code{C++} program defining the engine. Typically
#' you just want to use the default, which is \code{macpan2}, unless you
#' are extending the
#' [engine](https://canmod.github.io/macpan2/articles/cpp_side.html)
#' yourself.
#' @param .initialize_ad_fun Should the automatic differentiation function (`ad_fun`)
#' be produced with TMB? Choosing \code{FALSE} can be useful if this is
#' expensive and you want to build up your simulator in steps without having
#' to recreate the `ad_fun` more than necessary.
#' @export
CompartmentalSimulator = function(model_directory, time_steps
    , .mats_to_save = .mats_to_return
    , .mats_to_return = "state"
    , .dimnames = list()
    , .tmb_cpp = "macpan2"
    , .initialize_ad_fun = TRUE
  ) {
    compartmental = Compartmental(model_directory)
    defaults = Defaults(DefaultFiles(model_directory))
    parameters = OptParamsFile(file.path(model_directory, "parameters.csv"))
    all_labels = compartmental$variables$all()$labels()
    all_init_labels = defaults$initialized_variables()$labels()
    mats_init_as_empty = setdiff(all_labels, all_init_labels)
    dot_mats = setdiff(defaults$matrix_names(), c("state", "flow"))
    args = list(
      time_steps = time_steps,
      state = defaults$initialized_matrix("state"),
      flow = defaults$initialized_matrix("flow")
    )
    for (m in dot_mats) args[[m]] = defaults$initialized_matrix(m)
    for (m in mats_init_as_empty) args[[m]] = empty_matrix
    args$.mats_to_save = .mats_to_save
    args$.mats_to_return = .mats_to_return
    args$.dimnames = .dimnames
    args$.tmb_cpp = .tmb_cpp
    args$.initialize_ad_fun = FALSE
    simulator = do.call(compartmental$simulators$tmb, args)
    dn = simulator$tmb_model$init_mats$.dimnames
    simulator$replace$params_frame(parameters$params_frame(dn))
    simulator$replace$random_frame(parameters$random_frame(dn))
    if (.initialize_ad_fun) simulator$ad_fun()
    simulator
}

CompartmentalMatsList = function(
      model
    , state
    , flow
    , ...
    , .mats_to_save = character(0L)
    , .mats_to_return = character(0L)
    , .dimnames = list()
    , .structure_labels = NullLabels()
  ) {

    dot_names = names(list(...))

    check_auto_component_names(dot_names, "state", model$labels$state())
    check_auto_component_names(dot_names, "flow", model$labels$flow())
    check_auto_names(dot_names, auto_gen_names)

    settings = model$settings
    indices = model$indices

    MatsList(
      state = as.matrix(state)[settings$state(), , drop = FALSE]
    , flow = as.matrix(flow)[settings$flow(), , drop = FALSE]
    , ...
    , state_length = length(state)
    , per_capita_from = indices$flow$per_capita$from()
    , per_capita_to = indices$flow$per_capita$to()
    , per_capita_flow = indices$flow$per_capita$flow()
    , absolute_from = indices$flow$absolute$from()
    , absolute_to = indices$flow$absolute$to()
    , absolute_flow = indices$flow$absolute$flow()
    , per_capita_inflow_from = indices$flow$per_capita_inflow$from()
    , per_capita_inflow_to = indices$flow$per_capita_inflow$to()
    , per_capita_inflow_flow = indices$flow$per_capita_inflow$flow()
    , per_capita_outflow_from = indices$flow$per_capita_outflow$from()
    #, per_capita_outflow_to = indices$flow$per_capita_outflow$to()
    , per_capita_outflow_flow = indices$flow$per_capita_outflow$flow()
    #, absolute_inflow_from = indices$flow$absolute_inflow$from()
    , absolute_inflow_to = indices$flow$absolute_inflow$to()
    , absolute_inflow_flow = indices$flow$absolute_inflow$flow()
    , absolute_outflow_from = indices$flow$absolute_outflow$from()
    #, absolute_outflow_to = indices$flow$absolute_outflow$to()
    , absolute_outflow_flow = indices$flow$absolute_outflow$flow()
    , per_capita = empty_matrix
    , absolute = empty_matrix
    , per_capita_inflow = empty_matrix
    , per_capita_outflow = empty_matrix
    , absolute_inflow = empty_matrix
    , absolute_outflow = empty_matrix
    , total_inflow = empty_matrix
    , total_outflow = empty_matrix
    , dummy = empty_matrix
    , .mats_to_save = .mats_to_save
    , .mats_to_return = .mats_to_return
    , .dimnames = .dimnames
    , .structure_labels = .structure_labels
  )
}

auto_gen_names = c(
    "state"
  , "flow"
  , "state_length"
  , "per_capita_from"
  , "per_capita_to"
  , "per_capita_flow"
  , "absolute_from"
  , "absolute_to"
  , "absolute_flow"
  , "per_capita_inflow_from"
  , "per_capita_inflow_to"
  , "per_capita_inflow_flow"
  , "per_capita_outflow_from"
  , "per_capita_outflow_flow"
  , "absolute_inflow_to"
  , "absolute_inflow_flow"
  , "absolute_outflow_from"
  , "absolute_outflow_flow"
  , "per_capita"
  , "absolute"
  , "per_capita_inflow"
  , "per_capita_outflow"
  , "absolute_inflow"
  , "absolute_outflow"
  , "total_inflow"
  , "total_outflow"
  , "dummy"
)
