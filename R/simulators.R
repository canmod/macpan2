#' Simulators
#'
#' Adapt a \code{\link{Compartmental}} model to a simulation engine. Currently
#' TMB is the only simulation engine.
#'
#' @param model A \code{\link{Compartmental}} model.
#'
#' @return An object with the following methods.
#'
#' ### Methods
#'
#' ```
#' $tmb(time_steps
#'    , state, rate
#'    , ...
#'    , .mats_to_save = .mats_to_return
#'    , .mats_to_return = "state"
#'    , .dimnames = list()
#' )
#' ```
#'
#' ### Method Arguments
#'
#' * `time_steps`: The number of time steps run in each simulation.
#' * `state`: Named numerical vector giving the default initial value of the
#' state vector. The names must be equal to `model$state_labels()`.
#' * `flow`: Named numerical vector giving the default initial value of the
#' flow vector. The names must be equal to `model$flow_labels()`.
#' * `...`: Named numerical objects that can be coerced to a matrix, giving
#' the matrices required by the model.
#' * `.mats_to_save`: Character vector of names of matrices to save. Defaults
#' to \code{"state"}, which is the state vector. Other useful names include
#' \code{"total_inflow"} (the incidence associated with each state variable)
#' and \code{"total_outflow"} (the total leaving each state variable at each
#' time step). One may also add any variable in `model$other_labels()`.
#' See \code{\link{MatsList}} for details.
#' * `.mats_to_return`: Character vector of names of matrices to return. Defaults
#' to \code{"state"}, which is the state vector. Other useful names include
#' \code{"total_inflow"} (the incidence associated with each state variable)
#' and \code{"total_outflow"} (the total leaving each state variable at each
#' time step). One may also add any variable in `model$other_labels()`.
#' See \code{\link{MatsList}} for details.
#' * `.dimnames`: Advanced and often not necessary.  See
#' \code{\link{MatsList}} for details.
#'
#' @export
Simulators = function(model) {
  self = Base()
  self$model = model
  self$tmb = function(time_steps
        , state
        , flow
        , ...
        , .mats_to_save = .mats_to_return
        , .mats_to_return = "state"
        , .dimnames = list()
    ) {
      TMBModel(
        init_mats = CompartmentalMatsList(state, flow
          , ...
          , .mats_to_save = .mats_to_save
          , .mats_to_return = .mats_to_return
          , .dimnames = .dimnames),
        expr_list = self$model$expr_list(),
        time_steps = Time(time_steps)
      )$simulator()
  }
  return_object(self, "Simulators")
}
