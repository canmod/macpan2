topological_sort_engine = function(flows, state_nms, warn_not_dag = TRUE) {
  has_inflow = function(focal_state, to_states, state_nms) {
    stopifnot(focal_state %in% state_nms)
    focal_state %in% to_states
  }
  if (missing(state_nms)) state_nms = unique(c(flows$from, flows$to))
  state_order = c()
  n = length(state_nms)
  for (i in 1:n) {
    if (length(state_nms) == 0L) break
    remaining_inflow = sapply(
      state_nms,
      has_inflow,
      flows$to,
      state_nms
    )
    state_order = c(state_order, state_nms[!remaining_inflow])
    flows = flows[!flows$to %in% state_order, , drop = FALSE]
    flows = flows[!flows$from %in% state_order, , drop = FALSE]
    state_nms = state_nms[remaining_inflow]
    is_acyclic = (
        isTRUE(all(remaining_inflow))
      & length(state_nms) != 0L
      & length(remaining_inflow) != 0L
    )
    if (is_acyclic) {
      if (warn_not_dag) {
        warning(
         "\nState network is not acyclic (i think),",
         "\nand therefore cannot be topologically sorted.",
         "\nDefaulting to original order where necessary."
        )
      }
      state_order = c(state_order, state_nms) |> unique()
      state_nms = character(0L)
    }
  }
  state_order
}

topological_sort = function(spec, warn_not_dag = TRUE) {
  topological_sort_engine(
      mp_flows_frame(spec)
    , mp_state_vars(spec)
    , warn_not_dag = warn_not_dag
  )
}

#' Flow Frame (experimental)
#' 
#' Get a data frame representing the flows in a model specification.
#' 
#' @param spec A \code{\link{mp_tmb_model_spec}}.
#' @param topological_sort Should the states be topologically sorted to
#' respect the main direction of flow?
#' @param warn_not_dag Should a warning be thrown if the flow model is  
#' not a directed acyclic graph. This is only relevant if `topological_sort`
#' is used.
#' @returns A data frame that gives information provided in calls to
#' \code{\link{mp_per_capita_flow}} and \code{\link{mp_per_capita_inflow}}.
#' 
#' @export
mp_flow_frame = function(spec, topological_sort = TRUE, warn_not_dag = TRUE) {
  cf = spec$change_model$change_frame()
  ff = spec$change_model$flow_frame()
  sv = unique(cf$state)
  to = cf[startsWith(cf$change, "+"), , drop = FALSE]
  from = cf[startsWith(cf$change, "-"), , drop = FALSE]
  to$change = sub("^\\+", "", to$change) |> macpan2:::reset_rownames()
  from$change = sub("^\\-", "", from$change) |> macpan2:::reset_rownames()
  to_only = to[to$change %in% setdiff(to$change, from$change), , drop = FALSE]
  flows = merge(
      merge(from, to, by = "change", suffixes = c(".from", ".to"))
    , ff, by = "change"
  )[, c("state.from", "state.to", "change", "rate"), drop = FALSE]
  inflows = merge(to_only, ff, by = "change")[, c("size", "state", "change", "rate"), drop = FALSE]
  names(flows) = c("from", "to", "name", "rate")
  names(inflows) = c("from", "to", "name", "rate")
  if (nrow(flows) > 0L) flows$type = "flow"
  if (nrow(inflows) > 0L) inflows$type = "inflow"
  flows = rbind(flows, inflows)
  if (topological_sort) {
    topo = topological_sort_engine(flows, sv, warn_not_dag)
    flows = flows[order(factor(flows$from, levels = topo)), , drop = FALSE]
  }
  return(flows)
}
