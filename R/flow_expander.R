#' Flow Expander
#'
#' @param model Object constructed with \code{\link{ModelFiles}} or
#' \code{\link{ModelCollection}}.
#'
#' @export
FlowExpander = function(model) {
  self = Base()
  model = model$freeze()

  self$.add_missing_columns = function(flows) {
    cols_on_file = names(flows)
    required_cols = c("from",	"to",	"flow",	"type")
    if (!all(required_cols %in% cols_on_file)) {
      stop(
        "Malformed flows.csv file.\n",
        "All of the following columns must exist:\n",
        required_cols
      )
    }
    rp = macpan2:::StringUndottedVector(self$s$required_partitions)$dot()$value()
    filtering_cols = c("from_partition",	"to_partition",	"flow_partition")
    for (col in filtering_cols) {
      if (!col %in% cols_on_file) {
        flows[[col]] = rp
      }
    }
    mp = c("", "", self$s$null_partition)
    matching_cols = c("from_to_partition",	"from_flow_partition",	"to_flow_partition")
    for (col in matching_cols) {
      if (!col %in% cols_on_file) {
        flows[[col]] = mp[col == matching_cols]
      }
    }
    flows
  }

  self$v = model$variables()
  self$d = model$derivations()
  self$s = model$settings()
  self$f = self$.add_missing_columns(model$flows())
  self$.namer = Namer()
  self$.filter = FilterBlanksNotSpecial()

  ## only rate makes sense now, but adding others to prepare for them
  valid_flow_types = sprintf("%s_variables", c("abs", "rate", "abs_source", "abs_sink", "rate_source", "rate_source"))
  flow_settings = self$s[valid_flow_types]
  ## TODO: add check to enforce that no variable is in more than one type??
  ## TODO: make sure that flow_settings is a valid character vector even
  ## though many of these types will not be present in any given model
  self$.flow_variables = do.call(c, flow_settings)



  self$filter_flow = function(flow_number, component_type) {
    filter_partitions = paste(component_type, "partition", sep = "_")
    partition_label = self$f[flow_number, component_type]
    partition_set_name = self$f[flow_number, filter_partitions]
    partition_set = self$.filter$filter(self$v, partition_set_name, partition_label)
    component_filter = switch(component_type
      , from = self$s$state_variables
      , to = self$s$state_variables
      , flow = self$s$flow_variables
    )
    #namer$v_to_psl_with_pn(partition_set, s$required_partitions)
    partition_set[
      self$.namer$v_to_psl_with_pn(partition_set, self$s$required_partitions) %in%
        component_filter
      ,, drop = FALSE
    ]
  }

  self$matching_flow = function(flow_number, component_types) {
    filter_one = self$filter_flow(flow_number, component_types[1])
    filter_two = self$filter_flow(flow_number, component_types[2])
    matching_partitions = paste(component_types[1], component_types[2], "partition", sep = "_")
    partition_set_name = self$f[flow_number, matching_partitions]
    if (partition_set_name == self$s$null_partition) {
      z = setNames(
        list(character(), character()),
        component_types
      )
      if (any(component_types == "flow")) {
        z$type = character()
      }
      z = as.data.frame(z)
    } else {
      matching_partition_names = self$.namer$psn_to_pn(partition_set_name)
      x = filter_one[, matching_partition_names, drop = FALSE]
      y = filter_two[, matching_partition_names, drop = FALSE]
      x[[component_types[[1L]]]] = self$.namer$v_to_psl_with_pn(filter_one, self$s$required_partitions)
      y[[component_types[[2L]]]] = self$.namer$v_to_psl_with_pn(filter_two, self$s$required_partitions)
      z = merge(x, y, by = matching_partition_names, sort = FALSE)[, component_types, drop = FALSE]
      if (any(component_types == "flow")) {
        z$type = self$f[flow_number, "type"]
      }
    }
    z
  }

  self$expand_flow = function(flow_number) {
    from_to = self$matching_flow(flow_number, c("from", "to"))
    from_flow = self$matching_flow(flow_number, c("from", "flow"))
    to_flow = self$matching_flow(flow_number, c("to", "flow"))
    col_order = c("from", "to", "flow", "type")
    if (nrow(from_flow) != 0L) {
      from_flow = merge(from_to, from_flow, by = "from", sort = FALSE)[col_order]
      if (nrow(to_flow) == 0L) return(from_flow)
    }
    if (nrow(to_flow) != 0L) {
      to_flow = merge(from_to, to_flow, by = "to", sort = FALSE)[col_order]
      if (nrow(from_flow) == 0L) return(to_flow)
    }
    unique(rbind(from_flow, to_flow))
  }

  self$expand_flows = function() {
    do.call(rbind, lapply(seq_len(nrow(self$f)), self$expand_flow))
  }

  return_object(self, "FlowExpander")
}
