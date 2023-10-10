Connection = function(row, variables
    , from_ref = "from", to_ref = "to", conn_ref = "conn", type_ref = "type"
    , from_set = "all", to_set = "all", conn_set = "all"
    , filter_ref = "filter", join_ref = "join"
) {
  self = Base()
  self$row = row
  self$variables = variables
  self$from_ref = from_ref
  self$to_ref = to_ref
  self$from_set = from_set
  self$to_set = to_set
  self$conn_set = conn_set
  self$conn_ref = conn_ref
  self$filter_ref = filter_ref
  self$join_ref = join_ref
  self$type_ref = type_ref

  self$from_universe = variables[[self$from_set]]
  self$to_universe = variables[[self$to_set]]
  self$conn_universe = variables[[self$conn_set]]

  self$from = self$row[[self$from_ref]]
  self$to = self$row[[self$to_ref]]
  self$conn = self$row[[self$conn_ref]]

  self$from_filter = function() {
    x = self$row[[sprintf("%s_%s", self$from_ref, self$filter_ref)]]
    if (isTRUE(nchar(x) == 0L)) x = character()
    if (is.null(x)) x = self$req_part()
    x
  }
  self$to_filter = function() {
    x = self$row[[sprintf("%s_%s", self$to_ref, self$filter_ref)]]
    if (isTRUE(nchar(x) == 0L)) x = character()
    if (is.null(x)) x = self$req_part()
    x
  }
  self$conn_filter = function() {
    x = self$row[[sprintf("%s_%s", self$conn_ref, self$filter_ref)]]
    if (isTRUE(nchar(x) == 0L)) x = character()
    if (is.null(x)) x = self$req_part()
    x
  }

  self$from_to_join = function() {
    x = self$row[[sprintf("%s_%s_%s", self$from_ref, self$to_ref, self$join_ref)]]
    if (isTRUE(nchar(x) == 0L) | is.null(x)) x = character()
    x
  }
  self$from_conn_join = function() {
    x = self$row[[sprintf("%s_%s_%s", self$from_ref, self$conn_ref, self$join_ref)]]
    if (isTRUE(nchar(x) == 0L) | is.null(x)) x = character()
    x
  }
  self$to_conn_join = function() {
    x = self$row[[sprintf("%s_%s_%s", self$to_ref, self$conn_ref, self$join_ref)]]
    if (isTRUE(nchar(x) == 0L) | is.null(x)) x = character()
    x
  }

  self$null_part = function() self$variables$model$settings$null()
  self$req_part = function() self$variables$model$settings$names()

  self$from_vars = function() {
    labelled_frame(
      self$from_universe()$filter(self$from, .wrt = self$from_filter()),
      self$from_ref
    )
  }
  self$to_vars = function() {
    labelled_frame(
      self$to_universe()$filter(self$to, .wrt = self$to_filter()),
      self$to_ref
    )
  }
  self$conn_vars = function() {
    f = labelled_frame(
      self$conn_universe()$filter(self$conn, .wrt = self$conn_filter()),
      self$conn_ref
    )
    if (length(self$type_ref) == 1L) f$type = self$row[[self$type_ref]]
    f
  }
  self$from_to_merge = function() {
    output_cols = c(self$from_ref, self$to_ref)
    if (identical(self$from_to_join(), self$null_part())) return(empty_frame(output_cols))
    connection_merge(
      self$from_vars(),
      self$to_vars(),
      by = self$from_to_join(),
      output_cols = output_cols
    )
  }
  self$from_conn_merge = function() {
    output_cols = c(self$from_ref, self$conn_ref, self$type_ref)
    if (identical(self$from_conn_join(), self$null_part())) return(empty_frame(output_cols))
    connection_merge(
      self$from_vars(),
      self$conn_vars(),
      by = self$from_conn_join(),
      output_cols = output_cols
    )
  }
  self$to_conn_merge = function() {
    output_cols = c(self$to_ref, self$conn_ref, self$type_ref)
    if (identical(self$to_conn_join(), self$null_part())) return(empty_frame(output_cols))
    connection_merge(
      self$to_vars(),
      self$conn_vars(),
      by = self$to_conn_join(),
      output_cols = output_cols
    )
  }

  self$frame = function() {
    from_to = self$from_to_merge()
    from_conn = self$from_conn_merge()
    to_conn = self$to_conn_merge()
    out_cols = c(self$from_ref, self$to_ref, self$conn_ref, self$type_ref)
    col_order = c(self$from_ref, self$to_ref, self$conn_ref, self$type_ref)
    if (nrow(from_conn) != 0L) {
      if (nrow(from_to) == 0L) return(enforce_schema(from_conn, out_cols))
      from_conn = merge(from_to, from_conn, by = self$from_ref, sort = FALSE)[col_order]
      if (nrow(to_conn) == 0L) return(enforce_schema(from_conn, out_cols))
    }
    if (nrow(to_conn) != 0L) {
      if (nrow(from_to) == 0L) return(enforce_schema(to_conn, out_cols))
      to_conn = merge(from_to, to_conn, by = self$to_ref, sort = FALSE)[col_order]
      if (nrow(from_conn) == 0L) return(enforce_schema(from_conn, out_cols))
    }
    unique(rbind(from_conn, to_conn))
  }

  return_object(self, "Connection")
}

connection_merge = function(x, y, by, output_cols) {
  merge(x, y, by = by, sort = FALSE)[, output_cols]
}
connection_merge = memoise(connection_merge)



#' @export
Flows = function(flows, variables) {
  self = Base()
  self$flows = flows
  self$variables = variables
  self$connections = list()
  for (i in seq_row(self$flows)) {
    self$connections[[i]] = Connection(
        row = self$flows[i, , drop = FALSE]
      , variables = self$variables
      , conn_ref = "flow"
      , from_set = "state", to_set = "state", conn_set = "flow"
      , filter_ref = "partition"
      , join_ref = "partition"
    )
  }
  self$frame = function() {
    if (ncol(self$flows) < 5) return(self$flows)
    do.call(rbind, method_apply(self$connections, "frame"))
  }
  return_object(self, "Flow")
}

#' @export
Trans = function(trans, variables) {
  self = Base()
  self$trans = process_trans_frame(trans)
  self$variables = variables
  self$connections = list()
  for (i in seq_row(self$trans)) {
    self$connections[[i]] = Connection(
        row = self$trans[i, , drop = FALSE]
      , variables = self$variables
      , from_ref = "state"
      , to_ref = "flow"
      , conn_ref = "pop"
      , type_ref = "type"
      , from_set = "state", to_set = "flow", conn_set = "all"
      , filter_ref = "partition"
      , join_ref = "partition"
    )
  }
  self$frame = function() {
    if (ncol(self$trans) < 4) return(self$trans)
    do.call(rbind, method_apply(self$connections, "frame"))
  }
  return_object(self, "Trans")
}

process_trans_frame = function(trans) {
  if (is.null(trans)) trans = empty_frame("state", "flow", "pop", "type")
  trans
}

labelled_frame = function(partition, label_name = "label") {
  f = partition$frame()
  f[[label_name]] = partition$labels()
  f
}

if (FALSE) {
  library(macpan2)
  m = Compartmental("inst/starter_models/minimal_all_index_cases")
  m$flows_expanded()



  m = Compartmental("../macpan2/inst/starter_models/sir_vax/")
  m$flows_info$frame()
  m$flows_info$connections[[5L]]$frame()
  m$flows_expanded()
  m = Compartmental("../macpan2/inst/starter_models/sir_symp/")
  m = Compartmental("../macpan2/inst/starter_models/sir/")
  xx = Flows(m$flows_explicit(), m$variables)
  xx$frame()
  xx$connections[[1]]$row
  yy = xx$connections[[1]]
  debug(yy$from_to_merge)
  debug(yy$from_vars)
  yy$frame()
  cc = Connection(
      row = m$flows()[2,,drop=FALSE]
    , variables = m$variables
    , conn_ref = "flow"
    , filter_ref = "partition"
    , join_ref = "partition"
  )
  cc$from_vars()
  cc$to_vars()
  cc$conn_vars()
  cc$from_to_merge()
  cc$from_conn_merge()
  cc$to_conn_merge()
  cc$frame()
}
