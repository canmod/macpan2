LedgerData = function(...) {
  self = Base()
  self$ledger_list = list(...)
  labelling_column_names_list = (self$ledger_list
    |> lapply(getElement, "labelling_column_names_list")
    |> unname()
    |> unique()
  )
  stopifnot(length(labelling_column_names_list) == 1L)
  self$labelling_column_names_list = labelling_column_names_list[[1L]]

  reference_index_list = (self$ledger_list
    |> lapply(getElement, "reference_index_list")
    |> unname()
    |> unique()
  )
  stopifnot(length(reference_index_list) == 1L)
  self$reference_index_list = reference_index_list[[1L]]

  table_names = (self$ledger_list
    |> method_apply("table_names")
    |> unname()
    |> unique()
  )
  stopifnot(length(table_names) == 1L)
  self$table_names = table_names[[1L]]

  self$labels_frame = function() {
    (self$ledger_list
      |> method_apply("labels_frame")
      |> bind_rows()
    )
  }

  self$positions_frame = function(zero_based = FALSE) {
    positions_list = list()
    for (i in seq_along(self$ledger_list)) {
      positions_list[[i]] = list()
      for (d in self$table_names) {
        positions_list[[i]][[d]] = self$ledger_list[[i]]$positions_for[[d]](zero_based)
      }
      positions_list[[i]] = as.data.frame(positions_list[[i]])
    }
    bind_rows(positions_list)
  }

  return_object(self, "LedgerData")
}


# print.LedgerData = function(x, ...) {
#   print(x$frame, row.names = FALSE)
# }

#' Bundle up Ledgers
#'
#' Bundle up several ledgers (see \code{\link{LedgerDefinition}}) to pass
#' to \code{\link{mp_dynamic_model}}.
#'
#' @param ... Ledgers to bundle up.
#'
#' @export
mp_ledgers = function(...) {
  wrap_ledgers_in_one_element_lists = function(x) {
    if (inherits(x, "Ledger")) return(list(x))
    if (inherits(x, "list")) return(x)
    stop("You can only pass ledgers and/or lists of ledgers.")
  }
  args = (list(...)
    |> lapply(wrap_ledgers_in_one_element_lists)
    |> unlist(recursive = FALSE, use.names = FALSE)
  )
  do.call(LedgerData, args)
}


#' Indexed Expressions (not currently used -- experimental)
#'
#' @param ... Formula objects that reference the columns in the
#' \code{index_data}, the vectors in \code{vector_list} and the matrices
#' in \code{unstructured_matrix_list}.
#' @param ledgers An object produced using \code{\link{mp_ledgers}}.
#' @param vector_list Named list of objected produced using
#' \code{\link{mp_structured_vector}}.
#' @param unstructured_matrix_list Named list of objects that can be coerced
#' to a matrix.
#'
#' @noRd
IndexedExpressions = function(...
    , index_data
    , vector_list = list()
    , unstructured_matrix_list = list()
  ) {
  self = Base()
  self$formulas = list(...)
  self$index_data = index_data
  self$vector_list = vector_list
  self$unstructured_matrix_list = unstructured_matrix_list
  self$int_vecs = function(zero_based = FALSE) {
    self$index_data$positions_frame(zero_based) |> as.list()
  }
  self$mats_list = function() {
    all_vars = (self$formulas
      |> lapply(formula_components)
      |> lapply(getElement, "variables")
      |> unlist(use.names = FALSE, recursive = FALSE)
      |> unique()
    )
    derived = setdiff(all_vars, c(
      names(self$vector_list),
      self$index_data$table_names,
      names(self$unstructured_matrix_list)
    ))
    derived = (empty_matrix
      |> list()
      |> rep(length(derived))
      |> setNames(derived)
    )
    vectors = method_apply(self$vector_list, "numbers")
    unstruc = self$unstructured_matrix_list
    c(vectors, unstruc, derived)
  }
  self$simulate = function(time_steps = 1L) {
    simple_sims(
      self$formulas,
      time_steps,
      self$int_vecs(zero_based = TRUE),
      self$mats_list()
    )
  }
  return_object(self, "IndexedExpressions")
}


mp_indexed_exprs = IndexedExpressions

