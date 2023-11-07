LinkData = function(...) {
  self = Base()
  self$link_list = list(...)

  labelling_column_names_list = (self$link_list
    |> lapply(getElement, "labelling_column_names_list")
    |> unname()
    |> unique()
  )
  stopifnot(length(labelling_column_names_list) == 1L)
  self$labelling_column_names_list = labelling_column_names_list[[1L]]

  reference_index_list = (self$link_list
    |> lapply(getElement, "reference_index_list")
    |> unname()
    |> unique()
  )
  stopifnot(length(reference_index_list) == 1L)
  self$reference_index_list = reference_index_list[[1L]]

  table_names = (self$link_list
    |> method_apply("table_names")
    |> unname()
    |> unique()
  )
  stopifnot(length(table_names) == 1L)
  self$table_names = table_names[[1L]]

  self$labels_frame = function() {
    (self$link_list
      |> method_apply("labels_frame")
      |> bind_rows()
    )
  }

  self$positions_frame = function(zero_based = FALSE) {
    positions_list = list()
    for (i in seq_along(self$link_list)) {
      positions_list[[i]] = list()
      for (d in self$table_names) {
        positions_list[[i]][[d]] = self$link_list[[i]]$positions_for[[d]](zero_based)
      }
      positions_list[[i]] = as.data.frame(positions_list[[i]])
    }
    bind_rows(positions_list)
  }

  return_object(self, "LinkData")
}

#' #' @export
#' print.LinkData = function(x, ...) {
#'   print(x$frame, row.names = FALSE)
#' }


#' @export
mp_link_data = function(...) LinkData(...)


#' Indexed Expressions
#'
#' @param ... Formula objects that reference the columns in the
#' \code{index_data}, the vectors in \code{vector_list} and the matrices
#' in \code{unstructured_matrix_list}.
#' @param index_data An object produced using \code{\link{mp_link_data}}.
#' @param vector_list Named list of objected produced using
#' \code{\link{mp_vector}}.
#' @param unstructured_matrix_list Named list of objects that can be coerced
#' to a matrix.
#'
#' @export
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
      |> lapply(macpan2:::formula_components)
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


#' @export
mp_indexed_exprs = IndexedExpressions

