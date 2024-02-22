#' Optimization Parameters List
#'
#' Create an object for specifying matrix elements to be optimized or integrated
#' out of the objective function using a Laplace transform.
#'
#' @param ... Objects that can be coerced to numeric vectors, which will be
#' concatenated to produce the default value of the parameter vector.
#' @param par_id Integer vector identifying elements of the parameter vector
#' to be used to replace elements of the model matrices.
#' @param mat Character vector the same length as `par_id` giving the names of
#' the matrices containing the elements to replace.
#' @param row_id Integer vector the same length as `par_id` giving the row
#' indices of the matrix elements to replace with parameter values.
#' @param col_id Integer vector the same length as `par_id` giving the column
#' indices of the matrix elements to replace with parameter values.
#'
#' @return Object of class \code{OptParamsList} with the following methods.
#'
#' ## Methods
#'
#' * `$data_arg(..., .type_string = c("p", "r"))`: Return the following components of the data structure
#' to pass to C++.
#'     * `{.type_string}_par_id` -- Integers identifying the replacing parameter.
#'     * `{.type_string}_mat_id` -- Integers identifying the matrix within which
#'     an element is to be replaced.
#'     * `{.type_string}_row_id` -- Integers identifying the rows within matrices
#'     to replace.
#'     * `{.type_string}_col_id` -- Integers identifying the columns within
#'     matrices to replace.
#' * `$vector()`: Return the initial value of the numerical parameter vector.
#' * `$data_frame()`: Return a data frame with each row describing a parameter.
#'
#' ## Method Arguments
#'
#' * `...`: Character vector containing the names of the matrices in the model.
#' * `.type_string`: Either `"p"` or `"r"` indicating whether the object
#' is to be used to represent fixed parameters to be optimized or random
#' parameters to be integrated out using the Laplace transform.
#'
#' @noRd
OptParamsList = function(...
    , par_id = integer(0L)
    , mat = character(0L)
    , row_id = integer(0L)
    , col_id = integer(0L)
  ) {
  self = Base()
  self$.vector = as.numeric(unlist(list(...)))
  self$vector = function() self$.vector

  # TMB needs at least one parameter (unless this is for a random effect),
  # so setting to zero without setting anything else so that it doesn't
  # actually get used
  #if (length(self$.vector) == 0L) self$.vector = 0
  self$.mat = mat
  self$.par_id = par_id
  self$.row_id = row_id
  self$.col_id = col_id
  self$.mat_id = function(...) {
    match(self$.mat, as.character(unlist(list(...)))) - 1L
  }
  self$data_frame = function(...) {
    d = data.frame(par_id = self$.par_id
      , mat = self$.mat
      , row = self$.row_id
      , col = self$.col_id
      , default = self$.vector[self$.par_id + 1L]
    )
    alternative_vectors = list(...)
    for (v in names(alternative_vectors)) {
      d[[v]] = alternative_vectors[[v]][self$.par_id + 1L]
    }
    d
  }
  self$data_arg = function(.type_string = c("p", "r")) {
    .type_string = match.arg(.type_string)
    r = setNames(
      list(self$.par_id
        , self$.mat_id(names(self$init_mats))
        , self$.row_id
        , self$.col_id
      ),
      paste(.type_string, c("par", "mat", "row", "col"), "id", sep = "_")
    )
    valid$opt_params_list_arg$assert(r)
  }

  ## Composition
  self$init_mats = MatsList()

  return_object(self, "OptParamsList")
}

OptParamsFrameStruc = function(..., frame) {
  OptParamsList(...
    , par_id = frame$par_id
    , mat = frame$mat
    , row_id = frame$row_id
    , col_id = frame$col_id
  )
}


## alternative constructor of OptParamsList
OptParamsFrame = function(frame, .dimnames = list()) {
  for (c in names(frame)) {
    if (is.character(frame[[c]])) frame[[c]] = trimws(frame[[c]])
  }
  if (is.null(frame$col)) frame$col = 0L
  if (is.null(frame$row)) frame$row = 0L
  
  frame = rename_synonyms(frame
    , mat = c("matrix", "Matrix", "mat", "Mat", "variable", "var", "Variable", "Var")
    , row = c("row", "Row")
    , col = c("col", "Col", "column", "Column")
    , default = c("value", "Value", "val", "Val", "default", "Default")
  )
  # synonyms_for_default = c("default", "Default", "value", "Value", "val", "Val")
  # synonym_present = synonyms_for_default %in% names(frame)
  # if (isTRUE(any(synonym_present))) {
  #   synonym = synonyms_for_default[which(synonym_present)[1L]]
  #   names(frame)[names(frame) == synonym] = "default"
  # } else {
  #   msg(
  #       msg_hline()
  #     , msg_colon(
  #         msg(
  #             "None of the following column names, which are valid for"
  #           , "containing default parameter values"
  #         )
  #       , msg_indent_break(synonyms_for_default)
  #     )
  #   ) |> stop()
  # }
  row_col_ids = make_row_col_ids(frame$mat, frame$row, frame$col, .dimnames)
  args = c(
    as.list(as.numeric(frame$default)),
    list(
      par_id = seq_len(nrow(frame)) - 1L,  ## zero-based c++ indices
      mat = frame$mat,
      row_id = row_col_ids$row_id,
      col_id = row_col_ids$col_id
    )
  )
  do.call(OptParamsList, args)
}

OptParamsFile = function(file_path
      , csv_reader = CSVReader
      , json_reader = JSONReader
      , txt_reader = TXTReader
    ) {
  self = Files(dirname(file_path)
    , reader_spec(basename(file_path), csv_reader)
  )
  self$.col_map = c(
      Matrix = "mat"
    , matrix = "mat"
    , mat = "mat"
    , Mat = "mat"
    , Row = "row"
    , row = "row"
    , Column = "col"
    , Col = "col"
    , col = "col"
    , type = "type"
    , Type = "type"
    , value = "default"
    , Value = "default"
    , Val = "default"
    , val = "default"
    , Default = "default"
    , default = "default"
  )
  self$.param = c("param", "par", "fixed", "fixef")
  self$.random = c("random", "ran", "rand", "ranef")
  self$frame = function() {
    x = self$get("parameters")
    names(x) = self$.col_map[names(x)]
    x
  }
  self$params_frame = function(.dimnames = list()) {
    f = self$frame()
    f[tolower(f[["type"]]) %in% self$.param, , drop = FALSE]
    #if (nrow(f) == 0L) return(OptParamsList())
  }
  self$random_frame = function(.dimnames = list()) {
    f = self$frame()
    f[tolower(f[["type"]]) %in% self$.random, , drop = FALSE]
    #if (nrow(f) == 0L) return(OptParamsList())
  }
  self
}
