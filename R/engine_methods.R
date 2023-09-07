#' Method List
#'
#' @param ... List of \code{\link{Method}} objects.
#' @export
MethList = function(...) {
  self = Base()

  # Args
  self$methods = list(...)

    # DATA_IVECTOR(meth_type_id);
    # // DATA_IVECTOR(meth_id);
    # DATA_IVECTOR(meth_n_mats);
    # DATA_IVECTOR(meth_n_int_vecs);
    # DATA_IVECTOR(meth_mat_id);
    # DATA_IVECTOR(meth_int_vec_id);
  # Static
  self$.null_data_arg = setNames(
    rep(list(integer()), 5),
    sprintf("meth_%s", c(
          "type_id"
        #, "id"
        , "n_mats"
        , "n_int_vecs"
        , "mat_id"
        , "int_vec_id"
      )
    )
  )

  self$data_arg = function(method_names, mat_names, const_names) {
    l = self$.null_data_arg
    for (i in seq_along(self$methods)) {
      new_arg = self$methods[[i]]$data_arg(method_names, mat_names, const_names)
      for (e in names(l)) {
        l[[e]] = c(l[[e]], as.integer(new_arg[[e]]))
      }
    }
    l
  }

  return_object(self, "MethList")
}

#' Engine Method Class
#'
#' Engine methods allow users to interact with the engine
#' by specifying a unique \code{name}. This name is used to reference recipes
#' for computing matrices from other matrices and constants.
#'
#' @param name Method name.
#' @param mat_args List of names of matrices that will be used by the method
#' to produce an output matrix.
#' @param const_args List of names of constants that will be used by the method
#' to produce an output matrix.
Method = function(name, mat_args, const_args) {
  self = Base()

  # Args
  self$name = name
  self$mat_args = mat_args
  self$const_args = const_args

  # Static
  ## abstract -- instantiate with implementation classes (e.g. MethodRowIndexer)
  self$meth_type_id = NA_integer_

  # Private
  self$.method_id = function(method_names) match(self$name, method_names) - 1L
  self$.mat_ids = function(mat_names) match(self$mat_args, mat_names) - 1L
  self$.const_ids = function(const_names) match(self$const_args, const_names) - 1L

  # Standard Methods
  self$data_arg = function(method_names, mat_names, const_names) {
    list(
      ## these must be length-1 integer vectors
        meth_type_id = self$meth_type_id
      , meth_id = self$meth_type_id
      , meth_n_mats = length(self$mat_args)
      , meth_n_const = length(self$const_args)

      ## these must be length-meth_n_mats and length-meth_n_const respectively
      , meth_mat_id = self$.mat_ids(mat_names)
      , meth_const_id = self$.const_ids(const_names)
    )
  }

  return_object(self, "Method")
}

meth_cls_types = c("MethodRowIndexer")

mk_meth_cls = function(cls_nm, meth_type_id) {
  pf = parent.frame()
  force(pf)
  force(cls_nm)
  force(meth_type_id)
  f = function(name, mat_args, const_args) {
    self = Method(name, mat_args, const_args)
    self$meth_type_id = meth_type_id
    return_object(self, cls_nm)
  }
  assign(cls_nm, f, envir = pf)
}
for (i in seq_along(meth_cls_types)) mk_meth_cls(meth_cls_types[i], i)
