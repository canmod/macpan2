#' @export
EngineMethods = function(...) {
  self = Base()
  self$method_exprs = list(...)

  self$methods = function() {
    method_objs = list()
    for (meth_nm in names(self$method_exprs)) {
      m = self$method_exprs[[meth_nm]]
      args = c(
        as.list(c(meth_nm, m$mat, m$id_nm)),
        init_mats = m$init_mats,
        int_vecs = self$int_vecs()
      )
      method_objs[[meth_nm]] = do.call(m$constructor, args)
    }
    do.call(MethList, method_objs)
  }
  self$int_vecs = function() {
    int_vec_objs = list()
    for (meth_nm in names(self$method_exprs)) {
      m = self$method_exprs[[meth_nm]]
      int_vec_objs[[m$id_nm]] = m$indices()
    }
    do.call(IntVecs, int_vec_objs)
  }
  self$refresh_init_mats = function(init_mats) {
    for (meth_nm in names(self$method_exprs)) {
      m = self$method_exprs[[meth_nm]]$init_mats = init_mats
    }
  }
  return_object(self, "EngineMethods")
}

MethodRowIndexer = NULL

#' @export
FromRows = function(mat
    , by
    , id_nm
    , init_mats = MatsList()
  ) {
  self = Base()

  self$mat = mat
  self$by = by
  self$id_nm = id_nm
  self$init_mats = init_mats

  self$call = deparse1(match.call())
  self$constructor = MethodRowIndexer

  self$indices = function() {
    l = list()
    l[[self$id_nm]] = get_indices(self$by
      , vec = self$init_mats$rownames()[[self$mat]]
      , vec_type = "variables"
      , expr_as_string = self$call
      , zero_based = TRUE
    )
    l
  }
  return_object(self, "FromRows")
}


#' Method List
#'
#' @param ... List of \code{\link{Method}} objects.
#' @export
MethList = function(...) {
  self = Base()

  # Args
  self$methods = list(...)

  # Static
  self$.null_data_arg = setNames(
    rep(list(integer()), 5),
    sprintf("meth_%s", c(
          "type_id"
        , "n_mats"
        , "n_int_vecs"
        , "mat_id"
        , "int_vec_id"
      )
    )
  )

  self$data_arg = function() {
    l = self$.null_data_arg
    new_args = method_apply(self$methods, "data_arg")
    for (a in names(l)) {
      l[[a]] = c(l[[a]], unlist(lapply(new_args, `[[`, a), recursive = FALSE, use.names = FALSE))
    }
    l
  }

  return_object(self, "MethList")
}

#' @export
names.MethList = function(x) {
  vapply(x$methods, getElement, character(1L), "name")
}

#' Engine Method Class
#'
#' Engine methods allow users to interact with the engine
#' by specifying a unique \code{name}. This name is used to reference recipes
#' for computing matrices from other matrices and constants.
#'
#' @param name Method name.
#' @param mat_args Character vector of names of matrices that will be used by
#' the method to produce an output matrix.
#' @param int_vec_args Character vector of names of constants that will be used
#' by the method to produce an output matrix.
#' @export
Method = function(name, mat_args, int_vec_args, init_mats = MatsList(), int_vecs = IntVecs()) {
  self = Base()

  # Args
  self$name = name
  self$mat_args = mat_args
  self$int_vec_args = int_vec_args
  self$init_mats = init_mats
  self$int_vecs = int_vecs

  # Static
  ## abstract -- instantiate with implementation classes (e.g. MethodRowIndexer)
  self$meth_type_id = NA_integer_

  # Private
  self$.mat_ids = function() match(self$mat_args, names(self$init_mats)) - 1L
  self$.int_vec_ids = function() match(self$int_vec_args, names(self$int_vecs)) - 1L

  # Standard Methods
  self$data_arg = function() {
    list(
      ## these must be length-1 integer vectors
        meth_type_id = self$meth_type_id
      , meth_n_mats = length(self$mat_args)
      , meth_n_int_vecs = length(self$int_vec_args)
      ## these must be length-meth_n_mats and length-meth_n_const respectively
      , meth_mat_id = self$.mat_ids()
      , meth_int_vec_id = self$.int_vec_ids()
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
  f = function(name, mat_args, const_args, init_mats = MatsList(), int_vecs = IntVecs()) {
    self = Method(name, mat_args, const_args, init_mats, int_vecs)
    self$meth_type_id = meth_type_id
    return_object(self, cls_nm)
  }
  assign(cls_nm, f, envir = pf)
}
for (i in seq_along(meth_cls_types)) mk_meth_cls(meth_cls_types[i], i)
