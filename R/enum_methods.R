## Auto-generated - do not edit by hand

#' Method Prototype
#'
#' Define a method type using a prototype. These prototypes can be compared
#' with methods defined in R to see if they are consistent with a method
#' type that has been defined in C++. All
#' arguments are automatically derived through comments in the C++ code where
#' the method types are defined.
#'
#' @param formula Formula for defining a method type using a prototype.
#' @param mat_arg_nms Character vector naming the matrix-valued arguments.
#' @param int_vec_arg_nms Character vector naming the integer-vector-valued
#' arguments.
#'
#' @noRd
MethodPrototype = function(formula, mat_arg_nms, int_vec_arg_nms) {
  self = Base()
  self$formula = formula
  self$mat_arg_nms = mat_arg_nms
  self$int_vec_arg_nms = int_vec_arg_nms
  self$mat_args = function(other_formula) {
    stopifnot(self$consistent(other_formula))
    that = concat_parse_table(other_formula)
    this = concat_parse_table(self$formula)
    which_mats = this$x %in% self$mat_arg_nms
    that$x[which_mats]
  }
  self$int_vec_args = function(other_formula) {
    stopifnot(self$consistent(other_formula))
    that = concat_parse_table(other_formula)
    this = concat_parse_table(self$formula)
    which_int_vecs = this$x %in% self$int_vec_arg_nms
    that$x[which_int_vecs]
  }
  self$as_character = function() deparse(self$formula)
  self$is_setter = function(other_formula) {
    two_sided(self$formula) & two_sided(other_formula)
  }
  self$is_getter = function(other_formula) {
    one_sided(self$formula) & one_sided(other_formula)
  }
  self$parse_table = function() {
    method_parser(self$formula)
  }
  self$consistent = function(other_formula) {
    this = concat_parse_table(self$formula)
    that = concat_parse_table(other_formula)
    this_funs = this$x[this$n > 0L]
    that_funs = that$x[that$n > 0L]
    good_n_sig = identical(this$n, that$n)
    good_fun_names = identical(this_funs, that_funs)
    good_n_sig & good_fun_names
  }

  return_object(self, "MethodPrototype")
}

#' Make Method Class
#'
#' Place a method object in the package namespace for a given method type
#' defined in the C++ code.
#'
#' @param cls_nm Character string giving the name of the class.
#' @param meth_type_id Integer giving the associated ID of the method type.
#'
#' @noRd
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

#' Method Type Utilities
#'
#' This class is here so that `MethodTypes`, which is automatically generated
#' from the C++ code, can inherit from it.
#'
#' @noRd
MethodTypeUtils = function() {
  self = Base()

  # return all prototypes as character strings
  self$all_prototype_formulas = function() {
    l = list()
    for (m in self$method_ordering) {
      l[[m]] = self[[m]]$as_character()
    }
    unlist(l, use.names = FALSE)
  }

  # construct an object of class `Method`
  #
  # @param formula -- a user-supplied formula for defining a method. this
  # formula gets compared with the prototypes to decide what method type to
  # use for this user-defined method.
  # @param meth_nm -- character string giving the method name.
  self$make_method = function(formula, meth_nm) {
    for (meth_type_nm in self$method_ordering) {
      if (self[[meth_type_nm]]$consistent(formula)) {
        method_cls = mk_meth_cls(
          cls_nm = var_case_to_cls_case(meth_type_nm),
          meth_type_id = match(meth_type_nm, self$method_ordering)
        )
        method = method_cls(meth_nm
          , self[[meth_type_nm]]$mat_args(formula)
          , self[[meth_type_nm]]$int_vec_args(formula)
        )
        return(method)
      }
    }
    stop(
        "\nThe following engine method formula ...\n\n", deparse(formula), "\n\n"
      , "... is inconsistent with all of the available prototypes:\n\n"
      , paste0(self$all_prototype_formulas(), collapse = "\n")
    )
  }

  self$could_make_method = function(formula) {
    v = setNames(logical(length(self$method_ordering)), self$method_ordering)
    for (meth_type_nm in self$method_ordering) {
      v[meth_type_nm] = self[[meth_type_nm]]$consistent(formula)
    }
    any(v)
  }

  return_object(self, "MethodTypesUtil")
}

MethodTypes = function() {
  self = MethodTypeUtils()
  self$method_ordering = c("meth_from_rows", "meth_to_rows", "meth_rows_to_rows", "meth_mat_mult_to_rows", "meth_tv_mat_mult_to_rows", "meth_group_sums", "meth_tv_mat", "meth_rows_times_rows")
  self$meth_from_rows = MethodPrototype(~ Y[i], "Y", "i")
  self$meth_to_rows = MethodPrototype(Y[i] ~ X, c("Y", "X"), "i")
  self$meth_rows_to_rows = MethodPrototype(Y[i] ~ X[j], c("Y", "X"), c("i", "j"))
  self$meth_mat_mult_to_rows = MethodPrototype(Y[i] ~ A %*% X[j], c("Y", "A", "X"), c("i", "j"))
  self$meth_tv_mat_mult_to_rows = MethodPrototype(Y[i] ~ time_var(A, change_points, block_size, change_pointer) %*% X[j], c("Y", "A", "X"), c("i", "j", "change_points", "block_size", "change_pointer"))
  self$meth_group_sums = MethodPrototype(~ groupSums(Y, i, n), "Y", c("i", "n"))
  self$meth_tv_mat = MethodPrototype(~ time_var(Y, change_points, block_size, change_pointer), "Y", c("change_points", "block_size", "change_pointer"))
  self$meth_rows_times_rows = MethodPrototype(~ A[i] * X[j], c("A", "X"), c("i", "j"))
  return_object(self, "MethodTypes")
}
