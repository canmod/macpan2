#' @export
setClass(
  'var_type',
  representation(
    var_list = "list",
    type_check = "function",
    time_varying = "logical"
  ),
  validity = function(object) {
    valid_check = sum(
      unlist(
        lapply(
          lapply(
            c(is_scalar, is_vector, is_matrix),
            all.equal,
            object@type_check
          ),
          isTRUE
        )
      )
    ) == 1L
    if (!valid_check) return("Invalid type checking function")
    homogeneous_types = all(unlist(lapply(object@var_list, object@type_check)))
    if (!homogeneous_types) return("Not all variable have the correct types")
    if (length(object@time_varying) != 1L) {
      return("time_varying must be a single boolean value")
    }
    if (!has_valid_names(object@var_list)) {
      return("all variables in variable lists must be uniquely named")
    }
    TRUE
  }
)

#' @export
scalars = function(...) {
  new(
    'var_type',
    var_list = list(...),
    type_check = is_scalar,
    time_varying = FALSE
  )
}

#' @export
vectors = function(...) {
  new(
    'var_type',
    var_list = list(...),
    type_check = is_vector,
    time_varying = FALSE
  )
}

#' @export
matrices = function(...) {
  new(
    'var_type',
    var_list = list(...),
    type_check = is_matrix,
    time_varying = FALSE
  )
}

#' @export
scalars_tv = function(...) {
  new(
    'var_type',
    var_list = list(...),
    type_check = is_scalar,
    time_varying = TRUE
  )
}

#' @export
vectors_tv = function(...) {
  new(
    'var_type',
    var_list = list(...),
    type_check = is_vector,
    time_varying = TRUE
  )
}

#' @export
matrices_tv = function(...) {
  new(
    'var_type',
    var_list = list(...),
    type_check = is_matrix,
    time_varying = TRUE
  )
}

#' @export
model_vars = function(...) new('model_vars', type_list = list(...))

setClass(
  'model_vars',
  representation(type_list = "list"),
  validity = function(object) {
    if (!all(unlist(lapply(object@type_list, is, "var_type")))) {
      return("Type list must only contain type objects")
    }
    nms = names(object)
    var_nms = unlist(dimnames(object))
    if (is.null(nms)) return("types need names")
    if (any(duplicated(nms))) return("type names need to be unique")
    if (any(nchar(nms) == 0L)) return("blank type names are not allowed")
    if (any(duplicated(var_nms))) {
      return("variable names need to be unique across the entire model")
    }
  }
)

#' @export
setMethod(
  f = "show",
  signature = "var_type",
  definition = function(object){
    str(object@var_list)
  }
)

#' @export
setMethod(
  f = "show",
  signature = "model_vars",
  definition = function(object) {
    l = lapply(object@type_list, slot, 'var_list')
    str(l)
  }
)

#' @export
setMethod("names", c(x = "var_type"), function(x) {names(x@var_list)})

#' @export
setMethod("names", c(x = "model_vars"), function(x) {names(x@type_list)})

#' @export
setMethod("dimnames", c(x = "model_vars"), function(x) {lapply(x@type_list, names)})

#' @export
as.list.var_type = function(x, ...) {
  x@var_list
}

#' @export
as.list.model_vars = function(x, ...) {
  lapply(x@type_list, as.list)
}

#' @export
matrix_list = function(x) {
  sapply(
    unclassify(x),
    as.matrix,
    simplify = FALSE
  )
}

#' @export
unclassify = function(x) {
  do.call(c, unname(as.list(x)))
}

#' @export
eval_expr = function(x, valid_vars, valid_funcs) {
  y = eval(

    # assume one-sided formula with the actual expression
    # in the second element
    x[[2L]],

    # valid objects in the expression
    c(valid_vars, valid_funcs),

    # don't look outside valid variables and functions
    # so that we fail if those objects are not present
    enclos = emptyenv()
  )
  # if (!is_scalar(y)) {
  #   stop("only scalars are allowed")
  # }
  y
}

#' @export
eval_expr_dims = function(x, valid_vars, valid_funcs) {
  eval(

    # assume one-sided formula with the actual expression
    # in the second element
    x[[2L]],

    # valid objects in the expression
    c(lapply(valid_vars, as_dim_mock), valid_funcs),
  )
}
