# ## not used -- but don't quite want to remove from the R folder
# 
# Formula = function(formula, name = "", mats_list = MatsList()) {
#   self = Base()
#   self$formula = formula
#   self$name = name
#   self$method_types = MethodTypes()
#   self$lhs = function() lhs(self$formula)
#   self$rhs = function() rhs(self$formula)
#   self$could_be_getter = function() {
#     self$method_types$could_make_method(self$rhs())
#   }
#   self$could_be_setter = function() {
#     self$method_types$could_make_method(self$formula)
#   }
#   self$method_name = function() {
#     pt = method_parser(self$lhs())
#     name = paste0(rev(pt$x[pt$n == 0]), collapse = "_")
#     if (self$could_be_setter()) {
#       return(sprintf("set_%s", name))
#     }
#     sprintf("get_%s", name)
#   }
#   self$setter = function() {
#     try(self$method_types$make_method(self$formula), silent = TRUE)
#   }
#   self$getter = function() {
#     try(self$method_types$make_method(self$rhs()), silent = TRUE)
#   }
#   self$mat_args = function() {
#     meth = self$setter()
#     if (inherits(meth, "try-error")) meth = self$getter()
#     if (!inherits(meth, "try-error")) return(meth$mat_args())
#     c(mat_vec_nms(self$rhs()), mat_vec_nms(self$lhs())) |> unique()
#   }
#   self$int_vec_args = function() {
#     meth = self$setter()
#     if (inherits(meth, "try-error")) meth = self$getter()
#     if (!inherits(meth, "try-error")) return(meth$int_vec_args())
#     character(0L)
#   }
#   self$expr_list = function() {
#     if (self$could_be_setter()) {
#       f = two_sided("dummy", self$method_name())
#     } else if (self$could_be_getter()) {
#       f = two_sided(lhs_char(self$formula), self$method_name())
#     } else if (is_two_sided(self$formula)) {
#       f = to_assign(self$formula)
#     } else {
#       stop("Invalid formula")
#     }
#     setNames(list(f), self$name)
#   }
#   self$meth_list = function() {
#     if (self$could_be_setter()) {
#       f = self$formula
#     } else if (self$could_be_getter()) {
#       f = self$rhs()
#     } else {
#       return(list())
#     }
#     setNames(list(f), self$method_name())
#   }
#   return_object(self, "Formula")
# }
