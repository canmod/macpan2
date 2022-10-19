# Checker = function() {
#   self = Base()
#   self$check = function(x) {stop("not implemented abstract method")}
#   return_object(self, "Checker")
# }
#
# CheckerPipeline = function(stages) {
#   self = Checker()
#
#   stopifnot(is.list(stages))
#   self$stages = stages
#
#   self$check = function(x) {
#     for (stage in self$stages) {
#       if (!stage$check(x)) return(FALSE)
#     }
#     return(TRUE)
#   }
#   return_object(self, "CheckerPipeline")
# }
#
# Is = function(is_function, non_validity_message) {
#   self = Checker()
#
#   stopifnot(is.function(is_function))
#   stopifnot(is.character(non_validity_message))
#   stopifnot(length(non_validity_message) == 1L)
#
#   self$non_validity_message = non_validity_message
#   self$is_function = is_function
#
#   self$check = function(x) self$is_function(x)
#
#   return_object(self, "Is")
# }
#
# IsNot = function(is_function, non_validity_message) {
#   self = Is(is_function, non_validity_message)
#   self$check = function(x) !self$is_function(x)
#   return_object(self, "IsNot")
# }
#
# IsApply = function(is_function, summarizing_function, non_validity_message) {
#   self = Is(is_function, non_validity_message)
#   self$summarizing_function = summarizing_function
#
#   self$check = function(x) {
#     list_of_booleans = vapply(x, self$is_function, logical(1L))
#     self$summarizing_function(list_of_booleans)
#   }
#   return_object(self, "IsApply")
# }
#
# AllInList = function(is_function, non_validity_message) {
#   self = IsApply(is_function, base::all, non_validity_message)
#   return_object(self, "AllInList")
# }
#
# AnyInList = function(is_function, non_validity_message) {
#   self = IsApply(is_function, base::any, non_validity_message)
#   return_object(self, "AnyInList")
# }
#
# HomoList = function(homogenizing_function, non_validity_message) {
#   self = Checker()
#   self$homogenizing_function = homogenizing_function
#   self$non_validity_message = non_validity_message
#   self$check = function(x) {
#     length(unique(lapply(x, self$homogenizing_function))) == 1L
#   }
#   return_object(self, "HomoList")
# }
#
# IsRange = function(lower, upper, non_validity_message) {
#   self = Checker()
#   self$lower = lower
#   self$upper = upper
#   self$non_validity_message = non_validity_message
#
#   self$check = function(x) {
#     all((self$lower <= x) & (self$upper >= x))
#   }
#   return_object(self, "IsRange")
# }
#
# xx = IsRange(1, 2, 'aa')
# attributes(xx) = NULL
# class(xx)
# xx$check(34)
