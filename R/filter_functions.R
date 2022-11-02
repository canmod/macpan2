# StateTransformer = function(relevant_indices) {
#   self = Base()
#
#   # S set
#   self$.relevant_indices = relevant_indices
#
#   # rho
#   self$original_to_zeroed = function(x) {
#     x[self$.relevant_indices] = 0
#     x
#   }
#
#   # sigma
#   self$zeroed_to_shortened = function(x) {
#     x[self$.relevant_indices]
#   }
#
#   # tau
#   self$original_to_shortened = function(x) {
#     x[self$.relevant_indices]
#   }
#   return_object(self, "StateTransformer")
# }
#
#
