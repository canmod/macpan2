# is_matrix_like = oor::ValidityMessager(
#   test_function = oor::Any(
#     oor::TestPipeline(
#       oor::Summarizer(class),
#       oor::Is("Mat")
#     ),
#     oor::All(
#       is.numeric,
#       oor::TestPipeline(
#         oor::Summarizer(dim, length),
#         oor::TestRange(0L, 2L)
#       )
#     )
#   ),
#   "Not a matrix-like object.",
#   "Matrix-like objects need to ...",
#   "  - be numeric vectors",
#   "  - have no dim attribute or a dim attribute with two or fewer dimensions"
# )

# is_matrix_like$assert("a")
#
# Mat = function(matrix_like) {
#   self = Base()
#   self$.matrix_like = is_matrix_like$assert(matrix_like)
#   return_object(self, "Mat")
# }
#
# Mats = function(...) {
#   self = Base()
#   self$
# }

#
#
#
#
#
#
# TMBMacro = function(...) {
#   self = Base()
#   self$.input_list = list(...)
#   self$to_tmb = function() {
#     return(self$.input_list)
#   }
#   return_object(self, "TMBMacro")
# }
#
#
#
# InputMatrices = function(...) {
#   self = TMBMacro(...)
#   return_object(self, "InputMatrices")
# }
#
# Parameters = function(...) {
#   self = TMBMacro(...)
# }
#
#
#
#
#
#
# valid_mats = function(mats) {
#
#   is_list =
#   if (!is_list) return(FALSE)
#
#   is_empty = length(mats) == 0L
#   if (is_empty) return(TRUE)
#
#   all_numeric =
#   if (!all_numeric) return(FALSE)
#
#
#   if (!all_mats) return(FALSE)
#
#   return(TRUE)
# }
#
# valid_mats_config = function(mats_config) {
#   is_list = is.list(mats_config)  # data frames etc are OK
#   if (!is_list) return(FALSE)
#
#   good_names =
#   if (!good_names) return(FALSE)
#
#   all_logical = all(vapply(mats_config, is.logical, logical(1L)))
#   if (!all_logical) return(FALSE)
#
#   same_length = length(unique(vapply(mats_config, length, integer(1L)))) == 1L
#   if (!same_length) return(FALSE)
#
#   return(TRUE)
# }
#
# component_validity = list(
#   mats = IsPipeline(
#     IsStage(is_plain_list),
#     AllInList(is.numeric),
#     AllInList(IsDimRange(0L, 2L))
#   ),
#   mats_config = IsPipeline(
#     IsStage(is.list),
#     NamesEqual("mats_save_hist", "mats_return"),
#     AllInList(is.logical),
#     HomoList(length)
#   ),
#   params = IsPipeline(
#     IsStage(is_plain_list),
#
#   )
# )
#
#
# component_validity$mats(initial_values$mats)
# component_validity$mats_config(initial_values$mats_config)
#
# initial_values = list(
#   mats = list(),
#   mats_config = data.frame(
#     mats_save_hist = logical(0L),
#     mats_return = logical(0L)
#   ),
#   params = data.frame(
#     params = numeric(0L),
#     p_par_id = integer(0L),
#     p_mat_id = integer(0L),
#     p_row_id = integer(0L),
#     p_col_id = integer(0L)
#   ),
#   random_index = data.frame(
#     random = numeric(0L),
#     r_par_id = integer(0L),
#     r_mat_id = integer(0L),
#     r_row_id = integer(0L),
#     r_col_id = integer(0L)
#   )
# )
#
#
#
