AbstractDerivationList = function() {
  self = Base()

  ## Each derivation in the list has an item in each of the following lists
  self$expressions = list()
  self$arguments = list()
  self$output_labels = list()

  ## Character vector with the full list of variable labels
  self$all_variable_labels = character(0L)

  self$.parse_one = function(e, v) {
    valid_vars = initial_valid_vars(v)
    #valid_funcs = macpan2:::valid_funcs
    f = as.formula(paste("~", e))
    parse_expr(f)
  }
  self$.parse = function(expressions, all_variable_labels) {
    lapply(expressions, self$.parse_one, all_variable_labels)
  }
  self$parse = function() {
    table_list = self$.parse(self$expressions, self$all_variable_labels)
    literals_list = lapply(table_list, getElement, "valid_literals")
    TMBExpressions(table_list
      , initial_valid_vars(self$all_variable_labels)
      , literals_list
    )
  }
  return_object(self, "AbstractDerivationList")
}

#' Expand Derivation Definitions
#'
#' @param derivation Model derivations.
#' @param variables Model variables.
#' @param required_partitions Undotted names of partitions used when naming
#' variables.
#'
#' @importFrom oor method_apply
#' @export
ExpandDerivationDefinitions = function(derivation, variables, required_partitions) {
  v = StringDataFromFrame(variables)
  rp = StringUndottedVector(required_partitions)$dot()$value()
  if (is.null(derivation$output_partition)) {
    derivation$output_partition = rp
  }
  output_filter = StringDataFromDotted(derivation$output_names, derivation$output_partition)$undot()
  filtered_data = v
  if (!is.null(derivation$filter_names)) {
    filtering_data = StringDataFromDotted(derivation$filter_names, derivation$filter_partition)$undot()
    filtered_data = v$filter(filtering_data, all_consistent)
  }
  if (!is.null(derivation$group_names)) {
    grouping_data = StringDataFromDotted(derivation$group_names, derivation$group_partition)$undot()
    grouped_data = filtered_data$group(grouping_data, all_consistent)
    # print(grouping_data)
    # print(grouped_data)
  } else {
    grouped_data = list(filtered_data)
  }
  {
    output_data = (filtered_data
      $ordered_unique_filter(output_filter, all_equals)
      $change_coordinates(required_partitions)
      $dot()
      $labels()
      $value()
    )
  }
  if (!is.null(derivation$arguments)) {
    if (is.null(derivation$input_partition)) {
      derivation$input_partition = rp
    }
    argument_filter = StringDataFromDotted(derivation$arguments, derivation$input_partition)$undot()
    argument_data = (grouped_data
      |> method_apply("ordered_unique_filter", argument_filter, all_equals)
      |> method_apply("change_coordinates", required_partitions)
      |> method_apply("dot")
      |> method_apply("labels")
      |> method_apply("value")
    )
  } else {
    argument_data = list(character(0L))
  }
  if (!is.null(derivation$argument_dots)) {
    if (is.null(derivation$input_partition)) {
      derivation$input_partition = rp
    }
    dots_filter = StringDataFromDotted(derivation$argument_dots, derivation$input_partition)$undot()
    dots_data = (grouped_data
      |> method_apply("ordered_unique_filter", dots_filter, all_equals)
      |> method_apply("change_coordinates", required_partitions)
      |> method_apply("dot")
      |> method_apply("labels")
      |> method_apply("value")
    )
  } else {
    dots_data = list(character(0L))
  }
  all_args_data = lapply(
    mapply(c, argument_data, dots_data, SIMPLIFY = FALSE, USE.NAMES = FALSE),
    as.list
  )
  math = MathExpressionFromStrings(derivation$expression, derivation$arguments, include_dots = TRUE)
  expression_data = lapply(all_args_data, do.call, what = math$symbolic$evaluate)
  self = AbstractDerivationList()
  self$arguments = all_args_data
  self$expressions = expression_data
  self$output_labels = output_data
  self$all_variable_labels = v$change_coordinates(required_partitions)$labels()$dot()$value()
  return_object(self, "DerivationList")
}



JoinDerivations = function(...) {
  self = AbstractDerivationList()
  derivation_lists = unlist(list(...), recursive = FALSE)
  self$arguments = do.call(c, lapply(derivation_lists, getElement, "arguments"))
  self$expressions = do.call(c, lapply(derivation_lists, getElement, "expressions"))
  self$output_labels = do.call(c, lapply(derivation_lists, getElement, "output_labels"))
  self$all_variable_labels = derivation_lists[[1L]]$all_variable_labels
  return_object(self, "DerivationList")
}


DerivationList = function(expressions, arguments, output_labels, all_variable_labels, literals) {
  self = AbstractDerivationList()
  self$expressions = expressions
  self$arguments = arguments
  self$output_labels = output_labels
  self$all_variable_labels = all_variable_labels
  return_object(self, "DerivationList")
}
