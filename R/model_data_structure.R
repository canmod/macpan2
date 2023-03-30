
# ValidityMessager(
#   All(
#     Is("data.frame"),
#     MappedAllTest(is.character),
#     TestPipeline(MappedSummarizer(names), MappedAllTest(is.null)),
#
#     ## bound the range of the number of variables
#     ## (question: should we allow one variable or not?)
#     TestPipeline(MappedSummarizer(length), MappedAllTest(Not(TestRange(0L, 0L)))),
#     MappedAllTest(label_requirements),
#
#     ## bound the number of characters that is allowed in partitions
#     TestPipeline(MappedSummarizer(nchar), MappedAllTest(Not(TestRange(0L, 0L)))),
#
#     TestPipeline(MappedSummarizer(length), TestHomo()),
#     TestPipeline(Summarizer(names, is.null), TestFalse()),
#     TestPipeline(Summarizer(names, duplicated, any), TestFalse()),
#     TestPipeline(Summarizer(names), TestBasic(label_requirements))
#   ),
#   "\nInvalid labelled partitions passed to ModelVars.",
#   labelled_partitions_validity_message
# )

# Initialize Model Definition
#
#
#initialize_model_definition(path)

#' Model Collection
#'
#' A model definition that is untied from a set of \code{\link{ModelFiles}}.
#'
#' @param variables Return value of the `variables` method in a
#' \code{\link{ModelFiles}} object.
#' @param derivations Return value of the `derivations` method in a
#' \code{\link{ModelFiles}} object.
#' @param flows Return value of the `flows` method in a
#' \code{\link{ModelFiles}} object.
#' @param settings Return value of the `settings` method in a
#' \code{\link{ModelFiles}} object.
#' @export
ModelCollection = function(variables
    , derivations
    , flows
    , settings
  ) {
  self = Collection(variables, derivations, flows, settings)

  ## methods required of model representations
  self$variables = function() self$get("variables")
  self$derivations = function() self$get("derivations")
  self$flows = function() self$get("flows")
  self$settings = function() self$get("settings")

  return_object(self, "ModelCollection")
}

#' Model Files
#'
#' Construct objects for accessing and caching model definition files.
#'
#' @param model_directory String giving a path to a directory containing
#' the following files, `variables.csv`, `derivations.json`, `flows.csv`,
#' and `settings.json`, described by
#' [this spec](https://canmod.net/misc/model_definitions).
#' @param csv_reader Class inheriting from \code{\link{Reader}} for reading
#' csv files.
#' @param json_reader Class inheriting from \code{\link{Reader}} for reading
#' json files.
#' @param txt_reader Class inheriting from \code{\link{Reader}} for reading
#' txt files.
#'
#' @examples
#' d = system.file("starter_models", "seir_symp_vax", package = "macpan2")
#' m = ModelFiles(d)
#' m$flows()
#' expander = FlowExpander(m)
#' expander$expand_flows()
#'
#' @export
ModelFiles = function(model_directory
    , csv_reader = CSVReader
    , json_reader = JSONReader
    , txt_reader = TXTReader
) {
  self = Files(model_directory
    , reader_spec("variables.csv", csv_reader)
    , reader_spec("derivations.json", json_reader)
    , reader_spec("flows.csv", csv_reader)
    , reader_spec("settings.json", json_reader)
  )

  ## methods required of model representations
  self$variables = function() self$get("variables")
  self$derivations = function() self$get("derivations")
  self$flows = function() self$get("flows")
  self$settings = function() self$get("settings")

  self$freeze = function() {
    ModelCollection(
      self$variables(),
      self$derivations(),
      self$flows(),
      self$settings()
    )
  }

  return_object(self, "ModelFiles")
}

#' Model
#'
#' Construct an object for representing a model structure.
#'
#' @param definition Output of \code{\link{ModelFiles}}.
#' @export
Model = function(definition) {
  self = Base()
  self$def = definition
  self$variables = function() Partition(self$def$variables())
  self$flows = function() self$def$flows()
  self$flows_expanded = function() FlowExpander(self$def)$expand_flows()
  self$flow_variables = function() {
    s = self$def$settings()
    self$variables()$filter(s$flow_variables, .wrt = s$required_partitions)
  }
  self$state_variables = function() {
    s = self$def$settings()
    self$variables()$filter(s$state_variables, .wrt = s$required_partitions)
  }
  self$derivations = self$def$derivations ## TODO: make this more useful
  return_object(self, "Model")
}


#' DerivationExtractor
#'
#' Construct an object for extracting the derivations within a
#' \code{\link{Model}}.
#'
#' @param model Object of type \code{\link{Model}}
#'
#' ## Methods
#'
#' * `$expand_derivation(derivation)` -- Expand a single derivation by name.
#' * `$expand_derivations()` -- Example all derivations in the model.
#'
#' @export
DerivationExtractor = function(model){
  self = Base()
  self$model = model
  self$.filtered_variables = function(derivation){
    if(!is.null(derivation$filter_partition)){
      filtered_variables = self$model$variables()$filter(derivation$filter_names, .wrt = derivation$filter_partition)
    }
    else filtered_variables = self$model$variables()
    return(filtered_variables)
  }

  self$.group_variables = function(derivation){
    if(!is.null(derivation$group_partition)){
      group_variables = lapply(derivation$group_names, self$.filtered_variables(derivation)$filter, .wrt = derivation$group_partition, .comparison_function = all_consistent)
    }
    else {
      group_variables = list(self$.filtered_variables(derivation))
    }
    return(group_variables)
  }

  self$.group_outputs = function(derivation){
    if(!is.null(derivation$output_partition)){
      group_output = lapply(derivation$output_names, self$.filtered_variables(derivation)$filter, .wrt = derivation$output_partition)
    }
    else {
      group_output = lapply(derivation$output_names, self$.filtered_variables(derivation)$filter, .wrt = self$model$def$settings()$required_partitions)
    }

    group_output = method_apply(group_output, "labels")
    return(group_output)
  }

  self$.group_inputs = function(derivation){
    if(!is.null(derivation$input_partition)){
      group_inputs = derivation$input_partition
    }
    else{
      group_inputs = self$model$def$settings()$required_partitions
    }
    return(group_inputs)
  }

  self$.number_of_groups = function(derivation){
    if(!is.null(derivation$output_partition)) number_of_groups = length(derivation$group_names)
    else number_of_groups = 1
    return(number_of_groups)
  }

  self$.filtered_group_variables = function(derivation){
    filtered_group_variables = list()
    if(!is.null(derivation$arguments)){
      for(j in 1:self$.number_of_groups(derivation)){
        unordered_group_variables = self$.group_variables(derivation)[[j]]$filter(derivation$arguments, .wrt = self$.group_inputs(derivation))
        ordered_group_variables = unordered_group_variables$filter_ordered(derivation$arguments, .wrt = self$.group_inputs(derivation))
        filtered_group_variables = c(filtered_group_variables, list(as.list(ordered_group_variables$labels())))
      }
    }
    return(filtered_group_variables)
  }

  self$.filtered_group_variable_dots = function(derivation){
    filtered_group_variable_dots = list()
    if(!is.null(derivation$argument_dots)){
      for(j in 1:self$.number_of_groups(derivation)){
        filtered_group_variable_dots = c(filtered_group_variable_dots, list(as.list(self$.group_variables(derivation)[[j]]$filter(derivation$argument_dots, .wrt = self$.group_inputs(derivation))$labels())))
      }
    }
    return(filtered_group_variable_dots)
  }

  self$extract_derivation = function(derivation){
    return(list(simulation_phase = derivation$simulation_phase, expression = derivation$expression, arguments = derivation$arguments, outputs = self$.group_outputs(derivation), variables = self$.filtered_group_variables(derivation), variable_dots = self$.filtered_group_variable_dots(derivation)))
  }

  self$extract_derivations = function(){
    derivation_list = self$model$derivations()
    return(lapply(derivation_list, self$extract_derivation))
  }

  return_object(self, "DerivationExtractor")
}


#' Scalar2Vector
#'
#' Construct an object for replacing scalar names within a \code{\link{Model}}
#' model, with the equivalent vector name.
#'
#' @param derivation_extractor Object of type \code{\link{DerivationExtractor}}
#'
#' ## Methods
#'
#' * `$vectorizer(expanded_derivation)`
#' * `$vectorize()`
#'
#' @export
Scalar2Vector = function(derivation_extractor){
  self = Base()
  self$model = derivation_extractor$model
  self$extracted_derivations = derivation_extractor$extract_derivations()
  self$.state_pointer = function(scalar_name){
    return(as.numeric(which(scalar_name == self$model$def$settings()[["state_variables"]]))-1)
  }
  self$.state_replacer = function(scalar_name){
    return(paste0("state[", paste0(self$.state_pointer(scalar_name), "]")))
  }
  self$.rate_pointer = function(scalar_name){
    return(as.numeric(which(scalar_name == self$model$def$settings()[["flow_variables"]]))-1)
  }
  self$.rate_replacer = function(scalar_name){
    return(paste0("rate[", paste0(self$.rate_pointer(scalar_name), "]")))
  }
  self$.replacer = function(scalar_name){
    if(any(scalar_name == self$model$def$settings()[["state_variables"]])){
      return(self$.state_replacer(scalar_name))
    }
    else if(any(scalar_name == self$model$def$settings()[["flow_variables"]])){
      return(self$.rate_replacer(scalar_name))
    }
    else return(scalar_name)
  }
  self$.list_replacer = function(scalar_name_list){
    return(lapply(scalar_name_list, self$.replacer))
  }
  self$.output_revisor = function(extracted_derivation){
    old_derivation = list(simulation_phase = extracted_derivation$simulation_phase, expression = extracted_derivation$expression, arguments = extracted_derivation$arguments, outputs = list(), variables = list(), variable_dots = list())
    new_derivation = list(simulation_phase = extracted_derivation$simulation_phase, outputs = list(), variables = list(), variable_dots = list())
    new_derivation$arguments = c("vect_name", "vect_index", extracted_derivation$arguments)
    new_derivation$expression = paste0("assign(vect_name, vect_index, 0, ", paste0(extracted_derivation$expression, ")"))
    for (i in 1:length(extracted_derivation$outputs)){
      if(any(extracted_derivation$outputs[[i]]==self$model$def$settings()[["state_variables"]])){
        new_derivation$outputs = c(new_derivation$outputs, "dummy")
        if(length(extracted_derivation$variables) != 0L) new_derivation$variables = c(new_derivation$variables, list(c("state", self$.state_pointer(extracted_derivation$outputs[[i]]), extracted_derivation$variables[[i]])))
        if (length(extracted_derivation$variable_dots) != 0) new_derivation$variable_dots = c(new_derivation$variable_dots, list(extracted_derivation$variable_dots[[i]]))
      }
      else if(any(extracted_derivation$outputs[[i]]==self$model$def$settings()[["flow_variables"]])){
        new_derivation$outputs = c(new_derivation$outputs, "dummy")
        if(length(extracted_derivation$variables) != 0L) new_derivation$variables = c(new_derivation$variables, list(c("rate", self$.rate_pointer(extracted_derivation$outputs[[i]]), extracted_derivation$variables[[i]])))
        if (length(extracted_derivation$variable_dots) != 0) new_derivation$variable_dots = c(new_derivation$variable_dots, list(extracted_derivation$variable_dots[[i]]))
      }
      else {
        old_derivation$outputs = c(old_derivation$outputs, extracted_derivation$outputs[[i]])
        if(length(extracted_derivation$variables) != 0L) old_derivation$variables = c(old_derivation$variables, list(extracted_derivation$variables[[i]]))
        if (length(extracted_derivation$variable_dots) != 0) old_derivation$variable_dots = c(old_derivation$variable_dots, list(extracted_derivation$variable_dots[[i]]))
      }
    }
    if((length(new_derivation$outputs)!=0L) & length(old_derivation$outputs)!=0L) revised_derivations = c(new_derivation, old_derivation)
    else if(length(new_derivation$outputs)!=0L) revised_derivations = new_derivation
    else revised_derivations = old_derivation
    return(revised_derivations)
  }
  self$vectorizer = function(extracted_derivation){
    extracted_derivation$variables = lapply(extracted_derivation$variables, self$.list_replacer)
    extracted_derivation$variable_dots = lapply(extracted_derivation$variable_dots, self$.list_replacer)
    extracted_derivation = self$.output_revisor(extracted_derivation)
    return(extracted_derivation)
  }
  self$vectorize = function(){
    return(lapply(self$extracted_derivations, self$vectorizer))
  }
  return_object(self, "Scalar2Vector")
}


#' UserExpr
#'
#' Evaluate user input expressions
#'
#' @param model Object created by \code{\link{Model}}.
#'
#' @export
UserExpr = function(model){
  self = Base()
  self$model = model
  self$derivation_extractor = DerivationExtractor(self$model)
  self$vectorized_derivations = Scalar2Vector(self$derivation_extractor)$vectorize()
  self$scalarized_derivations = self$derivation_extractor$extract_derivations()
  self$.vars_check = function(extracted_derivation){
    return(!is.null(extracted_derivation$variables) & !(length(extracted_derivation$variables) == 0L))
  }
  self$.dots_check = function(extracted_derivation){
    return(!is.null(extracted_derivation$variable_dots) & !(length(extracted_derivation$variable_dots) == 0L))
  }
  # self$.vect_check = function(output){
  #
  # }
  self$.make_expression = function(extracted_derivation){
    if(self$.vars_check(extracted_derivation) & self$.dots_check(extracted_derivation)){
      return(MathExpressionFromStrings(extracted_derivation$expression, extracted_derivation$arguments, include_dots = TRUE))
    }
    else if (self$.vars_check(extracted_derivation)){
      return(MathExpressionFromStrings(extracted_derivation$expression, extracted_derivation$arguments, include_dots = FALSE))
    }
    else if (self$.dots_check(extracted_derivation)){
      return(MathExpressionFromStrings(extracted_derivation$expression, include_dots = TRUE))
    }
    else stop("Derivations file appears invalid, no arguments or argument dots.")
  }
  self$.argument_collector = function(extracted_derivation){
    if(self$.vars_check(extracted_derivation) & self$.dots_check(extracted_derivation)){
      args = mapply(c, extracted_derivation$variables, extracted_derivation$variable_dots, SIMPLIFY = FALSE)
    }
    else if (self$.vars_check(extracted_derivation)){
      args = extracted_derivation$variables
    }
    else if (self$.dots_check(extracted_derivation)){
      args = extracted_derivation$variable_dots
    }
    else stop("Derivations file appears invalid, no arguments or argument dots.")
    return(args)
  }
  self$.evaluate_expression = function(extracted_derivation){
    formula = self$.make_expression(extracted_derivation)
    evaluator = function(args) return(do.call(formula$symbolic$evaluate, args))
    evaluated_expressions = lapply(self$.argument_collector(extracted_derivation), evaluator)
    return(evaluated_expressions)
  }
  self$.format_expression = function(extracted_derivation){
    expressions = self$.evaluate_expression(extracted_derivation)
    arguments = self$.argument_collector(extracted_derivation) #This is inefficient as the same method is already called as part of self$.evaluate_expression
    outputs = extracted_derivation$outputs
    sim_phases = rep(extracted_derivation$simulation_phase, length(outputs))
    return(mapply(list, output_names = outputs, expression = expressions, arguments = arguments, simulation_phase = sim_phases, SIMPLIFY = FALSE))
  }
  self$expand_scalar_expressions = function(){
    return(do.call(c, lapply(self$scalarized_derivations, self$.format_expression)))
  }
  self$expand_vector_expressions = function(){
    return(do.call(c, lapply(self$vectorized_derivations, self$.format_expression)))
  }
  return_object(self, "UserExpr")
}

#' StandardExpr
#'
#' Evaluate standard model expressions
#'
#' @param model Object created by \code{\link{Model}}.
#'
#' @export
StandardExpr = function(model){
  self = Base()
  self$model = model
  self$.expanded_flows = self$model$flows_expanded()
  self$.state_length = length(self$model$def$settings()$state_variables)
  self$.state_pointer = function(scalar_name){
    return(as.numeric(which(scalar_name == self$model$def$settings()[["state_variables"]]))-1)
  }
  self$.rate_pointer = function(scalar_name){
    return(as.numeric(which(scalar_name == self$model$def$settings()[["flow_variables"]]))-1)
  }
  self$.rate_types = list("per_capita",
                          "absolute",
                          "per_capita_inflow",
                          "per_capita_outflow",
                          "absolute_inflow",
                          "absolute_outflow")
  self$.inflow_rate_types = list("per_capita",
                                 "absolute",
                                 "per-capita_inflow",
                                 "absolute_inflow")
  self$.outflow_rate_types = list("per_capita",
                                  "absolute",
                                  "per_capita_outflow",
                                  "absolute_outflow")
  self$.flow_seperator = function(rate_type){
    return(self$.expanded_flows[self$.expanded_flows$type == rate_type,])
  }
  self$.seperated_flows = lapply(self$.rate_types, self$.flow_seperator)
  self$.init_index_vector = function(flow_frame){
    from = lapply(flow_frame$from, self$.state_pointer)
    to = lapply(flow_frame$to, self$.state_pointer)
    flow = lapply(flow_frame$flow, self$.rate_pointer)
    return(list(from = from, to = to, flow = flow))
  }
  self$.init_index_vectors = function(){
    output_list = lapply(self$.seperated_flows, self$.init_index_vector)
    names(output_list) = self$.rate_types
    return(output_list)
  }
  self$.flow_tester = function(rate_type){
    return(!(nrow(self$.expanded_flows[self$.expanded_flows$type == rate_type,])==0L))
  }
  self$.init_derivations_list = function(){
    present_flows = unlist(lapply(self$.rate_types, self$.flow_tester), use.names = FALSE)
    present_inflows = unlist(lapply(self$.inflow_rate_types, self$.flow_tester), use.names = FALSE)
    present_outflows = unlist(lapply(self$.outflow_rate_types, self$.flow_tester), use.names = FALSE)

    total_inflow_expression_vct = c("groupSums(per_capita, per_capita_to, state_length)",
                                    "groupSums(absolute, absolute_to, state_length)",
                                    "groupSums(per_capita_outflow, per_capita_outflow_from, state_length)",
                                    "groupSums(absolute_inflow, absolute_inflow_to, state_length)")
    total_inflow_argument_list = list("per_capita", "per_capita_to", "absolute", "absolute_to", "per_capita_inflow",
                                  "per_capita_inflow_to", "absolute_inflow", "absolute_inflow_to", "state_length")

    total_outflow_expression_vct = c("groupSums(per_capita, per_capita_from, state_length)",
                                     "groupSums(absolute, absolute_from, state_length)",
                                     "groupSums(per_capita_outflow, per_capita_outflow_from, state_length)",
                                     "groupSums(absolute_outflow, absolute_outflow_from, state_length)")
    total_outflow_argument_list = list("per_capita", "per_capita_from", "absolute", "absolute_from", "per_capita_outflow",
                                   "per_capita_outflow_from", "absolute_outflow", "absolute_outflow_from", "state_length")

    per_capita_list = list(output_names = "per_capita", expression = "state[per_capita_from]*rate[per_capita_flows]", arguments = list("state", "per_capita_from", "rate", "per_capita_flows"), simulation_phase = "during_update")
    absolute_list = list(output_names = "absolute", expression = "rate[absolute_flows]", arguments = list("rate", "absolute_flows"), simulation_phase = "during_update")
    per_capita_inflow_list = list(output_names = "per_capita_inflow", expression = "state[per_capita_inflow_from]*rate[per_capita_inflow_flows]", arguments = list("state", "per_capita_inflow_from", "rate", "per_capita_inflow_flows"), simulation_phase = "during_update")
    per_capita_outflow_list = list(output_names = "per_capita_outflow", expression = "state[per_capita_outflow_from]*rate[per_capita_outflow_flows]", arguments = list("state", "per_capita_outflow_from", "rate", "per_capita_outflow_flows"), simulation_phase = "during_update")
    absolute_inflow_list = list(output_names = "absolute_inflow", expression = "rate[absolute_inflow_flows]", arguments = list("rate", "absolute_inflow_flows"), simulation_phase = "duing_update")
    absolute_outflow_list = list(output_names = "absolute_outflow", expression = "rate[absolute_outflow_flows]", arguments = list("rate", "absolute_outflow_flows"), simulation_phase = "during_update")
    total_inflow_list = list(output_names = "total_inflow", expression = paste0(total_inflow_expression_vct[present_inflows], collapse = "+") ,arguments = total_inflow_argument_list[c(rep(present_inflows, each = 2), TRUE)] , simulation_phase = "during_update")
    total_outflow_list = list(output_names = "total_outflow", expression = paste0(total_outflow_expression_vct[present_outflows]),arguments = total_outflow_argument_list[c(rep(present_outflows, each = 2), TRUE)] , simulation_phase = "during_update")
    state_list = list(output_names = "state", expression = "state - total_outflow + total_inflow", arguments = list("state", "total_outflow", "total_inflow"), simulation_phase = "during_update")
    output_list = list(per_capita_list, absolute_list, per_capita_inflow_list, per_capita_outflow_list, absolute_inflow_list, absolute_outflow_list, total_inflow_list, total_outflow_list, state_list)
    return(output_list[c(present_flows, rep(TRUE, 3))])
  }
  self$.index_vector_evaluator = function(prefix_string, index_vector, formula){
    return(list(
      list(output_names = paste0(prefix_string, "_from"), expression = do.call(formula$symbolic$evaluate, index_vector$from), arguments = index_vector$from, simulation_phase = "before"),
      list(output_names = paste0(prefix_string, "_to"), expression = do.call(formula$symbolic$evaluate, index_vector$to), arguments = index_vector$to, simulation_phase = "before"),
      list(output_names = paste0(prefix_string, "_flow"), expression = do.call(formula$symbolic$evaluate, index_vector$flow), arguments = index_vector$flow, simulation_phase = "before")
    ))
  }
  self$.index_vectors_evaluator = function(){
    prefix_strings = unique(self$.expanded_flows$type) # self$.rate_types
    #prefix_strings = self$.rate_types
    index_vectors = self$.init_index_vectors()[prefix_strings]
    formula = MathExpressionFromStrings("c(...)", include_dots = TRUE)
    return(do.call(c, mapply(self$.index_vector_evaluator, prefix_strings, index_vectors, MoreArgs = list(formula = formula), SIMPLIFY = FALSE)))
  }
  self$.derivation_evaluator = function(input_list){
    formula = MathExpressionFromStrings(input_list$expression, input_list$arguments)
    expressions = do.call(formula$symbolic$evaluate, input_list$arguments)
    return(list(output_names = input_list$output_names, expression = expressions, arguments = input_list$arguments, simulation_phase = input_list$simulation_phase))
  }
  self$.derivations_evaluator = function(){
    return(lapply(self$.init_derivations_list(), self$.derivation_evaluator))
  }
  self$standard_expressions = function(){
    return(c(self$.index_vectors_evaluator(), self$.derivations_evaluator()))
  }
  return_object(self, "StandardExpr")
}

#' @export
expression_formatter = function(expression_list_element){
  as.formula(paste(expression_list_element$output_names, expression_list_element$expression, sep = " ~ "))
}

#' @export
expression_phase_sorter = function(user_expr_list, standard_expr_list
  , phase = c("before", "during_pre_update", "during_update", "during_post_update", "after")) {

  phase = match.arg(phase)
  user_phases = vapply(user_expr_list, getElement, character(1L), "simulation_phase")
  standard_phases = vapply(standard_expr_list, getElement, character(1L), "simulation_phase")
  c(user_expr_list[user_phases == phase], standard_expr_list[standard_phases == phase])
}

#' @export
create_expr_list_phase = function(user_expr_list, standard_expr_list
  , phase = c("before", "during_pre_update", "during_update", "during_post_update", "after")) {
  lapply(expression_phase_sorter(user_expr_list, standard_expr_list, phase), expression_formatter)
}

#' Model Starter
#'
#' Create a directory with a template model definition.
#'
#' @param starter_name Currently can only be \code{sir}.
#' @param dir_name String giving the path to a directory for copying the
#' template model definition.
#'
#' @export
model_starter = function(starter_name, dir_name) {
  starter_dir = system.file("starter_models", starter_name, package = "macpan2")
  starter_files = list.files(starter_dir)
  required_files = c(
    variables_file = "variables.csv",
    derivations_file = "derivations.json",
    flows_file = "flows.csv",
    settings_file = "settings.json"
  )
  if (!all(required_files %in% starter_files)) {
    stop("Could not find a valid starter model by that name.")
  }

  starter_paths = setNames(
    file.path(starter_dir, required_files),
    names(required_files)
  )

  if (dir.exists(dir_name)) stop("Directory for the model already exists.")
  dir.create(dir_name, recursive = TRUE)

  file.copy(starter_paths, dir_name)
  ModelFiles(dir_name)
}
