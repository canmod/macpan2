
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
#' extract the derivations of a model object
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
        #tmp = self$.group_variables(derivation)[[j]]$filter(derivation$argument_dots, .wrt = self$.group_inputs(derivation))$labels()
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
#' Replace scalar names with the equivalent vector name
#' 
#' @export
Scalar2Vector = function(model){
  self = Base()
  self$model = model
  self$extracted_derivations = DerivationExtractor(self$model)$extract_derivations()
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
  self$vectorizer = function(extracted_derivation){
    extracted_derivation$outputs = self$.list_replacer(extracted_derivation$outputs)
    extracted_derivation$variables = lapply(extracted_derivation$variables, self$.list_replacer)
    extracted_derivation$variable_dots = lapply(extracted_derivation$variable_dots, self$.list_replacer)
    return(extracted_derivation)
  }
  self$vectorize = function(){
    return(lapply(self$extracted_derivations, self$vectorizer))
  }
  return_object(self, "Scalar2Vector")
}


#' UserExpr
#' 
#' Evaluate user inpu expressions
#' 
#' @export
UserExpr = function(model){
  self = Base()
  self$model = model
  # self$vectorized_derivations = Scalar2Vector(self$model)$vectorize()
  self$scalarized_derivations = DerivationExtractor(self$model)$extract_derivations()
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
      args = mapply(c, extracted_derivation$variables, extracted_derivation$variable_dots)
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
    outputs = extracted_derivation$outputs
    sim_phases = rep(extracted_derivation$simulation_phase, length(outputs))
    return(mapply(list, Output = outputs, Expression = expressions, Simulation_phase = sim_phases, SIMPLIFY = FALSE))
  }
  self$expand_expressions = function(){
    return(lapply(self$scalarized_derivations, self$.format_expression))
  }

  return_object(self, "UserExpr")
}

StandardExpr = function(){
  self = Base()
  #TODO: Evaluate standard expressions
  return_object(self, "StandardExpr")
}

ExpressionFormater = function(){
  self = Base()
  #TODO: Combine all user and standard expressions into a format that can be given to TMBModel.
  return_object(self, "ExpressionFormater")
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

#model_starter("seir", "../../../inst/starter_models/seir_symp")
# sir_test_files = ModelFiles("starter_sir")
# sir_test_files$variables()
# sir_test_files$derivations()
# sir_test_files$flows()
# sir_test_files = model_starter("sir", "LDSKjf")
#v = sir_test_files$derivations()
#valid$is_variables_component$apply(v)
#debug(valid$is_variables_component$apply)
