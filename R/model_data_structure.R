
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
  self = Base()
  self$.components = nlist(variables, derivations, flows, settings)
  self$.get = function(component_name) self$.components[[component_name]]

  ## methods required of model representations
  self$variables = function() self$.get("variables")
  self$derivations = function() self$.get("derivations")
  self$flows = function() self$.get("flows")
  self$settings = function() self$.get("settings")

  self$freeze = function() self

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
  self = Base()
  self$.directory = normalizePath(model_directory)
  self$.readers = list(
    variables = csv_reader(self$.directory, "variables.csv"),
    derivations = json_reader(self$.directory, "derivations.json"),
    flows = csv_reader(self$.directory, "flows.csv"),
    settings = json_reader(self$.directory, "settings.json")
  )
  self$.access_times = setNames(
    rep(list(Sys.time()), length(self$.readers)),
    names(self$.readers)
  )
  self$.components = setNames(
    vector("list", length(self$.readers)),
    names(self$.readers)
  )
  # read data and store it, bumping the access time
  self$.fetch = function(component_name) {
    self$.access_times[[component_name]] = Sys.time()
    self$.components[[component_name]] =
      self$.readers[[component_name]]$read()
  }
  # read data, store it, return it, bumping the access time
  self$.read = function(component_name) {
    self$.fetch(component_name)
    self$.components[[component_name]]
  }
  # fill the components fields
  self$.components = setNames(
    lapply(names(self$.readers), self$.read),
    names(self$.readers)
  )
  # fetch data only if it was last accessed before it changed
  self$.pull = function(component_name) {
    access_time = self$.access_times[[component_name]]
    modification_time = file.mtime(self$.readers[[component_name]]$file)
    if (modification_time > access_time) self$.fetch(component_name)
  }
  # pull data (i.e. fetch it only if necessary) and return it
  self$.get = function(component_name) {
    self$.pull(component_name)
    self$.components[[component_name]]
  }

  ## methods required of model representations
  self$variables = function() self$.get("variables")
  self$derivations = function() self$.get("derivations")
  self$flows = function() self$.get("flows")
  self$settings = function() self$.get("settings")

  ## Convert to a ModelCollection object, which is equivalent to
  ## a ModelFiles object in that the variables, derivations,
  ## flows, and settings methods return objects with the
  ## same requirements. The difference is that a ModelFiles object
  ## will update what it returns if the associated files change
  ## on disk, whereas ModelCollection is intended to be
  ## (by convention) immutable (hence the verb 'freeze'). The
  ## 'by convention' bit means that a user is free to change the
  ## contents of `.components`, but this would violate
  ## the convention.
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

  self$user_expressions = UserDefinedExpr(self$variables(), self$derivations(), self$def$settings(), self$flows())

  return_object(self, "Model")
}




DerivationExpander = function(model){
  self = Base()
  self$model = model
  .filtered_variables = function(derivation){
    if(!is.null(derivation$filter_partition)){
      filtered_variables = variables$filter(derivation$filter_names, .wrt = derivation$filter_partition)
    } 
    else filtered_variables = variables
    return(filtered_variables)
  }
  
  .group_variables = function(derivation){
    if(!is.null(derivation$group_partition)){
      group_variables = lapply(derivation$group_names, filtered_variables$filter, .wrt = derivation$group_partition, .comparison_function = all_consistent)
      # group_variables = lapply(derivation$group_names, self$.filtered_variables(derivation)$filter, .wrt = derivation$group_partition, .comparison_function = all_consistent)
    }
    else {
      group_variables = list(filtered_variables)
    }
    return(group_variables)
  }
  
  .group_outputs = function(filtered_variables, derivation, settings){
    if(!is.null(derivation$output_partition)){
      group_outputs = lapply(derivation$output_names, filtered_variables$filter, .wrt = derivation$output_partition)
    }
    else {
      group_outputs = lapply(derivation$output_names, filtered_variables$filter, .wrt = settings$required_partitions)
    }
    return(group_outputs)
  }
  
  .group_inputs = function(derivation, settings){
    if(!is.null(derivation$input_partition)){
      group_inputs = derivation$input_partition
    }
    else{
      group_inputs = settings$required_partitions
    }
    return(group_inputs)
  }
  
  .number_of_groups = function(derivation){
    if(!is.null(derivation$output_partition)) number_of_groups = length(derivation$group_names)
    else number_of_groups = 1
    return(number_of_groups)
  }
  
  .filtered_group_variables = function(derivation, group_variables, group_inputs){
    filtered_group_variables = list()
    if(!is.null(derivation$arguments)){
      for(j in 1:self$.number_of_groups(derivation)){
        filtered_group_variables = c(filtered_group_variables, group_variables[[j]]$filter(derivation$arguments, .wrt = group_inputs))
      }
    }
    return(filtered_group_variables)
  }
   
  .filtered_group_variable_dots = function(derivation, group_variables, group_inputs){ 
    filtered_group_variable_dots = list()
    if(!is.null(derivation$argument_dots)){
      for(j in 1:self$.number_of_groups){
        filtered_group_variable_dots = c(filtered_group_variable_dots, group_variables[[j]]$filter(derivation$argument_dots, .wrt = group_inputs))
      }
    }
    return(filterd_group_variable_dots)
  }
  
  expand_derivation = function(derivation){
    
  }
  
  expand_derivations = function(){
    
  }
  
  return_object(self, "DerivationExpander")
}

Scalar2Vector = function(){
  self = Base()
  #TODO: Replace scalar variables with the equivalent vector variables
  return_object(self, "Scalar2Vector")
}

UserExpr = function(){
  self = Base()
  #TODO: Evaluate user provided expressions using variables obtained from Scalar2Vector.
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

#' Reader
#'
#' Construct objects for reading data.
#'
#' @param ... Character vectors giving path components.
#'
#' @export
Reader = function(...) {
  self = Base()
  self$file = normalizePath(file.path(...))
  self$read = function() {
    stop("Abstract class that is not implemented.")
  }
  return_object(self, "Reader")
}

#' @describeIn Reader Read CSV files.
#' @export
CSVReader = function(...) {
  self = Reader(...)
  self$.empty = function(row) {
    isTRUE(all((row == "") | startsWith(row, " ")))
  }
  self$read = function() {
    data_frame = read.table(
      self$file, sep = ",", quote = "", na.strings = character(0L),
      colClasses = "character", header = TRUE,
      strip.white = TRUE, blank.lines.skip = TRUE,
      stringsAsFactors = TRUE
    )
    data_frame[!apply(data_frame, 1, self$.empty), , drop = FALSE]
  }
  return_object(self, "CSVReader")
}

#' @describeIn Reader Read JSON files.
#' @importFrom jsonlite fromJSON
#' @export
JSONReader = function(...) {
  self = Reader(...)
  self$read = function() {
    jsonlite::fromJSON(self$file, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
  }
  return_object(self, "JSONReader")
}

#' @describeIn Reader Read TXT files.
#' @export
TXTReader = function(...) {
  self = Reader(...)
  self$read = function() readLines(self$file)
  return_object(self, "TXTReader")
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
