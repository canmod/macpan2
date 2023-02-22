
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
  self$rate_variables = function() {
    s = self$def$settings()
    self$variables()$filter(s$rate_variables, .wrt = s$required_partitions)
  }
  self$state_variables = function() {
    s = self$def$settings()
    self$variables()$filter(s$state_variables, .wrt = s$required_partitions)
  }
  self$derivations = self$def$derivations ## TODO: make this more useful
  self$vctr_pointer = function(vrbl, vctr_nm = "state"){
    vctr = self$def$settings()[[paste0(vctr_nm, "_variables")]]
    return(as.numeric(which(vrbl == vctr))-1)# -1 because c++ side uses zero based indexing.
  }
  self$from_states = function(){
    from_flows = self$flows_expanded()$from
    return(lapply(from_flows, self$state_pointer))
  }
  self$to_states = function(){
    to_flows = self$flows_expanded()$to
    return(lapply(to_flows, self$state_pointer))
  }
  # self$init_vals = function(){
  #   #TODO: obtain all required values.
  # }
  # self$init_mats = function(){
  #   #TODO: initiate all required variables. Including:
  #   #1) state, flow, rate
  #   #2) from, to, from_lngth, to_lngth
  #   #3) flow variables
  # }
  self$standard_expressions = function(){
    flw_frml = MathExpressionFromStrings("state[from]*rate", list("state", "from", "rate"))
    symblc_flw_frml = do.call(flw_frml$symbolic$evaluate, list("state", "from", "rate"))
    
    stt_frml = MathExpressionFromStrings("state - groupSums(state, from, from_lngth)+groupSums(state, to, to_lngth)", list("state", "from", "from_lngth", "to", "to_lngth"))
    symblc_stt_frml = do.call(stt_frml$symbolic$evaluate, list("state", "from", "from_lngth", "to", "to_lngth"))
    
    return(list(list("flow", symblc_flw_frml), list("state", symblc_stt_frml)))
  }
  self$vctr_replacer = function(symbol, vrbls, vctr_nm = "state") {
    if(any(symbol == vrbls)){
      return(paste0(paste0(paste0(vctr_nm,"["), self$vctr_pointer(symbol, vctr_nm)), "]"))
    }
    else return(symbol)
  }
  self$user_expressions = function(){
    stt_vrbls = self$state_variables()$labels()
    rt_vrbls = self$rate_variables()$labels()
    derivation_list = self$derivations()
    nmbr_of_drvtns = length(derivation_list)
    before = list()
    during_pre_update = list()
    during_post_update = list()
    after = list()
    for ( i in 1:nmbr_of_drvtns){
      if(!is.null(derivation_list[[i]]$filter_partition)){
        vrbls = self$variables()$filter(derivation_list[[i]]$filter_names, .wrt = derivation_list[[i]]$filter_partition)
      } 
      else vrbls = self$variables()
      if(!is.null(derivation_list[[i]]$group_partition)){
        nmbr_of_grps = length(derivation_list[[i]]$group_names)
        grp_vrbls = lapply(derivation_list[[i]]$group_names, vrbls$filter, .wrt = derivation_list[[i]]$group_partition, .comparison_function = all_consistent)
      }
      else {
        nmbr_of_grps = 1
        grp_vrbls = list(vrbls)
      }
      if(!is.null(derivation_list[[i]]$output_partition)){
        grp_outputs = lapply(derivation_list[[i]]$output_names, vrbls$filter, .wrt = derivation_list[[i]]$output_partition)
      }
      else {
        grp_outputs = lapply(derivation_list[[i]]$output_names, vrbls$filter, .wrt = self$def$settings()$required_partitions)
      }
      if(!is.null(derivation_list[[i]]$input_partition)){
        grp_inputs = derivation_list[[i]]$input_partition
      }
      else{
        grp_inputs = self$def$settings()$required_partitions
      }
      
      nondots_flag = !is.null(derivation_list[[i]]$arguments) #Does the derivation have regular (i.e. not related to dots) arguments
      dots_flag = !is.null(derivation_list[[i]]$argument_dots) #Does the derivation have arguments to go in place of dots
      
      if(nondots_flag){
        fltrd_grp_vrbls = list()
        for(j in 1:nmbr_of_grps){
          fltrd_grp_vrbls = c(fltrd_grp_vrbls, grp_vrbls[[j]]$filter(derivation_list[[i]]$arguments, .wrt = grp_inputs))
        }
      }
      if(dots_flag){
        #dots_flag = TRUE
        fltrd_grp_vrbls_dts = list()
        for(j in 1:nmbr_of_grps){
          fltrd_grp_vrbls_dts = c(fltrd_grp_vrbls_dts, grp_vrbls[[j]]$filter(derivation_list[[i]]$argument_dots, .wrt = grp_inputs))
        }
      }
      grp_exprs_list = list()
      if(nondots_flag & dots_flag){
        frml = MathExpressionFromStrings(derivation_list[[i]]$expression, derivation_list[[i]]$arguments, include_dots = TRUE)
        for(j in 1:nmbr_of_grps){
          ordrd_grp_vrbls = fltrd_grp_vrbls[[j]]$filter_ordered(derivation_list[[i]]$arguments, .wrt = grp_inputs)
          symblc_input = c(ordrd_grp_vrbls$labels(), flted_grp_vrbls_dts[[j]]$labels())
          symblc_input = lapply(symblc_input, self$vctr_replacer, stt_vrbls)
          symblc_input = lapply(symblc_input, self$vctr_replacer, rt_vrbls, vctr_nm = "rate")
          symblc_frml = do.call(frml$symbolic$evaluate, symblc_input)
          symblc_output = grp_outputs[[j]]$labels()
          symblc_output = lapply(symblc_output, self$vctr_replacer, stt_vrbls)
          symblc_output = lapply(symblc_output, self$vctr_replacer, rt_vrbls, vctr_nm = "rate")
          grp_exprs_list = append(grp_exprs_list, list(list(symblc_output, symblc_frml)))
        }
      }
      else if(nondots_flag){
        frml = MathExpressionFromStrings(derivation_list[[i]]$expression, derivation_list[[i]]$arguments)
        for(j in 1:nmbr_of_grps){
          ordrd_grp_vrbls = fltrd_grp_vrbls[[j]]$filter_ordered(derivation_list[[i]]$arguments, .wrt = grp_inputs)
          symblc_input = ordrd_grp_vrbls$labels()
          symblc_input = lapply(symblc_input, self$vctr_replacer, stt_vrbls)
          symblc_input = lapply(symblc_input, self$vctr_replacer, rt_vrbls, vctr_nm = "rate")
          symblc_frml = do.call(frml$symbolic$evaluate, symblc_input)
          symblc_output = grp_outputs[[j]]$labels()
          symblc_output = lapply(symblc_output, self$vctr_replacer, stt_vrbls)
          symblc_output = lapply(symblc_output, self$vctr_replacer, rt_vrbls, vctr_nm = "rate")
          grp_exprs_list = append(grp_exprs_list, list(list(symblc_output, symblc_frml)))
        }
      }
      else if(dots_flag){
        frml = MathExpressionFromStrings(derivation_list[[i]]$expression, include_dots = TRUE)
        for(j in 1:nmbr_of_grps){
          symblc_input = fltrd_grp_vrbls_dts[[j]]$labels()
          symblc_input = lapply(symblc_input, self$vctr_replacer, stt_vrbls)
          symblc_input = lapply(symblc_input, self$vctr_replacer, rt_vrbls, vctr_nm = "rate")
          symblc_frml = do.call(frml$symbolic$evaluate, symblc_input)
          symblc_output = grp_outputs[[j]]$labels()
          symblc_output = lapply(symblc_output, self$vctr_replacer, stt_vrbls)
          symblc_output = lapply(symblc_output, self$vctr_replacer, rt_vrbls, vctr_nm = "rate")
          grp_exprs_list = append(grp_exprs_list, list(list(symblc_output, symblc_frml)))
        }
      }
      else{
        stop("Invalid derivations file? No arguments or argument dots.")
      }
      if(derivation_list[[i]]$simulation_phase == "before") before = append(before, list(grp_exprs_list))
      else if(derivation_list[[i]]$simulation_phase == "during_pre_update") during_pre_update = append(during_pre_update, list(grp_exprs_list))
      else if(derivation_list[[i]]$simulation_phase == "during_post_update") during_post_update = append(during_post_update, list(grp_exprs_list))
      else if(derivation_list[[i]]$simulation_phase == "after") after = append(after, list(grp_exprs_list))
      else stop("Unrecognized simulation phase")
    }
    usr_exprs = list(before, during_pre_update, during_post_update, after)
    names(usr_exprs) = c("before", "during_pre_update", "during_post_update", "after")
    return(usr_exprs)
  }
  # self$prep_expresions = function(){
  #   #TODO: collect all expressions together into single list
  #   #      Order is: 1. "before" expressions
  #   #                2. "during_pre_update" expressions
  #   #                3. assign variable rate expressions
  #   #                4. standard expressions
  #   #                5. "during_post_update" expressions
  #   #                6. "after" expressions
  #   # Note: 2 - 5 should be collected together as "during" expressions
  # }
  return_object(self, "Model")
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
