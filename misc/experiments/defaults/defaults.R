library(macpan2)
library(oor)

Derivation = function(derivation, required_partition) {
  impute = function(default) {
    function(x) {
      if (is.null(x)) return(default)
      x
    }
  }
  impute_partition = impute(required_partition)
  impute_phase = impute("before")
  self = Base()
  self$simulation_phase = impute_phase(derivation$simulation_phase)
  self$filter_partition = impute_partition(derivation$filter_partition)
  self$filter_names = derivation$filter_names
  self$group_partition = impute_partition(derivation$group_partition)
  self$group_names = derivation$group_names
  self$output_partition = impute_partition(derivation$output_partition)
  self$output_names = macpan2:::valid$char$assert(derivation$output_names)
  self$input_partition = impute_partition(derivation$input_partition)
  self$arguments = derivation$arguments
  self$argument_dots = derivation$argument_dots
  self$expression = macpan2:::valid$char1$assert(derivation$expression)


  self$outputs = function() {
    Partition(macpan2:::StringDataFromDotted(self$output_names, self$output_partition)$frame())
  }
  return_object(self, "Derivation")
}

defaults_path = "misc/experiments/defaults/sir_defaults"
defaults = macpan2:::Defaults(macpan2:::DefaultFiles(defaults_path))
defaults$numeric$vector
defaults$numeric$filter_vector("state", .wrt = "Matrix")
defaults$numeric$matrix("contact", "Age", "AgeInfective")

defaults$numeric$partition$expand("Epi.Vax.Symp")
xx = Derivation(defaults$def$derivations()[[1L]], "Epi")
xx$outputs()


defaults$initialized_variables()
defaults$matrix_names()
defaults$initialized_matrix_long("state")
defaults$initialized_matrix_long("rate")

required_partitions = initialized_variables$name()

defaults$dimensions()
defaults$defaults()

## assumes that all derivations are in reduced form
updated_labels = unique(unlist(lapply(defaults$derivations(), getElement, "output_names"), recursive = TRUE))
derived_labels = updated_labels[!updated_labels %in% initialized_variables$labels()]
derived_variables = Partition(macpan2:::StringDataFromDotted(derived_labels, required_partitions)$undot()$frame())

variables = Partition(rbind(initialized_variables$frame(), derived_variables$frame()))

settings = list(
  required_partitions = initialized_variables$names(),
  null_partition = "Null",
  state_variables = state_labels,
  flow_variables = unique(defaults$flows()$flow)
)

definition_path = "misc/experiments/defaults/sir_definition"
jsonlite::write_json(settings, file.path(definition_path, "settings.json"), pretty = TRUE)
file.copy(file.path(defaults_path, "flows.csv"), file.path(definition_path, "flows.csv"), overwrite = TRUE)
write.csv(variables$frame(), file.path(definition_path, "variables.csv"), quote = FALSE, row.names = FALSE)
file.copy(file.path(defaults_path, "derivations.json"), file.path(definition_path, "derivations.json"), overwrite = TRUE)
