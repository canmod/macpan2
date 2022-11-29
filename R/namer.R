NameAndLabelUtilities = function() {
  self = Base()

  self$.csv = function(x) paste0(as.character(x), collapse = ",")
  self$.dot = function(x) paste0(as.character(x), collapse = ".")
  self$.tager = MathExpressionFromFunc(function(label, partition) partition[label])
  self$.tag = function(labels, partitions) {
    mapply(
      self$.tager$symbolic$evaluate,
      labels,
      partitions,
      USE.NAMES = FALSE,
      SIMPLIFY = TRUE
    )
  }
  self$.mat = function(x) {
    m = vapply(x, as.character, character(nrow(x)))
    if (is.null(dim(m))) m = matrix(m, nrow = 1L)
    m
  }
  self$.parts = function(x) sub("\\[[a-zA-Z0-9_]*\\]$", "", x)
  self$.labs = function(x, parts) {
    re = paste(
      "^(",
      paste0(parts, collapse = "|"),
      ")\\[([a-zA-Z0-9_]*)\\]$",
      sep = ""
    )
    sub(re, "\\2", x)
  }
  self$.names = function(variables) {
    var_mat = self$.mat(variables)
    apply(var_mat, 1L, self$.dot)
  }
  self$.names_inverse = function(names, partition_names) {
    read.table(text = names, sep = ".", col.names = partition_names)
  }
  self$.names_full_inverse = function(names, partition_names, all_partition_names) {
    labelled_partitions = self$.names_inverse(names, partition_names)
    labels_data = list()
    for (partition in all_partition_names) {
      if (partition %in% partition_names) {
        labels_data[[partition]] = labelled_partitions[[partition]]
      } else {
        labels_data[[partition]] = character(nrow(labelled_partitions))
      }
    }
    as.data.frame(labels_data)
  }
  # self$string = function(variables) {
  #   var_mat = self$.mat(variables)
  #   tagged_mat = matrix(
  #     sweep(var_mat, 2L, names(variables), self$.tag),
  #     nrow = nrow(variables)
  #   )
  #   apply(tagged_mat, 1L, self$.csv)
  # }
  # self$string_inverse = function(strings) {
  #   #browser()
  #   vars = strsplit(strings, ",")
  #   partition_names = unique(lapply(vars, self$.parts))
  #   if (length(partition_names) != 1L) stop("Malformed variable names")
  #   partition_names = partition_names[[1L]]
  #   var_mat = t(vapply(vars
  #     , self$.labs
  #     , character(length(partition_names))
  #     , partition_names
  #   ))
  #   setNames(as.data.frame(var_mat), partition_names)
  # }
  return_object(self, "NameAndLabelUtilities")
}

#' Name and Label Translator
#'
#' @export
NameAndLabelTranslator = function() {
  ## v -- variables
  ## pl -- partition_labels -- vector representing one column of the variables list
  ## pn -- partition_names -- names of an ordered subset of the columns of the variables list
  ## ps -- partition_set -- ordered subset of the columns of the variables list
  ## psn -- partition_set_name -- dot-delimited concatenation of the names of the columns in a partition_set
  ## psl -- partition_set_labels -- dot-delimited concatenation of the partition_labels in a partition_set
  ## vn -- variable_names -- partition_set_labels with respect to the required partition_set

  self = NameAndLabelUtilities()
  self$pn_to_psn = function(partition_names) {
    self$.dot(partition_names)
  }
  self$psn_to_pn = function(partition_set_name) {
    unlist(strsplit(partition_set_name, "\\."), use.names = FALSE)
  }
  self$v_to_ps_with_pn = function(variables_list, partition_names) {
    variables_list[, partition_names, drop = FALSE]
  }
  self$v_to_ps_with_psn = function(variables_list, partition_set_name) {
    variables_list[, self$psn_to_pn(partition_set_name), drop = FALSE]
  }
  self$ps_to_psl = function(partition_set) {
    self$.names(partition_set)
  }
  self$v_to_psl_with_pn = function(variable_list, partition_names) {
    self$.names(self$v_to_ps_with_pn(variable_list, partition_names))
  }
  self$v_to_psl_with_psn = function(variable_list, partition_set_name) {
    self$.names(self$v_to_ps_with_psn(variable_list, partition_set_name))
  }
  self$ps_to_v = function(partition_set, all_partition_names) {
    partition_names = names(partition_set)
    variables = list()
    for (name in all_partition_names) {
      if (name %in% partition_names) {
        variables[[name]] = partition_set[[name]]
      } else {
        variables[[name]] = character(nrow(partition_set))
      }
    }
    as.data.frame(variables)
  }

  return_object(self, "NameAndLabelTranslator")
}

Select = function() {
  self = NameAndLabelTranslator()
  self$select = function(variables
    , partition_set_name
  ) {
    self$v_to_ps_with_psn(variables, partition_set_name)
  }
  return_object(self, "Select")
}

FilterTraits = function() {
  self = NameAndLabelTranslator()
  self$.filter_blanks_not_special = function(variables
      , partition_set_name
      , partition_set_labels
    ) {
    if (is.null(partition_set_name)) return(variables)
    all_partition_set_labels =
      self$v_to_psl_with_psn(variables, partition_set_name)
    keepers = all_partition_set_labels %in% partition_set_labels
    variables[keepers, , drop = FALSE]
  }
  self$.filter_always_match_blanks = function(variables
    , partition_set_name
    , partition_set_labels
  ) {
    full_set = self$v_to_ps_with_psn(variables, partition_set_name)
    target_set = self$.names_inverse(partition_set_labels, self$psn_to_pn(partition_set_name))
    filtered_set = variables[apply(mapply(`%in%`, full_set, target_set) | sapply(full_set, `==`, ""), 1L, all), , drop = FALSE]
    filtered_set
  }
  self$group = function(variables
    , partition_set_name
    , partition_set_labels
  ) {
    lapply(partition_set_labels
      , self$filter
      , variables = variables
      , partition_set_name = partition_set_name
    )
  }
  return_object(self, "FilterTraits")
}

#' Filter so that all names
#'
#' @export
FilterBlanksNotSpecial = function() {
  self = FilterTraits()
  self$filter = function(variables
    , partition_set_name
    , partition_set_labels
  ) {
    self$.filter_blanks_not_special(variables
      , partition_set_name
      , partition_set_labels
    )
  }
  return_object(self, "Filter")
}

#' @export
FilterAlwaysMatchBlanks = function() {
  self = FilterTraits()
  self$filter = function(variables
    , partition_set_name
    , partition_set_labels
  ) {
    self$.filter_always_match_blanks(variables
      , partition_set_name
      , partition_set_labels
    )
  }
  return_object(self, "Filter")
}

#' @export
FilterUniqueAndSort = function() {
  self = FilterTraits()
  self$filter = function(variables
    , partition_set_name
    , partition_set_labels
  ) {
    variables = self$.filter_blanks_not_special(variables
      , partition_set_name
      , partition_set_labels
    )
    if (nrow(variables) != length(partition_set_labels)) {
      stop("filtered variables are not unique against the partition_set")
    }
    unsorted_psl = self$v_to_psl_with_psn(variables, partition_set_name)
    variables[match(partition_set_labels, unsorted_psl), , drop = FALSE]
  }
  return_object(self, "Filter")
}


# m = ModelFiles("inst/starter_models/seir_symp_vax/")
# f1 = FilterBlanksNotSpecial()
# f2 = FilterAlwaysMatchBlanks()
# f3 = FilterUniqueAndSort()
# s = Select()
# x = f1$filter(m$variables()
#   , "SympStruc.Vax"
#   , c(
#     "unstructured.vax", "effective.unvax",
#     "unstructured.unvax", "effective.vax",
#     "unstructured.", "effective."
#   )
# )
# y = f2$group(x, "Vax", c("unvax", "vax"))
# lapply(y, f3$filter, "Epi", c("I", "beta", "N"))


#' Namer
#'
#' Name model variables and parse names into labelled partitions.
#'
#' @return An object of class \code{Namer} with two methods public,
#' \code{names} and \code{partitions}, which are inverses of each other.
#'
#' @export
Namer = function() {
  self = NameAndLabelTranslator()

  self$filter = function(variables
      , partition_set_name
      , partition_set_labels
    ) {
    if (is.null(partition_set_name)) return(variables)
    all_partition_set_labels =
      self$v_to_psl_with_psn(variables, partition_set_name)

    variables[all_partition_set_labels %in% partition_set_labels, , drop = FALSE]
  }
  self$filter_match = function(variables
      , partition_set_name
      , partition_set_labels
    ) {
    if (is.null(partition_set_name)) return(variables)
    all_partition_set_labels =
      self$v_to_psl_with_psn(variables, partition_set_name)

    #variables[all_partition_set_labels %in% partition_set_labels, , drop = FALSE]
    variables[match(partition_set_labels, all_partition_set_labels), , drop = FALSE]
  }

  self$filter_blanks_match_everything = function(variables
    , partition_set_name
    , partition_set_labels
  ) {
    full_set = self$v_to_ps_with_psn(variables, partition_set_name)
    target_set = self$.names_inverse(partition_set_labels, self$psn_to_pn(partition_set_name))
    filtered_set = variables[apply(mapply(`%in%`, full_set, target_set) | sapply(full_set, `==`, ""), 1L, all), , drop = FALSE]
    filtered_set
  }


  return_object(self, "Namer")
}
