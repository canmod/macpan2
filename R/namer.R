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
    read.table(text = names
      , sep = "."
      , col.names = partition_names
      , na.strings = character()
      , colClasses = "character"
    )
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

  return_object(self, "NameAndLabelUtilities")
}

#' Name and Label Translator
#'
#' Construct an object with methods for manipulating the names and labels
#' associated with model variables.
#'
#' Names refer the columns of the variables list, and labels refer to the atomic
#' elements of this list. A partition set is a (possibly improper but not empty) subset and ordering of
#' the columns of the variables list. Note that the variables list itself is a
#' partition set because this definition does not require a proper subset to be
#' taken. A filtered partition set is a non-empty subset of the rows in a partition set.
#' An identifying partition set is a (possibly filtered) partition set with no
#' repeated rows. The required partition set is an unfiltered and identifying
#' partition set, containing the columns listed in the `required_partition`
#' component of the `settings` file.
#'
#' Vectors of both names and labels can be dot-concatenated to produce a single
#' string providing an alternative representation of the vector. We distinguish
#' between dotted and undotted strings. Only vectors of undotted strings can be
#' dot-concatenated. Dot-concatenation can be used to refer to different aspects
#' of a partition set. The vector of names of the columns of a partition set can
#' be dot-concatenated to produce a single dotted string representing a single
#' name for the partition set. Each row of labels in a partition set can be
#' dot-concatenated to produce a single dotted string that provides a single
#' label for that row. The collection of rows can therefore be represented as a
#' vector of dotted strings giving the names of the rows.
#'
#' We develop a systematic approach to name methods for converting between these
#' different representations and subsets. This approach is based on assigning
#' letters to different cases.
#'
#' * Dotted versus undotted
#'     * `u` for undotted
#'     * `d` for dotted
#' * Names versus labels
#'     * `n` for names
#'     * `l` for labels
#' * Single string versus vector of strings versus a data frame (i.e.
#' collection) of rows of strings
#'     * `s` for a single string
#'     * `v` for a vector of strings
#'     * `c` for a data frame (i.e. collection)
#'
#' With this lettering scheme we can characterize different entities. For
#' example, `dns` denotes a partition `n`ame represented as a `d`otted `s`tring.
#' Some of the combinations do not occur, but that is fine. For example,
#' collections are never dotted because we just have not found a need to do so.
#' Note also that length-one vectors can potentially be thought of as single
#' strings, but the context will typically make it clear which is more
#' appropriate. An analogous ambiguity occurs with dotted strings that happen
#' to contain zero dots.
#'
#' * `uns` -- The name of a single column in the variables list.
#' * `uls` -- The label of a single element in the variables list.
#' * `unv`, `dns` -- Alternative representations of the names of several columns
#' in the variables list.
#' * `ulv`, `dls` -- Alternative representations of the labels of several
#' elements (i.e. a row, column or other subset of elements) in the variables
#' list.
#' * `ulc`, `dlv` -- Alternative representations of a possibly improper subset
#' of the rows/columns of the variables list.
#' * `dnv` -- List of subsets of the columns in the variables list.
#' * `unc` -- Unused because collections (i.e. data frames) need names for their
#' columns and names do not have names in our scheme.
#' * `dnc`, `dlc` -- Unused because collections are represented by data frames,
#' which have natural row and column separation making it unnecessary to
#' include dot-separation.
#'
#' With this scheme we can name methods of translation between the alternative
#' representations listed above. For example, the methods `unv_to_dns` and
#' `dns_to_unv` translate between two alternative representations of the names
#' of several columns in the variables list. When translating between
#' representations of labels we need to specify what names should be used for
#' their interpretation. For example, the method `dlv_to_ulc_wrt_unv` converts
#'
#' This scheme can also be used to denote methods for subsetting names and
#' labels.
#'
#'
#' * `v` -- variables
#' * `rpn` -- required_partition_name -- required partition set
#' * `vn` -- variable_names -- partition_set_labels with respect to the required partition_set
#' * `pl` -- partition_labels -- vector representing one column of the variables list
#' * `pn` -- partition_names -- names of an ordered subset of the columns of the variables list
#' * `ps` -- partition_set -- ordered subset of the columns of the variables list
#' * `psn` -- partition_set_name -- dot-delimited concatenation of the names of the columns in a partition_set
#' * `psl` -- partition_set_labels -- dot-delimited concatenation of the partition_labels in a partition_set
#'
#' @noRd
TranslatorUtilities = function() {
  self = NameAndLabelUtilities()

  # `unv` <-> `dns` and `ulv` <-> `dls`
  self$uv_to_ds = function(uv) self$.dot(uv)
  self$ds_to_uv = function(ds) {
    unlist(strsplit(ds, "\\."), use.names = FALSE)
  }

  self$ulc_to_dlv = function(ulc) {
    mat = self$.mat(ulc)
    apply(mat, 1L, self$.dot)
  }
  self$dlv_to_ulc_by_unv = function(dlv, unv) {
    list_of_strings_as_vectors = strsplit(dlv, '')
    n_dots = vapply(
      lapply(list_of_strings_as_vectors, `==`, "."),
      sum,
      integer(1L)
    )
    mat = vapply(dlv, self$ds_to_uv, character(n_dots + 1L))
    setNames(
      as.data.frame(mat),
      unv
    )
  }
  self$dlv_to_ulc_by_dns = function(dlv, dns) {
    self$dlv_to_ulc_by_unv(dlv, self$ds_to_uv(dns))
  }

  return_object(self, "Translator")
}


#' Coordinate Change Utilities
#'
#' Construct an object with methods for changing the coordinate system
#' being used to identify model variables and sets of model variables.
#'
#' The \code{\link{TranslatorUtilities}} help page describes a difference
#' between names and labels. Specifically, names refer to the columns
#' of a variables list and labels refer to the elements of that list.
#' Another way to understand the difference between names and labels is that
#' Labels provide coordinates for identifying model variables and names
#' provide coordinate systems for interpreting these coordinates. For example,
#' consider a variable with dotted label string given by `"S.S"`. This looks
#' like a susceptible category in a multi-strain or multiple disease model, but
#' what disease does each `"S"` refer to? An associated dotted label name of
#' `"Covid.Flu"` would would clear up this question.
#'
#' We consider two kinds of coordinate change -- rotation and projection. A
#' rotation is just a re-ording of the names. For example, consider the label
#' `"S.E"` indicating susceptibility to COVID-19 and exposure to influenza in
#' the `"Covid.Flu"` coordinate system. With this example, we could rotate to
#' `"Flu.Covid"`, and in this new coordinate system we would write `"S.E"` as
#' `"E.S"`. A projection is a removal of coordinates. Continuing the example,
#' we could project `"S.E"` with respect to `"Covid.Flu"` onto `"Covid"` to
#' get `"S"`, denoting all variables denoting compartments with individuals
#' who are susceptible to COVID-19. Note also that we can have a rotation
#' and a projection at the same time by removing a set of names (projection)
#' and shuffling the order of the remaining names (rotation). For example,
#' working in a basis of `"Alpha.Beta.Omicron"` representing COVID-19 strains,
#' we could refer to all compartments with individuals who have recovered from
#' alpha but are susceptible to omicron as `"S.R"` with respect to
#' the rotated projection `"Omicron.Alpha"` of the original coordinate system.
#'
#' All of the methods in this class have names of the form `.l._by_.n.` where
#' the dots can be replaced by letters indicating the representation of labels
#' and names. For example, `ulv_by_unv` is a method for taking an `u`ndotted
#' `l`abel `v`ector and changing its coordinates in terms of an `u`ndotted
#' `n`ame `v`ector. Most of the methods require that both the initial
#' and target coordinate systems are provided, but two do not: `ulc_by_unv`
#' and `ulc_by_dns`. These methods are for the `ulc` label format which comes
#' bundled with its coordinate system as column names. This is the format of
#' the `variables.csv` file in the model definition.
#'
#' @noRd
CoordinateChangeUtilities = function() {
  self = NameAndLabelUtilities()
  self$translate = TranslatorUtilities()

  self$ulv_by_unv = function(ulv, unv, target_unv) {
    illegal_names = setdiff(target_unv, unv)
    if (length(illegal_names) != 0L) {
      stop(
        "\nThe following names were in the target but not in the original basis: \n",
        paste(illegal_names, collapse = "\n")
      )
    }
    unname(setNames(ulv, unv)[target_unv])
  }
  self$ulv_by_dns = function(ulv, dns, target_dns) {
    self$ulv_by_unv(
      ulv,
      self$translate$ds_to_uv(dns),
      self$translate$ds_to_uv(target_dns)
    )
  }
  self$dls_by_unv = function(dls, unv, target_unv) {
    self$translate$uv_to_ds(
      self$ulv_by_unv(
        self$translate$ds_to_uv(dls),
        unv,
        target_unv
      )
    )
  }
  self$dls_by_dns = function(dls, dns, target_dns) {
    self$translate$uv_to_ds(
      self$ulv_by_unv(
        self$translate$ds_to_uv(dls),
        self$translate$ds_to_uv(dns),
        self$translate$ds_to_uv(target_dns)
      )
    )
  }
  self$dlv_by_unv = function(dlv, unv, target_unv) {
    self$translate$ulc_to_dlv(
      self$ulc_by_unv(
        self$translate$dlv_to_ulc_by_unv(dlv, unv),
        target_unv
      )
    )
  }
  self$dlv_by_dns = function(dlv, dns, target_dns) {
    self$dlv_by_unv(
      dlv,
      self$translate$ds_to_uv(dns),
      self$translate$ds_to_uv(target_dns)
    )
  }
  self$ulc_by_unv = function(ulc, target_unv) {
    illegal_names = setdiff(target_unv, names(ulc))
    if (length(illegal_names) != 0L) {
      stop(
        "\nThe following names were not in the target but not in the original basis: \n",
        paste(illegal_names, collapse = "\n")
      )
    }
    ulc[, target_unv, drop = FALSE]
  }
  self$ulc_by_dns = function(ulc, target_dns) {
    self$ulc_by_unv(
      ulc,
      self$translate$ds_to_uv(target_dns)
    )
  }

  return_object(self, "BasisChangeUtilities")
}


#' @noRd
FilterUtilities = function() {
  self = NameAndLabelUtilities()
  self$translate = TranslatorUtilities()
  self$change_coordinates_of = CoordinateChangeUtilities()

  self$always_match_blanks = list()
  self$exact = list(
    dlv = function(dlv, dns, filtering_dlv, filtering_dns) {
      if (is.null(filtering_dns)) return(dlv)
      dlv_wrt_filtering_dns = self$change_coordinates_of$dlv_by_dns(dlv
        , dns
        , filtering_dns
      )
      dlv[dlv_wrt_filtering_dns %in% filtering_dlv]
    },
    ulc = function(ulc, filtering_ulc) {
      self$translate$dlv_to_ulv_by_unv(
        self$exact$dlv(
          self$translate$ulc_to_dlv(ulc),
          self$translate$unv_to_dns(names(ulc)),
          self$translate$ulc_to_dlv(filtering_ulc),
          self$translate$unv_to_dns(names(filtering_ulc))
        ),
        names(ulc)
      )
    }
  )

  return_object(self, "BasisChangeUtilities")
}

#' @noRd
NameAndLabels = function() {
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
  self$psl_to_vn = function(partition_set_labels
      , partition_set_name
      , required_partition_set_name
    ) {
    self$ps_to_psl(
      self$.names_full_inverse(
        partition_set_labels,
        self$psn_to_pn(partition_set_name),
        self$psn_to_pn(required_partition_set_name)
      )
    )

  }
  return_object(self, "NameAndLabelTranslator")
}

FilterTraits = function() {
  self = NameAndLabels()
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
#' @noRd
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

#' Filter Matching Blanks to Everything
#'
#' @noRd
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

#' Filter to a Unique and Sorted Partition Set
#'
#' @noRd
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
  self = NameAndLabels()

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
