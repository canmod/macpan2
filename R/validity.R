name_starts_with = function(prefix) function(x) x[startsWith(names(x), prefix)]
name_ends_with = function(prefix) function(x) x[endsWith(names(x), prefix)]
run_no_op_method = function(method_name) function(x) x[[method_name]]()
extract_with_name = function(name) function(x) x[[name]]
subset_with_names = function(names) function(x) x[names]
starts_with_this = function(prefix) function(x) startsWith(x, prefix)
ends_with_this = function(prefix) function(x) endsWith(x, prefix)

is_opt_par_name = function(x) {
  is_good = grepl("(p|r)_(par|mat|row|col)_id", x)
  all_good = all(is_good, na.rm = FALSE)
  if (!all_good) {
    message(
      "\nThe following components have names that do not follow the rules:\n",
      paste0(x[!is_good], collapse = "\n")
    )
    return(FALSE)
  }
  TRUE
}

are_matrix_list_names = function(x) {
  is_good = grepl("^([A-Za-z.]{1}[A-Za-z0-9_.]*|)$", x) | (x == "")
  all_good = all(is_good, na.rm = FALSE)
  if (!all_good) {
    message(
      "\nThe following matrices have names that do not follow the rules:\n",
      paste0(x[!is_good], collapse = "\n")
    )
    return(FALSE)
  }
  TRUE
}

## ΤΟDO: make this a wrapper for a function that takes a params data frame
## and matrix dimensions and checks consistency, so that consistency can be
## checked before updating the params in a tmb_model
p_or_r_consistency_opt_mat = function(parameter_type = c("params", "random")) {
  parameter_type = match.arg(parameter_type)
  function(tmb_model) {
    pf = tmb_model[[parameter_type]]$data_frame()
    pd = tmb_model$init_mats$mat_dims()
    invalid_pnms = setdiff(pf$mat, pd$mat)
    if (length(invalid_pnms) > 0) {
      sprintf(
          "The following parameters:\n     %s\nAre not in the following list of valid model parameters:\n     %s\n"
        , paste0(invalid_pnms, collapse = ", ")
        , paste0(unique(pd$mat), collapse = ", ")
      ) |> stop()
    }
    par_dim_summary = merge(pf, pd, all.x = TRUE)
    valid_pars = with(par_dim_summary, (row < nrow) & (col < ncol))
    if (any(!valid_pars)) {
      stop(
        "\nMatrices involved in model parameterization must contain the right",
        "\nrows and columns within which to insert each parameter.",
        "\nHowever, the following matrix-parameter pairs fail this test:\n",
        frame_formatter(par_dim_summary[!valid_pars, , drop = FALSE])
      )
    }
    TRUE
  }
}

check_auto_component_names = function(all_names, auto_vec_name, component_names) {
  clash_names = component_names[component_names %in% all_names]
  if (length(clash_names) > 0) {
    stop(
      sprintf(
        "\nThe following components of the %s vector are getting used as the",
        auto_vec_name
      ),
      "\nnames of matrices. This is not allowed because it could create",
      "\nnaming ambiguity in some situations. Please rename the following",
      "\nvariables.\n",
      paste0(clash_names, collapse = "\n")
    )
  }
}

check_auto_names = function(all_names, ...) {
  auto_names = (list(...)
    |> lapply(as.character)
    |> unlist(recursive = FALSE, use.names = FALSE)
  )
  clash_names = auto_names[auto_names %in% all_names]
  if (length(clash_names) > 0) {
    stop(
      "\nThe following automatically generated names are already used in the model.",
      "\nPlease use different names so that these names are available.\n",
      paste0(clash_names, collapse = "\n")
    )
  }
}

AllValid = function(..., .msg = "") {
  self = Base()
  self$valid_dots = ValidityMessager(
    MappedAllTest(Is("ValidityMessager")),
    "bug with the following validity check",
    .msg,
    "please open canmod/macpan2 github issue"
  )
  self$message = .msg
  self$validity_objects = self$valid_dots$assert(list(...))
  self$assert = function(x) {
    self$check(x)
    x
  }
  self$check = function(x) {
    for (valid in self$validity_objects) {
      test_result = try(valid$check(x), silent = TRUE)
      if (!isTRUE(test_result)) {
        message(.msg)
        stop(test_result)
      }
    }
    TRUE
  }
  self$is_true = function(x) {
    for (valid in self$validity_objects) {
      if (!valid$is_true(x)) return(FALSE)
    }
    TRUE
  }
  return_object(self, "AllValid")
}

#' @importFrom oor ValidityMessager
#' @importFrom oor All Any Not Is
#' @importFrom oor TestTrue TestFalse
#' @importFrom oor TestPipeline Summarizer
#' @importFrom oor MappedAllTest MappedAnyTest MappedSummarizer
#' @importFrom oor TestHomo TestRange TestSubset
#' @importFrom oor Base Unclean return_object return_facade
BaseTypeChecking = function() {
  self = Base()
  self$char = ValidityMessager(is.character, "not a character vector")
  self$numeric = ValidityMessager(is.numeric, "not numeric")
  self$int = ValidityMessager(is.integer, "not integer")
  self$list = ValidityMessager(is.list, "not a list")
  self$matrix = ValidityMessager(is.matrix, "not a matrix")
  self$logic = ValidityMessager(is.logical, "not a logical vector")
  self$func = ValidityMessager(is.function, "not a function")
  self$name = ValidityMessager(is.name, "not a name")
  return_object(self, "BaseTypeChecking")
}
CustomTypeChecking = function() {
  self = BaseTypeChecking()

  ## custom type checking
  self$math = ValidityMessager(
    Is("MathExpression"),
    "not an object of class MathExpression"
  )
  self$edge = ValidityMessager(
    Is("EdgeModel"),
    "not an edge model"
  )
  self$name_or_num = ValidityMessager(
    Any(is.name, is.numeric),
    "neither name nor number"
  )
  return_object(self, "CustomTypeChecking")
}
HomoTypeChecking = function() {
  self = CustomTypeChecking()
  ## base homogeneous type checking
  self$all_matrices = ValidityMessager(
    MappedAllTest(is.matrix),
    "not all components are matrices"
  )
  self$all_numeric = ValidityMessager(
    MappedAllTest(is.numeric),
    "not all components are numeric"
  )
  self$char_list = ValidityMessager(
    All(is.list, MappedAllTest(is.character)),
    "not a list of character vectors"
  )
  self$all_int = ValidityMessager(
    MappedAllTest(is.integer),
    "not all components are integer vectors"
  )
  return_object(self, "HomoTypeChecking")
}
BasePropertyChecking = function() {
  self = HomoTypeChecking()
  self$no_missing = ValidityMessager(
    TestPipeline(Summarizer(is.na, any, `!`), TestTrue()),
    "missing some values"
  )
  self$scalar = ValidityMessager(
    TestPipeline(Summarizer(length), TestRange(1L, 1L)),
    "not a scalar (i.e. not length-1)"
  )
  self$no_blank = ValidityMessager(
    TestPipeline(Summarizer(nchar), MappedAllTest(TestRange(1, Inf))),
    "some blank strings"
  )
  self$no_null_names = ValidityMessager(
    TestPipeline(Summarizer(names, is.null, any), TestFalse()),
    "some null names"
  )
  self$no_dup_names = ValidityMessager(
    TestPipeline(Summarizer(names, duplicated, any), TestFalse()),
    "some duplicated names"
  )
  self$no_blank_names = ValidityMessager(
    TestPipeline(Summarizer(names, nchar), TestRange(1, Inf)),
    "some blank names"
  )
  self$no_dims = ValidityMessager(
    TestPipeline(Summarizer(dim, is.null), TestTrue()),
    "dimensions are present"
  )
  return_object(self, "BasePropertyChecking")
}
HomoPropertyChecking = function() {
  self = BasePropertyChecking()
  ## base homogeneous property checking
  self$eq_len = ValidityMessager(
    TestPipeline(MappedSummarizer(length), TestHomo()),
    "not an object containing equal length components"
  )
  return_object(self, "HomoPropertyChecking")
}
BaseValidity = function() {
  self = HomoPropertyChecking()
  # compound validity -- failed checks will reference
  # the specific reason for the failure
  self$char1 = AllValid(
    self$char,
    self$scalar,
    .msg = "not a string (i.e. not a length-1 character vector)"
  )
  self$num1 = AllValid(
    self$numeric,
    self$scalar,
    .msg = "not a number (i.e. not a length-1 numeric vector)"
  )
  self$int1 = AllValid(
    self$int,
    self$scalar,
    .msg = "not an integer (i.e. not a length-1 integer vector)"
  )
  self$char_no_empty = AllValid(
    self$char,
    self$no_missing,
    self$no_blank,
    .msg = "missing stuff"
  )
  self$named_list = AllValid(
        self$list
      , self$no_null_names
      , self$no_dup_names
      , self$no_blank_names
      , .msg = "not a list with unique names that are neither blank nor null"
  )
  self$num_mat = AllValid(
    self$numeric,
    self$matrix,
    .msg = "not a numeric matrix"
  )
  self$num_vec = AllValid(
    self$numeric,
    self$no_dims,
    .msg = "not a numeric vector"
  )
  self$list_char_eq_len = AllValid(
    self$char_list,
    self$eq_len,
    .msg = "not all character vectors of equal length"
  )
  return_object(self, "BaseValidity")
}

TMBConsistency = function() {
  ## consistency among TMBModel components
  self = BaseValidity()
  self$consistency_params_mats = ValidityMessager(
    p_or_r_consistency_opt_mat("params"),
    "optimization parameters are not consistent with matrices"
  )
  self$consistency_random_mats = ValidityMessager(
    p_or_r_consistency_opt_mat("random"),
    "random-effect parameters are not consistent with matrices"
  )
  self$tmb_model = AllValid(
    self$consistency_params_mats,
    self$consistency_random_mats,
    .msg = "TMB model components are not consistent"
  )
  return_object(self, "TMBConsistency")
}

TMBAdaptorValidity <- function() {
  self = TMBConsistency()
  # model component validity
  self$matrix_list_names = function() {
    ValidityMessager(
      TestPipeline(
        Summarizer(names, are_matrix_list_names),
        TestTrue()
      ),
      "invalid names for matrices passed to c++"
    )
  }
  self$n_components = function(n) {
    ValidityMessager(
      TestPipeline(
        Summarizer(length),
        TestRange(n, n)
      ),
      sprintf("not %s c++ components", n)
    )
  }
  self$component_names = function(...) {
    component_nms = as.character(unlist(list(...), recursive = TRUE, use.names = FALSE))
    ValidityMessager(
      TestPipeline(Summarizer(names), TestSubset(component_nms)),
      sprintf(
        "one or more of the following c++ components are not present,\n%s",
        paste0(component_nms, collapse = "\n")
      )
    )
  }
  self$homo_length_components = function(prefix, type) {
    ValidityMessager(
      TestPipeline(
        Summarizer(name_starts_with(prefix)),
        MappedSummarizer(length),
        TestHomo()
      ),
      sprintf("c++ components related to %s are not of equal length", type)
    )
  }
  self$all_homo_length = ValidityMessager(
    TestPipeline(
      MappedSummarizer(length),
      TestHomo()
    ),
    .msg = "not all components have the same length"
  )
  self$component_lengths = function(component, lower, upper) {
    ValidityMessager(
      TestPipeline(
        Summarizer(extract_with_name(component), length),
        TestRange(lower, upper)
      ),
      sprintf(
        "c++ component %s is either shorter than %s or longer than %s",
        component, lower, upper
      )
    )
  }
  self$component_ranges = function(component, lower, upper) {
    ValidityMessager(
      TestPipeline(
        Summarizer(extract_with_name(component)),
        TestRange(lower, upper)
      ),
      sprintf(
        "c++ component %s has some values either less than %s or greater than %s",
        component, lower, upper
      )
    )
  }
  self$component_is = function(component, type, is_func) {
    ValidityMessager(
      TestPipeline(
        Summarizer(extract_with_name(component)),
        MappedAllTest(is_func)
      ),
      sprintf("%s component is not of type %s", component, type)
    )
  }
  self$opt_par_names = ValidityMessager(
    TestPipeline(
      Summarizer(names, is_opt_par_name, all),
      TestTrue()
    ),
    "optimization parameter object has invalid names"
  )
  self$all_non_neg = ValidityMessager(
    MappedAllTest(TestRange(0L, Inf)),
    "not all components are non-negative"
  )

  # model component compound validity
  self$expr_arg = AllValid(
    self$n_components(10L),
    self$component_names(
      "expr_sim_block",
      "expr_num_p_table_rows", "assign_num_a_table_rows",
      "eval_schedule",
      "p_table_x", "p_table_n", "p_table_i",
      "a_table_x", "a_table_n", "a_table_i"
    ),
    self$homo_length_components("expr_", "expressions"),
    self$homo_length_components("p_", "the expression parse table"),
    self$homo_length_components("a_", "the assignment parse table"),
    self$all_int,
    self$component_lengths("eval_schedule", 3L, 3L),
    self$component_ranges("p_table_x", 0L, Inf),
    self$component_ranges("p_table_n", -3L, Inf),
    self$component_ranges("p_table_i", -1L, Inf),
    self$component_ranges("a_table_x", 0L, Inf),
    self$component_ranges("a_table_n", -3L, Inf),
    self$component_ranges("a_table_i", -1L, Inf),
    #self$component_ranges("expr_output_id", 0L, Inf),
    self$component_ranges("expr_sim_block", 0L, 1L),
    self$component_ranges("expr_num_p_table_rows", 1L, Inf),
    self$component_ranges("assign_num_a_table_rows", 1L, Inf),
    .msg = "expression information passed to c++ is not valid"
  )

  self$mats_list = AllValid( ## never called! this means that matrices are not yet checked for name validity
    self$all_matrices,
    self$all_numeric,
    self$matrix_list_names(),
    .msg = "matrices passed to c++ are not all valid"
  )
  self$mats_arg = AllValid(
    self$n_components(3L),
    self$component_names("mats", "mats_save_hist", "mats_return"),
    self$homo_length_components("mats", "matrices"),
    self$component_is("mats", "matrix", is.matrix),
    self$component_is("mats", "numeric", is.numeric),
    self$component_is("mats_save_hist", "logical", is.logical),
    self$component_is("mats_return", "logical", is.logical),
    .msg = "matrix information passed to c++ is not valid"
  )
  self$opt_params_list_arg = AllValid(
    self$n_components(4L),
    self$opt_par_names,
    self$all_int,
    self$all_homo_length,
    self$all_non_neg,
    .msg = "optimization parameter information passed to c++ is not valid"
  )
  self$obj_fn_arg = AllValid(
    self$homo_length_components("p_", "parse tables"),
    self$n_components(7L),
    self$component_names(
      "expr_output_id", "expr_sim_block", "expr_num_p_table_rows",
      "eval_schedule", "p_table_x", "p_table_n", "p_table_i"
    ),
    .msg = "objective function information passed to c++ is not valid"
  )

  ## dynamic message generation
  self$engine_outputs = function(all_names) {
    ValidityMessager(
      TestSubset(all_names),
      "some expressions are saved to matrices that are not initialized"
    )
  }
  return_object(self, "TMBAdaptorValidity")
}

valid = TMBAdaptorValidity()
