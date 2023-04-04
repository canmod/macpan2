name_starts_with = function(prefix) function(x) x[startsWith(names(x), prefix)]
name_ends_with = function(prefix) function(x) x[endsWith(names(x), prefix)]
extract_with_name = function(name) function(x) x[[name]]
subset_with_names = function(names) function(x) x[names]
starts_with_this = function(prefix) function(x) startsWith(x, prefix)
ends_with_this = function(prefix) function(x) endsWith(x, prefix)
is_opt_par_name = function(x) grepl("(p|r)_(par|mat|row|col)_id", x)
are_matrix_list_names = function(x) {
  all(grepl("^([A-Za-z.]{1}[A-Za-z0-9_.]*|)$", x) | (x == ""), na.rm = FALSE)
}
init_el_valid_pars = function(tmb_model) {
  par_dim_summary = merge(
    tmb_model$.params$data_frame(),
    tmb_model$.init_mats$mat_dims(),
    all.x = TRUE
  )
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

#' @importFrom oor ValidityMessager
#' @importFrom oor All Any Not Is
#' @importFrom oor TestTrue TestFalse
#' @importFrom oor TestPipeline Summarizer
#' @importFrom oor MappedAllTest MappedAnyTest MappedSummarizer
#' @importFrom oor TestHomo TestRange TestSubset
#' @importFrom oor Base Unclean return_object return_facade
valid <- list(
  char = ValidityMessager(is.character, "not a character vector"),
  char1 = ValidityMessager(
    All(
      is.character,
      TestPipeline(Summarizer(length), TestRange(1L, 1L))
    ),
    "not a length-1 character vector"
  ),
  char_no_empty = ValidityMessager(
    All(
      is.character,
      TestPipeline(Summarizer(is.na, any, `!`), TestTrue()),
      TestPipeline(Summarizer(nchar), MappedAllTest(TestRange(1, Inf)))
    ),
    "missing stuff"
  ),
  named_list = ValidityMessager(
    All(
      is.list,
      TestPipeline(Summarizer(names, is.null, any), TestFalse()),
      TestPipeline(Summarizer(names, duplicated, any), TestFalse()),
      TestPipeline(Summarizer(names, nchar), TestRange(1, Inf))
    ),
    "not a list with unique names that are neither blank nor null"
  ),
  logic = ValidityMessager(
    is.logical, "not a logical vector"
  ),
  num_mat = ValidityMessager(
    All(is.numeric, is.matrix),
    "not a numeric matrix"
  ),
  num_vec = ValidityMessager(
    All(
      is.numeric,
      TestPipeline(Summarizer(dim, is.null), TestTrue())
    ),
    "not a numeric vector"
  ),
  list_char_eq_len = ValidityMessager(
    All(
      is.list,
      MappedAllTest(is.character),
      TestPipeline(MappedSummarizer(length), TestHomo())
    ),
    "not all character vectors of equal length"
  ),
  math = ValidityMessager(
    Is("MathExpression"),
    "not an object of class MathExpression"
  ),
  edge = ValidityMessager(
    Is("EdgeModel"),
    "not an edge model"
  ),
  func = ValidityMessager(
    is.function,
    "not a function"
  ),
  name_or_num = ValidityMessager(
    Any(is.name, is.numeric),
    "neither name nor number"
  ),
  expr_arg = ValidityMessager(
    All(
      TestPipeline(
        Summarizer(length),
        TestRange(7L, 7L)
      ),
      TestPipeline(
        Summarizer(names),
        TestSubset(c(
          "expr_output_id", "expr_sim_block", "expr_num_p_table_rows",
          "eval_schedule", "p_table_x", "p_table_n", "p_table_i"
        ))
      ),
      TestPipeline(
        Summarizer(name_starts_with("expr_")),
        MappedSummarizer(length),
        TestHomo()
      ),
      TestPipeline(
        Summarizer(name_starts_with("p_")),
        MappedSummarizer(length),
        TestHomo()
      ),
      TestPipeline(
        Summarizer(extract_with_name("eval_schedule"), length),
        TestRange(3L, 3L)
      ),
      MappedAllTest(is.integer),
      TestPipeline(
        Summarizer(extract_with_name("p_table_x")),
        TestRange(0L, Inf)
      ),
      TestPipeline(
        Summarizer(extract_with_name("p_table_n")),
        TestRange(-1L, Inf)
      ),
      TestPipeline(
        Summarizer(extract_with_name("p_table_i")),
        TestRange(-1L, Inf)
      ),
      TestPipeline(
        Summarizer(extract_with_name("expr_output_id")),
        TestRange(0L, Inf)
      ),
      TestPipeline(
        Summarizer(extract_with_name("expr_sim_block")),
        TestRange(0L, 1L)
      ),
      TestPipeline(
        Summarizer(extract_with_name("expr_num_p_table_rows")),
        TestRange(1L, Inf)
      )
    )
  ),
  tmb_model = ValidityMessager(
    init_el_valid_pars,
    "TMB model is not valid"
  ),
  mats_list = ValidityMessager(
    All(
      MappedAllTest(is.matrix),
      MappedAllTest(is.numeric),
      TestPipeline(
        Summarizer(names, are_matrix_list_names),
        TestTrue()
      )
    )
  ),
  mats_arg = ValidityMessager(
    All(
      TestPipeline(
        Summarizer(length),
        TestRange(3L, 3L)
      )
      , TestPipeline(
        Summarizer(names),
        TestSubset(c(
          "mats", "mats_save_hist", "mats_return"
        ))
      )
      , TestPipeline(
        MappedSummarizer(length),
        TestHomo()
      )
      , TestPipeline(
        Summarizer(extract_with_name("mats")),
        MappedAllTest(is.matrix)
      )
      , TestPipeline(
        Summarizer(extract_with_name("mats")),
        MappedAllTest(is.numeric)
      )
      , TestPipeline(
        Summarizer(subset_with_names(c("mats_save_hist", "mats_return"))),
        MappedAllTest(is.logical)
      )
    )
  ),
  opt_params_list_arg = ValidityMessager(
    All(
      TestPipeline(
        Summarizer(length),
        TestRange(4L, 4L)
      ),
      TestPipeline(
        Summarizer(names, is_opt_par_name, all),
        TestTrue()
      ),
      MappedAllTest(is.integer),
      TestPipeline(
        MappedSummarizer(length),
        TestHomo()
      ),
      MappedAllTest(TestRange(0L, Inf))
    )
  ),
  obj_fn_arg = ValidityMessager(
    All(
      TestPipeline(
        Summarizer(name_starts_with("p_")),
        MappedSummarizer(length),
        TestHomo()
      ),
      TestPipeline(
        Summarizer(length),
        TestRange(7L, 7L)
      ),
      TestPipeline(
        Summarizer(names),
        TestSubset(c(
          "expr_output_id", "expr_sim_block", "expr_num_p_table_rows",
          "eval_schedule", "p_table_x", "p_table_n", "p_table_i"
        ))
      )
    )
  )
)
