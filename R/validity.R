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
  logic = ValidityMessager(
    is.logical, "not a logical vector"
  ),
  num_mat = ValidityMessager(
    All(is.numeric, is.matrix),
    "not a numeric matrix"
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
