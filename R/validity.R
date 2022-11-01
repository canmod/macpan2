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
  )
)
