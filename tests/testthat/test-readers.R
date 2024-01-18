test_that("attempts to construct abstract readers give appropriate instructions", {
  tmp_file = function(ext) {
    f = tempfile(fileext = ext)
    file.create(f)
    f
  }
  expect_error(
    Reader(tmp_file(".csv"))$read(),
    regexp = "Please try a specific reader like CSVReader"
  )
  expect_error(
    Reader(tmp_file(".json"))$read(),
    "Please try a specific reader like JSONReader"
  )
  expect_error(
    Reader(tmp_file(".txt"))$read(),
    "Please try a specific reader like TXTReader"
  )
})

test_that("txt reader works", {
  tmp_file = function() {
    f = tempfile(fileext = ".txt")
    writeLines(c("testing", "testing", "1, 2, 3 ..."), f)
    f
  }
  expect_identical(
    TXTReader(tmp_file())$read(),
    c("testing", "testing", "1, 2, 3 ...")
  )
})
