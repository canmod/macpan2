test_that("frame formatter produces correct string for catting", {
  expect_identical(
    macpan2:::frame_formatter(data.frame(aa = letters[1:2], zz = letters[26:25])),
    "aa  zz\n--  --\na   z \nb   y "
  )
})
