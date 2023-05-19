testthat::test_that("class", {
  expected <- "character"
  testthat::expect_equal(class(intro()), expected)
})
