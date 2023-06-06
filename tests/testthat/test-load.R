testthat::test_that("load.label_colname works", {
  testdata <- system.file("extdata","tool.xlsx", package = "utilityR")
  actual_output <- load.label_colname(testdata)
  testthat::expect_equal(actual_output,"label::English")
})
