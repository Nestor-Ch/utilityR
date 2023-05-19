testthat::test_that("equality works", {

  testdf <- data.frame(a = c(1,2),
                       b = c(1,3))
  expected_output <- data.frame(a = c(1,2),
                                b = c(1,3),
                                c = c(TRUE,FALSE))

  testthat::expect_equal(testdf %>%
                 dplyr::mutate(c = a %==% b), expected_output)


})


testthat::test_that("inequality works", {
  testdf <- data.frame(a = c(1,2),
                       b = c(1,3))
  expected_output <- data.frame(a = c(1,2),
                                b = c(1,3),
                                c = c(FALSE,TRUE))

  testthat::expect_equal(testdf %>%
                           dplyr::mutate(c = a %!=% b), expected_output)
})

