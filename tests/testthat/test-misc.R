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
                           dplyr::mutate(c = a %not=% b), expected_output)
})


testthat::test_that("A greater than b works", {
  testdf <- data.frame(a = c(1,2,3,4,5, NA),
                       b = c(5,4,2,3,10, 2))
  expected_output <- data.frame(a = c(1,2,3,4,5,NA),
                                b = c(5,4,2,3,10,2),
                                c = c(FALSE,FALSE,TRUE,TRUE,FALSE, FALSE))

  testthat::expect_equal(testdf %>%
                           dplyr::mutate(c = a %_>_% b), expected_output)
})

testthat::test_that("A greater than or equal to b works", {
  testdf <- data.frame(a = c(1,2,3,4,5,NA),
                       b = c(5,4,2,4,10, 11))
  expected_output <- data.frame(a = c(1,2,3,4,5,NA),
                                b = c(5,4,2,4,10,11),
                                c = c(FALSE,FALSE,TRUE,TRUE,FALSE, FALSE))

  testthat::expect_equal(testdf %>%
                           dplyr::mutate(c = a %_>=_% b), expected_output)
})


testthat::test_that("A less than b works", {
  testdf <- data.frame(a = c(1,2,3,4,5,NA),
                       b = c(5,4,2,3,10,2))
  expected_output <- data.frame(a = c(1,2,3,4,5,NA),
                                b = c(5,4,2,3,10,2),
                                c = c(TRUE,TRUE,FALSE,FALSE,TRUE, FALSE))

  testthat::expect_equal(testdf %>%
                           dplyr::mutate(c = a %_<_% b), expected_output)
})

testthat::test_that("A less than or equal to b works", {
  testdf <- data.frame(a = c(1,2,3,4,5,NA),
                       b = c(5,4,2,4,10,11))
  expected_output <- data.frame(a = c(1,2,3,4,5,NA),
                                b = c(5,4,2,4,10,11),
                                c = c(TRUE,TRUE,FALSE,TRUE,TRUE, FALSE))

  testthat::expect_equal(testdf %>%
                           dplyr::mutate(c = a %_<=_% b), expected_output)
})

testthat::test_that("Numeric sum works", {
  testdf <- data.frame(a = c(1,2,3,NA),
                     b = c(1,2,3,11))
  expected_output <- data.frame(a = c(1,2,3,NA),
                              b = c(1,2,3,11),
                              c = c(2,4,6,NA))
  # check if warning is produced when a character value is transformed into na
  testthat::expect_warning( 1 %_+_% 'b')
  # check if the function itself works properly
  testthat::expect_equal(testdf %>%
                         dplyr::mutate(c = a %_+_% b), expected_output)
  # check if the sum produces a numerical column
  testthat::expect_equal(testdf %>%
                           dplyr::mutate(c = a %_+_% b)%>%
                           na.omit()%>%
                           dplyr::pull(c)%>%
                           class(), 'numeric')

})



testthat::test_that("Equality of 2 variables works", {
  testdf <- data.frame(a = c(1,2,3,NA,'a','b','f'),
                       b = c(1,2,3,11,'a','c',NA))
  expected_output <- data.frame(a = c(1,2,3,NA,'a','b','f'),
                                b = c(1,2,3,11,'a','c',NA),
                                c = c(TRUE,TRUE,TRUE,FALSE,TRUE,FALSE,FALSE))
  # check if the function itself works properly
  testthat::expect_equal(testdf %>%
                           dplyr::mutate(c = a %==na% b), expected_output)
  # check if the function produces a boolean vector column
  testthat::expect_equal(testdf %>%
                           dplyr::mutate(c = a %==na% b)%>%
                           dplyr::pull(c)%>%
                           class(), 'logical')

})

testthat::test_that("Check variables are non-equal and not NA works", {
  testdf <- data.frame(a = c(1,NA,3,NA,'a',NA,'f'),
                       b = c(1,2,NA,11,'a',NA,NA))
  expected_output <- data.frame(a = c(1,NA,3,NA,'a',NA,'f'),
                                b = c(1,2,NA,11,'a',NA,NA),
                                c = c(FALSE,TRUE,TRUE,TRUE,FALSE,FALSE,TRUE))
  # check if the function itself works properly
  testthat::expect_equal(testdf %>%
                           dplyr::mutate(c = a %not=na% b), expected_output)
  # check if the function produces a boolean vector column
  testthat::expect_equal(testdf %>%
                           dplyr::mutate(c = a %not=na% b)%>%
                           dplyr::pull(c)%>%
                           class(), 'logical')

})



testthat::test_that("Check that collapsing works multiple options into 1 works", {
  a <-  c(1,2,'a','b','testing', NA,"\\NULL", TRUE, "\\b",NaN,"[[:punct:]]")
  expected_output_a <- "(1)|(2)|(a)|(b)|(testing)|(NA)|(\\NULL)|(TRUE)|(\\b)|(NaN)|([[:punct:]])"
  testthat::expect_equal( anychoice_pattern(a), expected_output_a)
})



testthat::test_that("Check that transposal of dataframes works", {

#Check for matching for the standard case of mixed character/numeric columns
mock_df <- data.frame(col1 = letters[1:5], col2 = 1:5, col3 = 5:1, row.names = paste0(letters[1:5], 11:15))
expected_df <- data.frame(rowname = paste0('col',1:3),
                          a11 = c("a",1,5), b12 = c("b",2,4), c13 = c("c",3,3), d14 = c("d",4,2), e15 = c("e",5,1))%>%
                dplyr::tibble()
testthat::expect_equal( transpose_df(mock_df), expected_df)


# Check for matching of weird columns with mixed data with NAs and booleans
mock_df2 <- data.frame(col1 = c(TRUE, "a", NA), col2 = c(NA,NA,NaN), col3 = c(1,0,-4), row.names = paste0(letters[1:3], 1:3))
expected_df2 <- data.frame(rowname = paste0('col',1:3),
                          a1 = c("TRUE",NA,1), b2 = c("a",NA,0), c3 = c(NA, "NaN", -4))%>%
                dplyr::tibble()

testthat::expect_equal( transpose_df(mock_df2), expected_df2)


# check if the clasess match of inputs and outputs match
mock_df3 <- data.frame(col1 = c(1,2), col2 = c(3,4), row.names = paste0(letters[1:2], 1:2))
expected_df3 <- data.frame(rowname = paste0('col',1:2),
                           a1 = c(1,3), b2 = c(3,4))%>%
  dplyr::tibble()

t_df2 <- transpose_df(mock_df3)

testthat::expect_equal( mock_df3%>%
                          transpose_df()%>%
                          lapply( class), list(rowname ='character',
                                                   a1 = 'numeric',
                                                   b2 = 'numeric'))


})















