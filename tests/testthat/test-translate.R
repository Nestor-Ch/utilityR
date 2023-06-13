testthat::test_that("find.responses works", {
  #test 1
  q.db <- data.frame()
  testdata <- data.frame(age = c(21,32), occupation = c("cook","train conductor"),uuid = c("abc","def"))
  actual_output <- find.responses(testdata,q.db,"responses",is.loop = F) %>%
    suppressWarnings()
  testthat::expect_equal(actual_output,
                         data.frame())

  #test 2
  q.db <- data.frame(name = c("age","occupation"))
  testdata <- data.frame()
  actual_output <- find.responses(testdata,q.db,"responses",is.loop = F) %>%
    suppressWarnings()
  testthat::expect_equal(actual_output,
                         data.frame())
  #test 3
  q.db <- data.frame(name = c("occupation"))
  testdata <- data.frame(age = c(21,32), occupation = c("cook","train conductor"),uuid = c("abc","def"))
  actual_output <- find.responses(testdata,q.db,"responses",is.loop = F)
  expected_output <- data.frame(uuid = c("abc","def"),
                                loop_index =c(NA,NA),
                                name = c("occupation","occupation"),
                                responses=c("cook","train conductor"))
  testthat::expect_equal(actual_output,
                         expected_output)
  #test 4
  q.db <- data.frame(name = c("occupation"))
  testdata <- data.frame(age = c(21,32), occupation = c("cook","train conductor"),
                         uuid = c("abc","def"), loop_index = c("loop_123","loop_456"))
  actual_output <- find.responses(testdata,q.db,"responses",is.loop = T)
  expected_output <- data.frame(uuid = c("abc","def"),
                                loop_index =c("loop_123","loop_456"),
                                name = c("occupation","occupation"),
                                responses=c("cook","train conductor"))
  testthat::expect_equal(actual_output,
                         expected_output)

})


