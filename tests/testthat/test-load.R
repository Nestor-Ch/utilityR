testthat::test_that("load.label_colname works", {
  testdata <- testthat::test_path("fixtures","tool.xlsx")
  actual_output <- load.label_colname(testdata)
  testthat::expect_equal(actual_output,"label::English")
})

testthat::test_that("load.tool.choices colnames are correct", {
  testdata <- testthat::test_path("fixtures","tool.xlsx")
  label_colname <- "label::English"
  actual_output <- load.tool.choices(testdata,label_colname) %>% names
  expected_output <- data.frame(list_name = c("test","test","test2","test2"),
                                name = c("yes","no","dont_know","dont_know"),
                                `label::English` = c("Yes", "No", "Dont know", "Dont know")) %>%
    dplyr::rename("label::English" = `label..English`)%>% names

  testthat::expect_equal(actual_output,expected_output)
})
