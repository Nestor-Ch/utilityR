testthat::test_that("get.type works", {
  filename <- testthat::test_path("fixtures","tool.xlsx")
  label_colname <- "label::English"
  tool.survey <- utilityR::load.tool.survey(filename,label_colname)
  testthat::expect_error(get.type("a2_partner"))

  testthat::expect_equal(get.type("a2_partner",tool.survey),"select_one")
  testthat::expect_warning(get.type("a_partner",tool.survey))
})

testthat::test_that("get.label works", {
  filename <- testthat::test_path("fixtures","tool.xlsx")
  label_colname <- "label::English"
  tool.survey <- utilityR::load.tool.survey(filename,label_colname)
  testthat::expect_error(get.label("a2_partner",label_colname = label_colname))
  testthat::expect_error(get.label("a2_partner",tool.survey = tool.survey))

  actual_output <- get.label("b17_access_stores/test",label_colname = label_colname,tool.survey = tool.survey) %>%
    suppressWarnings()
  testthat::expect_equal(actual_output,"B17_How have the war and its related developments affected your ability to access your usual store or marketplace this month?")
  testthat::expect_equal(get.label("a2_partner",label_colname,tool.survey),"A2_Partner name")
  testthat::expect_warning(get.label("a_partner",label_colname,tool.survey))
})

testthat::test_that("get.choice.label", {
  filename <- testthat::test_path("fixtures","tool.xlsx")
  label_colname <- "label::English"
  tool.choices <- load.tool.choices(filename,label_colname)

  testthat::expect_error(get.choice.label("yes",list = "yn",label_colname = label_colname))
  testthat::expect_error(get.choice.label("yes",list = "yn",tool.choices = tool.choices))
  testthat::expect_error(get.choice.label("yes",list="y",tool.choices = tool.choices,label_colname = label_colname))

  testthat::expect_equal(get.choice.label("yes","yn",label_colname,tool.choices),"Yes")
  testthat::expect_warning(get.choice.label("ye","yn",label_colname,tool.choices))
})
