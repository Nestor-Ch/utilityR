testthat::test_that('select.multiple.check works',{

  tool.survey_test_dir <- testthat::test_path('fixtures','tool.survey_full.xlsx')
  test_tool.survey <- readxl::read_excel(tool.survey_test_dir)

  raw.data_test_dir <- testthat::test_path('fixtures','utilityR_raw_data.xlsx')
  test_raw.main <- readxl::read_excel(raw.data_test_dir)[1:100,]

  # test correctness of the work when empty output

  test_raw.main <- test_raw.main %>% dplyr::rename(uuid=`_uuid`)
  output <- select.multiple.check(test_raw.main, test_tool.survey, id_col="uuid")
  testthat::expect_equal(nrow(output), 20)

  # test correctness of the work when tool.survey doesn't match data

  tool.wrong_survey_test_dir <- testthat::test_path('fixtures','tool.survey_wrong.xlsx')
  test_tool.wrong_survey <- readxl::read_excel(tool.wrong_survey_test_dir)

  testthat::expect_error(select.multiple.check(test_raw.main,
                                               test_tool.wrong_survey, id_col="uuid"))

  # test correctness of the work on the real data

  raw.data_test_dir <- testthat::test_path('fixtures','utilityR_raw_data.xlsx')
  test_raw.main <- readxl::read_excel(raw.data_test_dir)
  test_raw.main <- test_raw.main %>% dplyr::rename(uuid=`_uuid`)
  output <- select.multiple.check(test_raw.main, test_tool.survey, id_col="uuid")
  testthat::expect_equal(nrow(output), 92)

  test_raw.main[1, "b7_vehicle_fuel/diesel_vehicles"] = "0"
  output <- select.multiple.check(test_raw.main, test_tool.survey, id_col="uuid")
  testthat::expect_equal(nrow(output), 93)

  test_raw.main[1, "b7_vehicle_fuel/diesel_vehicles"] = "1"
  output <- select.multiple.check(test_raw.main, test_tool.survey, id_col="uuid")
  testthat::expect_equal(nrow(output), 92)

  test_raw.main[2, "b7_vehicle_fuel/diesel_vehicles"] = "1"
  output <- select.multiple.check(test_raw.main, test_tool.survey, id_col="uuid")
  testthat::expect_equal(nrow(output), 93)

  # test error when tool.survey doesn't consist select_multiple questions

  survey.df <- data.frame(
    type = c("select_one", "text", "text", "integer"),
    name = c("country", "name", "last_name", "age"),
    `label::English` = c("label1", "label2", "label3", "label4")
  )
  data <- data.frame(
    uuid = as.character(1:5),
    country = rep("Ukraine", 5),
    name = rep("Alex", 5)
  )

  testthat::expect_error(select.multiple.check(data, survey.df, id_col="uuid"))

  # test correctness

  survey.df <- data.frame(
    type = c("select_multiple"),
    name = c("favourite_food"),
    `label::English` = c("favourite_food")
  )
  data <- data.frame(
    uuid = as.character(1:4),
    favourite_food = c("apple meat", "meat", NA, NA),
    "favourite_food/apple" = c(1, 0, NA, NA),
    "favourite_food/meat" = c(1, 1, NA, NA),
    "favourite_food/potato" = c(0, 0, NA, NA),
    check.names = FALSE
  )

  output <- select.multiple.check(data, survey.df, id_col="uuid")
  testthat::expect_equal(nrow(output), 0)

  data <- data.frame(
    uuid = as.character(1:4),
    favourite_food = c("apple meat", "meat", NA, NA),
    "favourite_food/apple" = c(1, 0, NA, NA),
    "favourite_food/meat" = c(1, 0, NA, NA),
    "favourite_food/potato" = c(0, 0, NA, NA),
    check.names = FALSE
  )
  output <- select.multiple.check(data, survey.df, id_col="uuid")
  testthat::expect_equal(nrow(output), 1)

  data <- data.frame(
    uuid = as.character(1:4),
    favourite_food = c("apple", "meat", NA, NA),
    "favourite_food/apple" = c(1, 0, NA, NA),
    "favourite_food/meat" = c(1, 1, NA, NA),
    "favourite_food/potato" = c(0, 0, NA, NA),
    check.names = FALSE
  )
  output <- select.multiple.check(data, survey.df, id_col="uuid")
  testthat::expect_equal(nrow(output), 1)

  data <- data.frame(
    uuid = as.character(1:4),
    favourite_food = c("apple meat", "meat", NA, NA),
    "favourite_food/apple" = c(1, 0, NA, NA),
    "favourite_food/meat" = c(1, 1, NA, NA),
    "favourite_food/potato" = c(0, NA, NA, NA),
    check.names = FALSE
  )
  output <- select.multiple.check(data, survey.df, id_col="uuid")
  testthat::expect_equal(nrow(output), 1)

  survey.df <- data.frame(
    name = c("favourite_food"),
    `label::English` = c("favourite_food")
  )
  testthat::expect_error(select.multiple.check(data, survey.df, id_col="uuid"))
})
