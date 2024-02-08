testthat::test_that('find.missing.ids works -  test 1 - issue if our data doesnt have a uuid column',{

  test_dir <- testthat::test_path('fixtures','data_others.xlsx')

  test_data <- readxl::read_xlsx(test_dir)


  testthat::expect_error(find.missing.ids(data = test_data,
                                          uniquis = c("f26b0917-a095-469a-af2e-db65c61afb2c","47cfe2eb-2fa7-4d20-8561-11390706f0a2")),
                         "data does not contain column uuid!")

})

testthat::test_that('find.missing.ids works -  test 2 - warning about the fake id',{

  test_dir <- testthat::test_path('fixtures','data_others.xlsx')

  test_data <- readxl::read_xlsx(test_dir)

  test_data <- test_data%>%
    dplyr::rename(uuid=`_uuid`)

  testthat::expect_warning(find.missing.ids(data = test_data,
                                            uniquis = c("f26b0917-a095-469a-af2e-db65c61afb2c","47cfe2eb-2fa7-4d20-8561-11390706f0a2",'fake')),
                           "Identifiers not found in data:\n\tfake")
})

testthat::test_that('find.missing.ids works -  test 3 - the result is correct',{

  test_dir <- testthat::test_path('fixtures','data_others.xlsx')

  test_data <- readxl::read_xlsx(test_dir)

  test_data <- test_data%>%
    dplyr::rename(uuid=`_uuid`)

  actual_result <- find.missing.ids(data = test_data,
                                    uniquis = c("f26b0917-a095-469a-af2e-db65c61afb2c","47cfe2eb-2fa7-4d20-8561-11390706f0a2",'fake'),
                                    print_warnings = F)

  testthat::expect_equal(actual_result,'fake')
})

testthat::test_that('find.missing.ids works -  test 4 Expect an issue if none of the ids exist',{

  test_dir <- testthat::test_path('fixtures','data_others.xlsx')

  test_data <- readxl::read_xlsx(test_dir)

  test_data <- test_data%>%
    dplyr::rename(uuid=`_uuid`)


  testthat::expect_warning(find.missing.ids(data = test_data,
                                            uniquis = c("fake1",'fake2')),
                           "NONE of the identifiers were found in data!")
})

testthat::test_that('find.missing.ids works -  test 5 - does it break if loop_index col is not in the data',{

  test_dir <- testthat::test_path('fixtures','data_others.xlsx')

  test_data_l <- suppressWarnings(readxl::read_xlsx(test_dir, sheet = 2) )

  testthat::expect_error(find.missing.ids(data = test_data_l,is.loop=T,
                                          uniquis = c("f26b0917-a095-469a-af2e-db65c61afb2c","47cfe2eb-2fa7-4d20-8561-11390706f0a2")),
                         "uniquis are loop indexes, but data does not contain column loop_index!")

})

testthat::test_that('find.missing.ids works -  test 6 - does it give a warning if I include 1 uuid?',{

  test_dir <- testthat::test_path('fixtures','data_others.xlsx')

  test_data_l <- suppressWarnings(readxl::read_xlsx(test_dir, sheet = 2) )

  test_data_l <- test_data_l%>%
    dplyr::rename(loop_index=`_index`)
  testthat::expect_warning(
    testthat::expect_warning(find.missing.ids(data = test_data_l,is.loop=T,
                                              uniquis = c("1","47cfe2eb-2fa7-4d20-8561-11390706f0a2")),
                             "not all provided identifiers are loop_indexes!"),
    "Identifiers not found in data:\n\t47cfe2eb-2fa7-4d20-8561-11390706f0a2"
  )

})

testthat::test_that('find.missing.ids works -  test 7 expect an empty string if you feed it one',{

  test_dir <- testthat::test_path('fixtures','data_others.xlsx')

  test_data_l <- suppressWarnings(readxl::read_xlsx(test_dir, sheet = 2) )

  # test 7 expect an empty string if you feed it one
  actual_result <- find.missing.ids(data = test_data_l,is.loop=T,
                                    uniquis = c())
  testthat::expect_equal(actual_result,character(0))
})


testthat::test_that('find.similar.surveys works - test 1 - test for the missing enum.column',{
  raw.data_test_dir <- testthat::test_path('fixtures','utilityR_raw_data.xlsx')
  tool.survey_test_dir <- testthat::test_path('fixtures','tool.survey_full.xlsx')
  test_tool.survey <- readxl::read_excel(tool.survey_test_dir)
  test_raw.main <- readxl::read_excel(raw.data_test_dir)[1:100,]


  # test for the missing enum.column
  testthat::expect_error(find.similar.surveys(test_raw.main, test_tool.survey, uuid="_uuid"))
})

testthat::test_that('find.similar.surveys works - test 2 test for the wrong uuid',{
  raw.data_test_dir <- testthat::test_path('fixtures','utilityR_raw_data.xlsx')
  tool.survey_test_dir <- testthat::test_path('fixtures','tool.survey_full.xlsx')
  test_tool.survey <- readxl::read_excel(tool.survey_test_dir)
  test_raw.main <- readxl::read_excel(raw.data_test_dir)[1:100,]

  # test for the wrong uuid
  testthat::expect_error(find.similar.surveys(test_raw.main, test_tool.survey, uuid="nouuid", enum.column="a2_1_enum_id"))
})

testthat::test_that('find.similar.surveys works - test 3 test for the small data frame',{
  raw.data_test_dir <- testthat::test_path('fixtures','utilityR_raw_data.xlsx')
  tool.survey_test_dir <- testthat::test_path('fixtures','tool.survey_full.xlsx')
  test_tool.survey <- readxl::read_excel(tool.survey_test_dir)
  test_raw.main <- readxl::read_excel(raw.data_test_dir)[1:100,]

  test_raw.main <- test_raw.main[1:1,]
  testthat::expect_error(find.similar.surveys(test_raw.main, test_tool.survey, uuid="_uuid", enum.column="a2_1_enum_id"))
})

testthat::test_that('find.similar.surveys works - test 4 test for the data without specified enum.column',{
  raw.data_test_dir <- testthat::test_path('fixtures','utilityR_raw_data.xlsx')
  tool.survey_test_dir <- testthat::test_path('fixtures','tool.survey_full.xlsx')
  test_tool.survey <- readxl::read_excel(tool.survey_test_dir)

  test_raw.main <- data.frame(
    "_uuid" = as.character(1:5),
    "start" = as.character(1:5),
    existing_column_1 = as.character(1:5),
    existing_column_2 = as.character(1:5),
    check.names = FALSE
  )

  testthat::expect_error(find.similar.surveys(test_raw.main, test_tool.survey, uuid="_uuid", enum.column="a2_1_enum_id"))
})

testthat::test_that('find.similar.surveys works - test 5 test for the data without specified uuid column',{
  raw.data_test_dir <- testthat::test_path('fixtures','utilityR_raw_data.xlsx')
  tool.survey_test_dir <- testthat::test_path('fixtures','tool.survey_full.xlsx')
  test_tool.survey <- readxl::read_excel(tool.survey_test_dir)

  test_raw.main <- data.frame(
    "uuid" = as.character(1:5),
    "start" = as.character(1:5),
    existing_column_1 = as.character(1:5),
    existing_column_2 = as.character(1:5),
    check.names = FALSE
  )

  testthat::expect_error(find.similar.surveys(test_raw.main, test_tool.survey, uuid="_uuid", enum.column="a2_1_enum_id"))
})

testthat::test_that('find.similar.surveys works - test 6 test for the little number of comparison columns',{
  raw.data_test_dir <- testthat::test_path('fixtures','utilityR_raw_data.xlsx')
  tool.survey_test_dir <- testthat::test_path('fixtures','tool.survey_full.xlsx')
  test_tool.survey <- readxl::read_excel(tool.survey_test_dir)


  test_raw.main <- data.frame(
    "_uuid" = as.character(1:5),
    "start" = as.character(1:5),
    # existing_column_1 = as.character(1:5),
    # existing_column_2 = as.character(1:5),
    "a2_1_enum_id" =rep("1", 5),
    check.names = FALSE
  )

  testthat::expect_error(find.similar.surveys(test_raw.main, test_tool.survey, uuid="_uuid", enum.column="a2_1_enum_id"))
})

testthat::test_that('find.similar.surveys works - test 7 no exist enumerators with more than 1 survey',{
  raw.data_test_dir <- testthat::test_path('fixtures','utilityR_raw_data.xlsx')
  tool.survey_test_dir <- testthat::test_path('fixtures','tool.survey_full.xlsx')
  test_tool.survey <- readxl::read_excel(tool.survey_test_dir)

  test_raw.main <- data.frame(
    "_uuid" = as.character(1:5),
    "start" = as.character(1:5),
    "a2_1_enum_id" = as.character(1:5),
    check.names = FALSE
  )

  testthat::expect_error(find.similar.surveys(test_raw.main, test_tool.survey, uuid="_uuid", enum.column="a2_1_enum_id"))
})

testthat::test_that('find.similar.surveys works - test 8 empty tool.survey column',{
  raw.data_test_dir <- testthat::test_path('fixtures','utilityR_raw_data.xlsx')
  tool.survey_test_dir <- testthat::test_path('fixtures','tool.survey_full.xlsx')
  test_tool.survey <- readxl::read_excel(tool.survey_test_dir)

  test_raw.main <- data.frame(
    "_uuid" = as.character(1:5),
    "start" = as.character(1:5),
    "a2_1_enum_id" = as.character(1:5),
    check.names = FALSE
  )

  # empty tool.survey column
  tool.survey <- data.frame()
  testthat::expect_error(find.similar.surveys(test_raw.main, test_tool.survey, uuid="_uuid", enum.column="a2_1_enum_id"))
})

testthat::test_that('find.similar.surveys works - test 9 correctness tests',{
  raw.data_test_dir <- testthat::test_path('fixtures','utilityR_raw_data.xlsx')
  tool.survey_test_dir <- testthat::test_path('fixtures','tool.survey_full.xlsx')
  test_tool.survey <- readxl::read_excel(tool.survey_test_dir)

  test_raw.main <- data.frame(
    "_uuid" = as.character(1:5),
    "start" = as.character(1:5),
    "a2_1_enum_id" = as.character(1:5),
    check.names = FALSE
  )


  test_raw.main1 <- readxl::read_excel(raw.data_test_dir)[1:1,]
  test_raw.main2 <- readxl::read_excel(raw.data_test_dir)[1:1,]
  test_raw.main3 <- readxl::read_excel(raw.data_test_dir)[1:1,]
  test_raw.main <- dplyr::bind_rows(test_raw.main1, test_raw.main2, test_raw.main3)
  test_raw.main$`_uuid` <- as.character(1:3)
  actual_output <- find.similar.surveys(test_raw.main, test_tool.survey, uuid="_uuid", enum.column="a2_1_enum_id")
  # correctness tests
  testthat::expect_equal(sum(actual_output$number_different_columns), 0)
  testthat::expect_equal(unique(actual_output$group_id), 1)
  # test for data with duplicates
  test_raw.main$`_uuid` <- rep(1, 3)
  testthat::expect_error(find.similar.surveys(test_raw.main, test_tool.survey, uuid="_uuid", enum.column="a2_1_enum_id"))

  analysis.result <- analyse.similarity(actual_output, enum.column="a2_1_enum_id")
  analysis <- analysis.result$analysis
  outliers <- analysis.result$outliers
  # correctness tests
  testthat::expect_equal(nrow(analysis), 1)
  testthat::expect_equal(nrow(outliers), 0)

  test_raw.main <- readxl::read_excel(raw.data_test_dir)
  test_raw.main <- test_raw.main %>% dplyr::filter(a2_1_enum_id == "JERU_001")
  # correctness tests
  testthat::expect_no_error(find.similar.surveys(test_raw.main, test_tool.survey, uuid="_uuid", enum.column="a2_1_enum_id"))
  actual_output <- find.similar.surveys(test_raw.main, test_tool.survey, uuid="_uuid", enum.column="a2_1_enum_id")
  # correctness tests
  testthat::expect_equal(nrow(actual_output), 21)
  testthat::expect_equal(sum(actual_output$number_different_columns), 68)

  ###### Analysis testing

  analysis.result <- analyse.similarity(actual_output, enum.column="a2_1_enum_id")
  analysis <- analysis.result$analysis
  outliers <- analysis.result$outliers
  # correctness tests
  testthat::expect_equal(nrow(analysis), 1)
  testthat::expect_equal(nrow(outliers), 0)
  testthat::expect_equal(analysis$sum_number_different_columns[[1]], 68)

})

testthat::test_that('column.cleaner works',{
  data <- data.frame(
    name = c("B A", "ferDINAND"),
    age = c("18", "20")
  )
  labels <- c("name")
  actual.output <- column.cleaner(data, labels=c("name"))
  expected.output <- data.frame(
    name = c("a b", "ferdinand"),
    age = c("18", "20")
  )
  testthat::expect_equal(actual.output, expected.output)
  temp <- data.frame(
    name = c("vlad", "peleKh Bohdan"),
    age = c(19, 20)
  )
  testthat::expect_warning(column.cleaner(temp, colnames(temp)))
})

testthat::test_that('find.relevances works - test 1 - general functionality',{
  path <- testthat::test_path('fixtures','tool.survey_full.xlsx')
  tool.survey <- load.tool.survey(path, 'label:English')

  var_list <- 'b8_petrol_official_price_yn'

  expected_output <- data.frame(name='b8_petrol_official_price',
                                relevant = 'b8_petrol_official_price_yn/yes') %>%
    dplyr::tibble()

  actual_output <- find.relevances(tool.survey,var_list = var_list)

  testthat::expect_equal(actual_output,expected_output)
})

testthat::test_that('find.relevances works - test 2 multiple items in the list',{
  path <- testthat::test_path('fixtures','tool.survey_full.xlsx')
  tool.survey <- load.tool.survey(path, 'label:English')

  # test 2 multiple items in the list.
  var_list <- c('b7_vehicle_fuel','b13_firewood_price_yn')

  expected_output <- data.frame(name=c('b8_petrol_official_price_yn',
                                       'b8_1_petrol_unofficial_price_yn',
                                       'b9_diesel_official_price_yn',
                                       'b9_1_diesel_unofficial_price_yn',
                                       'b10_gas_vehicle_price_yn',
                                       'b13_firewood_price',
                                       'b15_electricity_vehicles_price_yn'
  ),
  relevant = c('b7_vehicle_fuel/petrol_vehicles',
               'b7_vehicle_fuel/petrol_vehicles',
               'b7_vehicle_fuel/diesel_vehicles',
               'b7_vehicle_fuel/diesel_vehicles',
               'b7_vehicle_fuel/gas_vehicles',
               'b13_firewood_price_yn/yes',
               'b7_vehicle_fuel/electricity_vehicles'
  )
  ) %>% dplyr::tibble()

  actual_output <- find.relevances(tool.survey,var_list = var_list)

  testthat::expect_equal(actual_output,expected_output)

})


