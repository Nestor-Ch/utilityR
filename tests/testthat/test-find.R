testthat::test_that('find.missing.ids works',{

  test_dir <- testthat::test_path('fixtures','data_others.xlsx')

  test_data <- readxl::read_xlsx(test_dir)

  # test 1 - issue if our data doesn't have a uuid column

  testthat::expect_error(find.missing.ids(data = test_data,
                                            uniquis = c("f26b0917-a095-469a-af2e-db65c61afb2c","47cfe2eb-2fa7-4d20-8561-11390706f0a2")),
                         "data does not contain column uuid!")


  test_data <- test_data%>%
    dplyr::rename(uuid=`_uuid`)

  # test 2 - warning about the 'fake' id
  testthat::expect_warning(find.missing.ids(data = test_data,
                                            uniquis = c("f26b0917-a095-469a-af2e-db65c61afb2c","47cfe2eb-2fa7-4d20-8561-11390706f0a2",'fake')),
                           "Identifiers not found in data:\n\tfake")
  # test 3 - the result is correct
  actual_result <- find.missing.ids(data = test_data,
                                    uniquis = c("f26b0917-a095-469a-af2e-db65c61afb2c","47cfe2eb-2fa7-4d20-8561-11390706f0a2",'fake'),
                                    print_warnings = F)

  testthat::expect_equal(actual_result,'fake')

  # test 4 Expect an issue if none of the ids exist

  testthat::expect_warning(find.missing.ids(data = test_data,
                                            uniquis = c("fake1",'fake2')),
                           "NONE of the identifiers were found in data!")

  # test 5 - does it break if loop_index col is not in the data

  test_dir <- testthat::test_path('fixtures','data_others.xlsx')

  test_data_l <- suppressWarnings(readxl::read_xlsx(test_dir, sheet = 2) )

  testthat::expect_error(find.missing.ids(data = test_data_l,is.loop=T,
                                          uniquis = c("f26b0917-a095-469a-af2e-db65c61afb2c","47cfe2eb-2fa7-4d20-8561-11390706f0a2")),
                         "uniquis are loop indexes, but data does not contain column loop_index!")

  # test 6 - does it give a warning if I include 1 uuid?

  test_data_l <- test_data_l%>%
    dplyr::rename(loop_index=`_index`)
  testthat::expect_warning(
  testthat::expect_warning(find.missing.ids(data = test_data_l,is.loop=T,
                                          uniquis = c("1","47cfe2eb-2fa7-4d20-8561-11390706f0a2")),
                           "not all provided identifiers are loop_indexes!"),
  "Identifiers not found in data:\n\t47cfe2eb-2fa7-4d20-8561-11390706f0a2"
)
  # test 7 expect an empty string if you feed it one
  actual_result <- find.missing.ids(data = test_data_l,is.loop=T,
                                    uniquis = c())
  testthat::expect_equal(actual_result,character(0))
})
