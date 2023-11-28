testthat::test_that('lookup_columns works',{

  test_frame <- data.frame(
    uuid = c('1','2','3','4'),
    test_col = c('test1','test2','test3','test4')
  )

  test_frame2 <- data.frame(
    uuid = c('1','2','3','4'),
    test_col = c('test4','test3','test2','test41')
  )

  actual_output <- lookup_columns(id = '1',
                 id_column = 'uuid',
                 column = 'test_col',
                 clean.data = test_frame,
                 raw.data = test_frame2)

  expected_output <- data.frame(
    id = '1',
    column = 'test_col',
    old.value = 'test4',
    new.value = 'test1'
  )

  testthat::expect_equal(actual_output,expected_output)

})



testthat::test_that('compare_columns works',{
  test_dir <- testthat::test_path('fixtures','data_others.xlsx')
  test_data <- readxl::read_xlsx(test_dir) %>%
    dplyr::rename(uuid = `_uuid`)

  col_names <- test_data %>% dplyr::select(dplyr::starts_with('q2_4_4')) %>% names()

  clean_data <- test_data[1:10,c('uuid',col_names)]
  raw_data <- clean_data
  raw_data[1,3] <- 0
  raw_data[4,5] <- 1
  raw_data[3,6] <- 1
  raw_data[10,8] <- 1
  raw_data[8,9] <- 2
  raw_data[5,14] <- NA

  expected_output <- data.frame(
    id = c("f26b0917-a095-469a-af2e-db65c61afb2c","ad166d0f-fc1e-4250-99ec-2ad040d5a871",
           "038f21c5-c9c5-4b45-a3fa-1a4871b5763d","fb0e950a-ce6d-4ef8-9054-78cfc4905b68",
           "4fed7425-1f7a-4c3e-8ab4-216b63091db9"),
    column = c("q2_4_4_main_reason/relative_safety","q2_4_4_main_reason/economic_migration",
               "q2_4_4_main_reason/availability_of_accommodation","q2_4_4_main_reason/feeling_part_of_the_community",
               "q2_4_4_main_reason/availability_of_humanitarian_aid"),
    old.value = c('0','1','1','1','2'),
    new.value = c('1','0',NA,NA,'0')
  )
  actual_output <- compare_columns(clean_data = clean_data, raw_data = raw_data, uuid_col = 'uuid',columns_to_check = col_names)

  testthat::expect_equal(actual_output,expected_output)


  # test 2 - remove unnecessary ids if any

  clean_data$uuid[1] <- 'fake'
  clean_data$uuid[2] <- 'fake2'


  testthat::expect_warning(
    compare_columns(clean_data = clean_data, raw_data = raw_data, uuid_col = 'uuid',columns_to_check = col_names),
    'Some of the uuids in your clean data are not present in your raw data and will be excluded: fake,\nfake2'
  )

  # test 3 - remove unnecessary columns if any

  clean_data <- test_data[1:10,c('uuid',col_names)]
  col_names2 <- c(col_names,'fake')

  testthat::expect_warning(
    compare_columns(clean_data = clean_data, raw_data = raw_data, uuid_col = 'uuid',columns_to_check = col_names2),
    'Some of the columns in your columns_to_check list are not present in your clean data and will be excluded: fake'
  )


})












