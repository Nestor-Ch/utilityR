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
    variable = 'test_col',
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
    uuid = c("f26b0917-a095-469a-af2e-db65c61afb2c","ad166d0f-fc1e-4250-99ec-2ad040d5a871",
           "038f21c5-c9c5-4b45-a3fa-1a4871b5763d","fb0e950a-ce6d-4ef8-9054-78cfc4905b68",
           "4fed7425-1f7a-4c3e-8ab4-216b63091db9"),
    loop_index = NA,
    variable = c("q2_4_4_main_reason/relative_safety","q2_4_4_main_reason/economic_migration",
               "q2_4_4_main_reason/availability_of_accommodation","q2_4_4_main_reason/feeling_part_of_the_community",
               "q2_4_4_main_reason/availability_of_humanitarian_aid"),
    old.value = c('0','1','1','1','2'),
    new.value = c('1','0',NA,NA,'0'),
    issue = 'test'
  )
  actual_output <- compare_columns(clean_data = clean_data, raw_data = raw_data, id_col = 'uuid',
                                   columns_to_check = col_names, issue = 'test')

  testthat::expect_equal(actual_output,expected_output)

  # test 2 - test if it works with loops

  clean_data$loop_index <- paste0('loop1',1:10)
  raw_data$loop_index <- paste0('loop1',1:10)


  expected_output <- data.frame(
    uuid = c("f26b0917-a095-469a-af2e-db65c61afb2c","ad166d0f-fc1e-4250-99ec-2ad040d5a871",
             "038f21c5-c9c5-4b45-a3fa-1a4871b5763d","fb0e950a-ce6d-4ef8-9054-78cfc4905b68",
             "4fed7425-1f7a-4c3e-8ab4-216b63091db9"),
    loop_index = c('loop11','loop14','loop13','loop110','loop18'),
    variable = c("q2_4_4_main_reason/relative_safety","q2_4_4_main_reason/economic_migration",
                 "q2_4_4_main_reason/availability_of_accommodation","q2_4_4_main_reason/feeling_part_of_the_community",
                 "q2_4_4_main_reason/availability_of_humanitarian_aid"),
    old.value = c('0','1','1','1','2'),
    new.value = c('1','0',NA,NA,'0'),
    issue = 'test'
  )
  actual_output <- compare_columns(clean_data = clean_data, raw_data = raw_data, id_col = 'loop_index',is.loop = T,
                                   columns_to_check = col_names, issue = 'test')

  testthat::expect_equal(actual_output,expected_output)

  # test 3 - remove unnecessary ids if any

  clean_data$loop_index[1] <- 'fake'
  clean_data$loop_index[2] <- 'fake2'


  testthat::expect_warning(
    compare_columns(clean_data = clean_data, raw_data = raw_data, id_col = 'loop_index',columns_to_check = col_names,
                    issue = 'test', is.loop=T)
    ,
    'Some of the ids in your clean data are not present in your raw data and will be excluded: fake,\nfake2'
  )

  # test 4 - remove unnecessary columns if any

  clean_data <- test_data[1:10,c('uuid',col_names)]
  clean_data$loop_index <- 'tes'
  raw_data <- clean_data
  col_names2 <- c(col_names,'fake')

  testthat::expect_warning(
    compare_columns(clean_data = clean_data, raw_data = raw_data, id_col = 'loop_index',columns_to_check = col_names2,
                    issue = 'test', is.loop=T),
    'Some of the columns in your columns_to_check list are not present in your clean data and will be excluded: fake'
  )

  # test 5 - make sure that the user has passed the correct columns

  testthat::expect_error(
    compare_columns(clean_data = clean_data, raw_data = raw_data, id_col = 'uuid',columns_to_check = col_names,
                    issue = 'test', is.loop=T),
    "You've indicated that the data is a loop but didn't choose loop_index as the id_col"
  )

  # test 6 - make sure that the user has passed the correct columns

  testthat::expect_error(
    compare_columns(clean_data = clean_data, raw_data = raw_data, id_col = 'loop_index',columns_to_check = col_names,
                    issue = 'test', is.loop=F),
    "You've indicated that the data is not a loop but choose loop_index as the id_col"
  )


  # test 7 - make sure that the user has passed the correct columns


  testthat::expect_error(
    compare_columns(clean_data = clean_data, raw_data = raw_data, id_col = 'uuid',columns_to_check = col_names,
                    issue = 'test', is.loop=F),
    "You've indicated that the data is not a loop but your dataframe contains loop_index column"
  )



  # test 8 - make sure that the user has passed the correct columns


  clean_data <- test_data[1:10,c('uuid',col_names)]
  raw_data <- clean_data


  testthat::expect_error(
    compare_columns(clean_data = clean_data, raw_data = raw_data, id_col = 'loop_index',columns_to_check = col_names,
                    issue = 'test', is.loop=T),
    'Your id_col is not present in one of the dataframes'
  )

})












