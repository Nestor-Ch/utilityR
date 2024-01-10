

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
    uuid = c("f26b0917-a095-469a-af2e-db65c61afb2c","038f21c5-c9c5-4b45-a3fa-1a4871b5763d",
             "ad166d0f-fc1e-4250-99ec-2ad040d5a871","4fed7425-1f7a-4c3e-8ab4-216b63091db9",
             "fb0e950a-ce6d-4ef8-9054-78cfc4905b68"),
    loop_index = NA,
    variable = c("q2_4_4_main_reason/relative_safety","q2_4_4_main_reason/availability_of_accommodation",
                 "q2_4_4_main_reason/economic_migration",
                 "q2_4_4_main_reason/availability_of_humanitarian_aid",
               "q2_4_4_main_reason/feeling_part_of_the_community"),
    old.value = c('0','1','1','2','1'),
    new.value = c('1',NA,'0','0',NA),
    issue = 'test'
  ) %>% dplyr::tibble()
  actual_output <- compare_columns(clean_data = clean_data, raw_data = raw_data, id_col = 'uuid',
                                   columns_to_check = col_names, issue = 'test')

  testthat::expect_equal(actual_output,expected_output)

  # test 2 - test if it works with loops

  clean_data$loop_index <- paste0('loop1',1:10)
  raw_data$loop_index <- paste0('loop1',1:10)


  expected_output <- data.frame(
    uuid = c("f26b0917-a095-469a-af2e-db65c61afb2c","038f21c5-c9c5-4b45-a3fa-1a4871b5763d",
             "ad166d0f-fc1e-4250-99ec-2ad040d5a871","4fed7425-1f7a-4c3e-8ab4-216b63091db9",
             "fb0e950a-ce6d-4ef8-9054-78cfc4905b68"),
    loop_index = c('loop11','loop13','loop14','loop18','loop110'),
    variable = c("q2_4_4_main_reason/relative_safety","q2_4_4_main_reason/availability_of_accommodation",
                 "q2_4_4_main_reason/economic_migration",
                 "q2_4_4_main_reason/availability_of_humanitarian_aid",
                 "q2_4_4_main_reason/feeling_part_of_the_community"),
    old.value = c('0','1','1','2','1'),
    new.value = c('1',NA,'0','0',NA),
    issue = 'test'
  ) %>% dplyr::tibble()
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


testthat::test_that('compare_rows works',{


  test_dir <- testthat::test_path('fixtures','data_others.xlsx')
  raw_data <- readxl::read_xlsx(test_dir) %>%
    dplyr::rename(uuid = `_uuid`)
  clean_data <- raw_data[3:nrow(raw_data),]

  # test 1- basic functionality

  actual_output <- compare_rows(data_raw = raw_data, data_clean = clean_data, id_col = 'uuid', col.enum = 'q0_2_enum_id',is.loop = F,reason = 'test')

  expected_output <- data.frame(uuid = raw_data$uuid[1:2],
                                col_enum =raw_data$q0_2_enum_id[1:2],
                                reason = 'test') %>% dplyr::tibble()

  testthat::expect_equal(actual_output,expected_output)

  # test that it works with loops
  raw_loop <- raw_data %>% dplyr::select(-q0_2_enum_id)
  raw_loop$loop_index <- 1:nrow(raw_loop)

  clean_loop <- raw_loop[3:nrow(raw_loop),]

  actual_output <- compare_rows(data_raw = raw_loop, data_clean = clean_loop, id_col = 'loop_index', col.enum = 'q0_2_enum_id',is.loop = T,data.main = raw_data,reason = 'test')

  expected_output <- data.frame(uuid = raw_data$uuid[1:2],
                                loop_index=1:2,
                                col_enum =raw_data$q0_2_enum_id[1:2],
                                reason = 'test') %>% dplyr::tibble()

  testthat::expect_equal(actual_output,expected_output)

  # test 3 - make sure that the user has passed the correct columns

  testthat::expect_error(
    compare_rows(data_raw = raw_loop, data_clean = clean_loop, id_col = 'uuid', col.enum = 'q0_2_enum_id',is.loop = T,data.main = raw_data,reason = 'test'),
    "You've indicated that the data is a loop but didn't choose loop_index as the id_col"
  )

  # test 4 - make sure that the user has passed the correct columns

  testthat::expect_error(
    compare_rows(data_raw = raw_loop, data_clean = clean_loop, id_col = 'loop_index', col.enum = 'q0_2_enum_id',is.loop = F,data.main = raw_data,reason = 'test'),

    "You've indicated that the data is not a loop but choose loop_index as the id_col"
  )


  # test 5 - make sure that the user has passed the correct columns

  testthat::expect_error(
    compare_rows(data_raw = raw_loop, data_clean = clean_loop, id_col = 'uuid', col.enum = 'q0_2_enum_id',is.loop = F,data.main = raw_data,reason = 'test'),
    "You've indicated that the data is not a loop but your dataframe contains loop_index column"
  )

  # test 6 - make sure that the user passed data with id columns

  testthat::expect_error(
    compare_rows(data_raw = raw_loop %>% dplyr::select(-loop_index), data_clean = clean_loop, id_col = 'loop_index', col.enum = 'q0_2_enum_id',is.loop = T,data.main = raw_data,reason = 'test'),
    'Your id_col is not present in one of the dataframes'
  )


  # test 7 -more ids

  clean_loop$loop_index[5] <- 'test'

  testthat::expect_error(
    compare_rows(data_raw = raw_loop , data_clean = clean_loop, id_col = 'loop_index', col.enum = 'q0_2_enum_id',is.loop = T,data.main = raw_data,reason = 'test')
  )

  # test 8 - nothing was deleted

  testthat::expect_message(
    compare_rows(data_raw = clean_loop , data_clean = clean_loop, id_col = 'loop_index', col.enum = 'q0_2_enum_id',is.loop = T,data.main = raw_data,reason = 'test'),
    "You didn't delete any observations during cleaning"
  )


})
























