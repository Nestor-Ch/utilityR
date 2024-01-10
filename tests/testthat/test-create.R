testthat::test_that("create.deletion.log works", {

  # get the dataframe
  filename <- testthat::test_path("fixtures","data_others.xlsx")
  test_data <- readxl::read_excel(filename, col_types = 'text')%>%
    dplyr::rename(uuid = `_uuid`)


  del_test <- dplyr::filter(test_data, uuid %in% c("37a0e3d8-8081-4b01-be36-aecf37495a31","98201d53-a8fa-4e10-84f2-e48ec3f8f98b",
                                                   "39c79139-987d-4f57-a21b-a5a3e92a22e7"))
  # test 1 - general functionality


  actual_output <- create.deletion.log(data = del_test,
                                       col_enum = 'q0_2_enum_id',
                                       is.loop = F,
                                       reason = 'test')

  expected_output <- data.frame(
    uuid = c("37a0e3d8-8081-4b01-be36-aecf37495a31","98201d53-a8fa-4e10-84f2-e48ec3f8f98b","39c79139-987d-4f57-a21b-a5a3e92a22e7"),
    col_enum = c("enum_20","enum_20","enum_20"),
    reason = 'test'
  )%>%
    dplyr::tibble()

  testthat::expect_equal(actual_output,expected_output)


  # test 2 - test that it returns an empty df if you feed it an empty df

  actual_output <- create.deletion.log(data = del_test[0,],
                                       col_enum = 'q0_2_enum_id',
                                       is.loop = F,
                                       reason = 'test')

  testthat::expect_equal(actual_output,data.frame())

  # test 3 - expect an error if we provide a non-existent col_enum

  testthat::expect_error(

    actual_output <- create.deletion.log(data = del_test,
                                         col_enum = 'fake_col',
                                         is.loop = F,
                                         reason = 'test')

  )

  # test 4 - expect an error if say that our data is a loop but it isn't

  testthat::expect_error(

    actual_output <- create.deletion.log(data = del_test,
                                         data.main = test_data,
                                         col_enum = 'q0_2_enum_id',
                                         is.loop = T,
                                         reason = 'test')
  )


  # test 5 - the function works with loops

  filename <- testthat::test_path("fixtures","data_others.xlsx")
  test_data_l <- readxl::read_excel(filename, col_types = 'text', sheet = 'loop')%>%
    dplyr::rename(loop_index = `_index`,
                  uuid = `_submission__uuid`)


  del_test <- dplyr::filter(test_data_l, loop_index %in% c('904',"276","999"))

  actual_output <- create.deletion.log(data = del_test,
                                       data.main = test_data,
                                       col_enum = 'q0_2_enum_id',
                                       is.loop = T,
                                       reason = 'test')

  expected_output <- data.frame(
    uuid = c("9297bf6b-e35d-45c9-ae19-02e92f53a678","da2516dd-a175-4c75-b941-ffcd7fe247cb","4ee0da4f-1262-4f50-82d3-7ea66f62ca02"),
    loop_index = c("276",'904',"999"),
    col_enum = c("enum_17","enum_13","enum_07"),
    reason = 'test'
  )%>%
    dplyr::tibble()

  testthat::expect_equal(actual_output,expected_output)


  # test 6 - is loop = T, but we haven't provided the main data

  testthat::expect_error(
    create.deletion.log(data = del_test,
                        col_enum = 'q0_2_enum_id',
                        is.loop = T,
                        reason = 'test')
  )

  # test 7 - is loop = F, but our data is a loop, and we haven't provided the main data
  # we should get both a warning and an error

  testthat::expect_error(
    testthat::expect_warning(
      create.deletion.log(data = del_test,
                          col_enum = 'q0_2_enum_id',
                          is.loop = F,
                          reason = 'test')
    ))

  # test 8 - is loop = F, but our data is aloop

  testthat::expect_warning(
    create.deletion.log(data = del_test,
                        data.main = test_data,
                        col_enum = 'q0_2_enum_id',
                        is.loop = F,
                        reason = 'test')
  )

  # test 9 - our data doesn't have a uuid column

  del_test <- dplyr::filter(test_data_l, loop_index %in% c('904',"276","999")) %>%
    dplyr::select(-uuid)


  testthat::expect_error(
    create.deletion.log(data = del_test,
                        data.main = test_data,
                        col_enum = 'q0_2_enum_id',
                        is.loop = T,
                        reason = 'test')
  )

})



testthat::test_that('create.follow.up.requests works',{

  test_frame <- data.frame(
    uuid = c('uuid','uuid2'),
    survey.date = c('start','start'),
    check.id = '0',
    variable = 'q2_4_3_main_cause',
    issue = 'test',
    old.value = c(NA_character_,'security_considerations'),
    new.value = NA_character_,
    invalid = NA_character_,
    explanation = NA_character_
  )

  temp_dir <- tempdir()

  create.follow.up.requests(
    checks.df = test_frame,
    directory = temp_dir,
    wb_name = 'testo'
  )

  testthat::expect_true(file.exists(paste0(temp_dir, "testo.xlsx")))

  # test 2

  test_frame <- data.frame(
    uuid = c('uuid','uuid2'),
    survey.date = c('start','start'),
    check.id = '2',
    variable = 'q2_4_3_main_cause',
    issue = 'test',
    old.value = c(NA_character_,'security_considerations'),
    new.value = NA_character_,
    invalid = NA_character_,
    explanation = NA_character_
  )

  temp_dir <- tempdir()

  create.follow.up.requests(
    checks.df = test_frame,
    directory = temp_dir,
    wb_name = 'testo1'
  )

  testthat::expect_true(file.exists(paste0(temp_dir, "testo1.xlsx")))

  unlink(paste0(temp_dir, "testo.xlsx"))
  unlink(paste0(temp_dir, "testo1.xlsx"))

})

testthat::test_that('create.translate.requests works',{


  test_frame = data.frame(
    uuid = c('test_uuid','testo_2','testo_3'),
    loop_index = NA,
    name = c('q0_4_2_1_center_idp_other','q2_3_6_1_underemployment_other','q2_3_9_1_sector_potential_other'),
    responses = c('ukr_resp 1','ukr_resp2','ukr_resp3'),
    ref.name = c('q0_4_2_center_idp','q2_3_6_underemployment','q2_3_9_sector_potential'),
    full.label = 'full_label',
    ref.type = 'select_one',
    choices = c('testo_1', 'testo2', 'testo3'),
    choices.label = c('testo_1', 'testo2', 'testo3'),
    response.en = c('en_resp 1','en_resp2','en_resp3')
  )


  actual_result <- create.translate.requests(
                                             responses.j = test_frame,
                                             response_colname = 'responses')
  expected_result <- dplyr::tibble(
    uuid = c('test_uuid','testo_2','testo_3'),
    loop_index = NA,
    name = c('q0_4_2_1_center_idp_other','q2_3_6_1_underemployment_other','q2_3_9_1_sector_potential_other'),
    ref.name = c('q0_4_2_center_idp','q2_3_6_underemployment','q2_3_9_sector_potential'),
    full.label = 'full_label',
    ref.type = 'select_one',
    choices.label = c('testo_1', 'testo2', 'testo3'),
    responses = c('ukr_resp 1','ukr_resp2','ukr_resp3'),
    response.en = c('en_resp 1','en_resp2','en_resp3'),
    "TRUE other (provide a better translation if necessary)" = NA,
    "EXISTING other (copy the exact wording from the options in column choices.label)"=NA,
    "INVALID other (insert yes or leave blank)"=NA
    )

  testthat::expect_equal(actual_result,expected_result)


})









