

testthat::test_that("apply.changes works", {


  # load the tool data
  filename <- testthat::test_path("fixtures","tool_others.xlsx")
  label_colname <- "label::English"
  tool.survey <- utilityR::load.tool.survey(filename,label_colname)

  # get the tool.choices db
  filename <- testthat::test_path("fixtures","tool_others.xlsx")
  tool.choices <- readxl::read_excel(filename, sheet = 'choices', col_types = 'text')

  # get the change log file
  filename <- testthat::test_path("fixtures","data_cl_log.xlsx")
  cl_log <- readxl::read_excel(filename, col_types = 'text') %>%
    dplyr::mutate(check = 2)

  # get the dataframe
  filename <- testthat::test_path("fixtures","data_others.xlsx")
  test_data <- readxl::read_excel(filename, col_types = 'text')%>%
    dplyr::rename(uuid = `_uuid`) %>%
    dplyr::filter(uuid %in% cl_log$uuid)  # keep only the uuids I'll be changing


  test_data <- test_data[,c('uuid', intersect(cl_log$variable, names(test_data)))] # keep only the colnames I'll be using

  # test 1 - general functionality

  expected_output <- test_data %>%
    dplyr::mutate(q0_4_2_1_center_idp_other= ifelse(uuid=="2343f19e-819c-4f1f-b827-cff4d9c7a953","Religious community of the First Christian Church of the Living God in Mukachevo",q0_4_2_1_center_idp_other),
                  q0_4_2_1_center_idp_other= ifelse(uuid=="db187669-7f5d-4d8b-9cba-aa212fd44da9","Religious community of the First Christian Evangelical Church of the Living God in Mukachevo",q0_4_2_1_center_idp_other),
                  q7_2_2_1_initiate_compensation_other= ifelse(uuid=="bd005032-6f63-455f-8202-313583a128b1",NA,q7_2_2_1_initiate_compensation_other),
                  q7_2_2_1_initiate_compensation_other= ifelse(uuid=="f903166e-abc9-4258-848b-77ada6987d31",NA,q7_2_2_1_initiate_compensation_other),
                  q7_2_2_initiate_compensation= ifelse(uuid=="bd005032-6f63-455f-8202-313583a128b1",NA,q7_2_2_initiate_compensation),
                  q7_2_2_initiate_compensation= ifelse(uuid=="f903166e-abc9-4258-848b-77ada6987d31",NA,q7_2_2_initiate_compensation),
                  q0_4_2_1_center_idp_other= ifelse(uuid=="a46a1c10-bf18-4594-a0be-99447fa22116",NA,q0_4_2_1_center_idp_other),
                  q0_4_2_1_center_idp_other= ifelse(uuid=="51862558-1b68-466a-8e71-be2817dce5aa",NA,q0_4_2_1_center_idp_other),
                  q0_4_2_center_idp= ifelse(uuid=="a46a1c10-bf18-4594-a0be-99447fa22116","UKRs006888",q0_4_2_center_idp),
                  q0_4_2_center_idp= ifelse(uuid=="51862558-1b68-466a-8e71-be2817dce5aa","UKRs006888",q0_4_2_center_idp),
                  q2_4_3_1_main_cause_other= ifelse(uuid=="10cef1b0-82ab-4cc2-bd59-bf9ade4fd1b6","evacuated due to injury",q2_4_3_1_main_cause_other), q2_4_3_1_main_cause_other= ifelse(uuid=="7a526721-e59d-4bef-aa69-d80f9a9558b5","For the purpose of the survey",q2_4_3_1_main_cause_other),
                  q10_1_3_relationship_negativ_factors= ifelse(uuid=="644ec1bf-4fec-4e93-b088-676dd2ae52ec",NA,q10_1_3_relationship_negativ_factors),
                  q10_1_3_relationship_negativ_factors= ifelse(uuid=="2a6bacd0-6a4d-420f-9463-cbf8a66cdb48",NA,q10_1_3_relationship_negativ_factors),
                  `q10_1_3_relationship_negativ_factors/a_lack_of_sense_of_trust_between_the_idps_and_the_nonidps`= ifelse(uuid=="644ec1bf-4fec-4e93-b088-676dd2ae52ec",NA,`q10_1_3_relationship_negativ_factors/a_lack_of_sense_of_trust_between_the_idps_and_the_nonidps`),
                  `q10_1_3_relationship_negativ_factors/a_lack_of_sense_of_trust_between_the_idps_and_the_nonidps`= ifelse(uuid=="2a6bacd0-6a4d-420f-9463-cbf8a66cdb48",NA,`q10_1_3_relationship_negativ_factors/a_lack_of_sense_of_trust_between_the_idps_and_the_nonidps`),
                  `q10_1_3_relationship_negativ_factors/different_cultural_identities`= ifelse(uuid=="644ec1bf-4fec-4e93-b088-676dd2ae52ec",NA,`q10_1_3_relationship_negativ_factors/different_cultural_identities`),
                  `q10_1_3_relationship_negativ_factors/different_cultural_identities`= ifelse(uuid=="2a6bacd0-6a4d-420f-9463-cbf8a66cdb48",NA,`q10_1_3_relationship_negativ_factors/different_cultural_identities`),
                  `q10_1_3_relationship_negativ_factors/different_language`= ifelse(uuid=="644ec1bf-4fec-4e93-b088-676dd2ae52ec",NA,`q10_1_3_relationship_negativ_factors/different_language`),
                  `q10_1_3_relationship_negativ_factors/different_language`= ifelse(uuid=="2a6bacd0-6a4d-420f-9463-cbf8a66cdb48",NA,`q10_1_3_relationship_negativ_factors/different_language`),
                  `q10_1_3_relationship_negativ_factors/stereotypes_against_each_other`= ifelse(uuid=="644ec1bf-4fec-4e93-b088-676dd2ae52ec",NA,`q10_1_3_relationship_negativ_factors/stereotypes_against_each_other`),
                  `q10_1_3_relationship_negativ_factors/stereotypes_against_each_other`= ifelse(uuid=="2a6bacd0-6a4d-420f-9463-cbf8a66cdb48",NA,`q10_1_3_relationship_negativ_factors/stereotypes_against_each_other`),
                  `q10_1_3_relationship_negativ_factors/a_lack_of_willingness_from_both_groups_to_interac`= ifelse(uuid=="644ec1bf-4fec-4e93-b088-676dd2ae52ec",NA,`q10_1_3_relationship_negativ_factors/a_lack_of_willingness_from_both_groups_to_interac`),
                  `q10_1_3_relationship_negativ_factors/a_lack_of_willingness_from_both_groups_to_interac`= ifelse(uuid=="2a6bacd0-6a4d-420f-9463-cbf8a66cdb48",NA,`q10_1_3_relationship_negativ_factors/a_lack_of_willingness_from_both_groups_to_interac`),
                  `q10_1_3_relationship_negativ_factors/a_perceived_lack_of_proactivity_from_the_idps_in_trying_to_find_work`= ifelse(uuid=="644ec1bf-4fec-4e93-b088-676dd2ae52ec",NA,`q10_1_3_relationship_negativ_factors/a_perceived_lack_of_proactivity_from_the_idps_in_trying_to_find_work`),
                  `q10_1_3_relationship_negativ_factors/a_perceived_lack_of_proactivity_from_the_idps_in_trying_to_find_work`= ifelse(uuid=="2a6bacd0-6a4d-420f-9463-cbf8a66cdb48",NA,`q10_1_3_relationship_negativ_factors/a_perceived_lack_of_proactivity_from_the_idps_in_trying_to_find_work`),
                  `q10_1_3_relationship_negativ_factors/other`= ifelse(uuid=="644ec1bf-4fec-4e93-b088-676dd2ae52ec",NA,`q10_1_3_relationship_negativ_factors/other`),
                  `q10_1_3_relationship_negativ_factors/other`= ifelse(uuid=="2a6bacd0-6a4d-420f-9463-cbf8a66cdb48",NA,`q10_1_3_relationship_negativ_factors/other`),
                  q10_1_3_1_relationship_negativ_factors_other= ifelse(uuid=="644ec1bf-4fec-4e93-b088-676dd2ae52ec",NA,q10_1_3_1_relationship_negativ_factors_other),
                  q10_1_3_1_relationship_negativ_factors_other= ifelse(uuid=="2a6bacd0-6a4d-420f-9463-cbf8a66cdb48",NA,q10_1_3_1_relationship_negativ_factors_other),
                  `q10_1_3_relationship_negativ_factors/do_not_know`= ifelse(uuid=="644ec1bf-4fec-4e93-b088-676dd2ae52ec",NA,`q10_1_3_relationship_negativ_factors/do_not_know`),
                  `q10_1_3_relationship_negativ_factors/do_not_know`= ifelse(uuid=="2a6bacd0-6a4d-420f-9463-cbf8a66cdb48",NA,`q10_1_3_relationship_negativ_factors/do_not_know`),
                  `q10_1_3_relationship_negativ_factors/prefer_not_to_answer`= ifelse(uuid=="644ec1bf-4fec-4e93-b088-676dd2ae52ec",NA,`q10_1_3_relationship_negativ_factors/prefer_not_to_answer`),
                  `q10_1_3_relationship_negativ_factors/prefer_not_to_answer`= ifelse(uuid=="2a6bacd0-6a4d-420f-9463-cbf8a66cdb48",NA,`q10_1_3_relationship_negativ_factors/prefer_not_to_answer`),
                  q10_2_1_discrimination_idp= ifelse(uuid=="f79999d6-192f-4b9b-aee9-5f613bd4e770","yes_we_feel_discriminated_against_when_trying_to_access_basic_services",q10_2_1_discrimination_idp),
                  `q10_2_1_discrimination_idp/yes_we_feel_discriminated_against_when_trying_to_access_basic_services`= ifelse(uuid=="f79999d6-192f-4b9b-aee9-5f613bd4e770","1",`q10_2_1_discrimination_idp/yes_we_feel_discriminated_against_when_trying_to_access_basic_services`),
                  `q10_2_1_discrimination_idp/other`= ifelse(uuid=="f79999d6-192f-4b9b-aee9-5f613bd4e770","0",`q10_2_1_discrimination_idp/other`),
                  q10_2_1_1_discrimination_idp_other= ifelse(uuid=="f79999d6-192f-4b9b-aee9-5f613bd4e770",NA,q10_2_1_1_discrimination_idp_other),
                  q2_4_3_main_cause= ifelse(uuid=="f79999d6-192f-4b9b-aee9-5f613bd4e770","security_considerations",q2_4_3_main_cause),
                  `q2_4_3_main_cause/security_considerations`= ifelse(uuid=="f79999d6-192f-4b9b-aee9-5f613bd4e770","1",`q2_4_3_main_cause/security_considerations`),
                  `q2_4_3_main_cause/other`= ifelse(uuid=="f79999d6-192f-4b9b-aee9-5f613bd4e770","0",`q2_4_3_main_cause/other`),
                  q2_4_3_1_main_cause_other= ifelse(uuid=="f79999d6-192f-4b9b-aee9-5f613bd4e770",NA,q2_4_3_1_main_cause_other))


  actual_output <-   apply.changes(data = test_data,
                                   clog = cl_log,
                                   is.loop = F,
                                   print_debug = T)

  testthat::expect_equal(actual_output, expected_output)

  # test 2 it should throw an error when we try to make it work like a loop on non-loop data

  testthat::expect_error(apply.changes(data = test_data,
                                       clog = cl_log,
                                       is.loop = T,
                                       print_debug = T))

  # test 3 - expect warning if the cl_log is empty

  testthat::expect_error(apply.changes(data = test_data,
                                       clog = cl_log[0,],
                                       is.loop = F,
                                       print_debug = T))


  # test 4 - test that it gives me a warning if I insert a fake uuid

  cl_fake <- cl_log
  cl_fake[5,]$uuid <- 'fake'

  testthat::expect_warning(
    apply.changes(data = test_data,
                  clog = cl_fake,
                  is.loop = F,
                  print_debug = T)
  )

  # test 5 - test that it gives me a warning if I insert a fake old.value

  cl_fake <- cl_log
  cl_fake[5,]$old.value <- 'fake'

  testthat::expect_warning(
    apply.changes(data = test_data,
                  clog = cl_fake,
                  is.loop = F,
                  print_debug = T)
  )

  # test 6 - test that it works with the loop

  filename <- testthat::test_path("fixtures","data_others.xlsx")
  test_data <- readxl::read_excel(filename, col_types = 'text', sheet = 'loop')%>%
    dplyr::rename(loop_index = `_index`) %>%
    dplyr::filter(loop_index %in% cl_log$loop_index)  # keep only the uuids I'll be changing


  test_data <- test_data[,c('loop_index', intersect(cl_log$variable, names(test_data)))] # keep only the colnames I'll be using


  expected_output <- test_data %>%
    dplyr::mutate(
      q2_3_4_1_employment_situation_last_week_other= ifelse(loop_index=="1040","Employed, but now the enterprise is idle",q2_3_4_1_employment_situation_last_week_other),
      q2_3_4_1_employment_situation_last_week_other= ifelse(loop_index=="95","listed at the place of residence officially",q2_3_4_1_employment_situation_last_week_other),
      q2_3_7_1_sector_working_other= ifelse(loop_index=="331",NA,q2_3_7_1_sector_working_other),
      q2_3_8_1_sector_working_currently_other= ifelse(loop_index=="241",NA,q2_3_8_1_sector_working_currently_other),
      q2_3_7_sector_working= ifelse(loop_index=="331",NA,q2_3_7_sector_working),
      q2_3_8_sector_working_currently= ifelse(loop_index=="241",NA,q2_3_8_sector_working_currently),
      q2_3_3_1_employment_situation_other= ifelse(loop_index=="2802",NA,q2_3_3_1_employment_situation_other),
      q2_3_3_1_employment_situation_other= ifelse(loop_index=="486",NA,q2_3_3_1_employment_situation_other),
      q2_3_3_employment_situation= ifelse(loop_index=="2802","student_not_working",q2_3_3_employment_situation),
      q2_3_3_employment_situation= ifelse(loop_index=="486","officially_employed_permanen_job",q2_3_3_employment_situation)
    )

  actual_output <- apply.changes(data = test_data,
                                 clog = cl_log,
                                 is.loop = T,
                                 print_debug = T)

  testthat::expect_equal(expected_output,actual_output)


  # test 7 - expect warning if working with a loop but is.loop = F but should still run

  testthat::expect_warning(
    apply.changes(data = test_data,
                  clog = cl_log,
                  is.loop = F,
                  print_debug = T)
  )

  # test 8 - expect a warning about a missing loop index if one of them is wrong

  cl_log_fake <- cl_log
  cl_log_fake[50,]$loop_index <- 'fake'


  testthat::expect_warning(
    apply.changes(data = test_data,
                  clog = cl_log_fake,
                  is.loop = T,
                  print_debug = T)
  )

  # test 9 - expect a warning about a missing variable if one of them is wrong

  cl_log_fake <- cl_log
  cl_log_fake[50,]$variable <- 'fake'

  testthat::expect_warning(
    apply.changes(data = test_data,
                  clog = cl_log_fake,
                  is.loop = T,
                  print_debug = T)
  )


  # test 10 - test if it gives a warning if I provide a wrong old.value to it

  cl_log_fake <- cl_log
  cl_log_fake[50,]$old.value <- 'fake'

  testthat::expect_warning(
    apply.changes(data = test_data,
                  clog = cl_log_fake,
                  is.loop = T,
                  print_debug = T)
  )

})


testthat::test_that("undo.changes works", {


  # get the change log file
  filename <- testthat::test_path("fixtures","data_cl_log.xlsx")
  cl_log <- readxl::read_excel(filename, col_types = 'text') %>%
    dplyr::mutate(check = 2)

  # get the dataframe
  filename <- testthat::test_path("fixtures","data_others.xlsx")
  test_data <- readxl::read_excel(filename, col_types = 'text')%>%
    dplyr::rename(uuid = `_uuid`) %>%
    dplyr::filter(uuid %in% cl_log$uuid)  # keep only the uuids I'll be changing


  test_data <- test_data[,c('uuid', intersect(cl_log$variable, names(test_data)))] # keep only the colnames I'll be using

  # general functionality


  temp_output <-   apply.changes(data = test_data,
                                 clog = cl_log,
                                 is.loop = F,
                                 print_debug = T)

  actual_output <-  undo.changes(data = temp_output,
                                 clog = cl_log,
                                 is.loop = F)

  expected_output <- test_data

  testthat::expect_equal(actual_output, expected_output)

})


testthat::test_that("make.logical.check.entry works", {

  # get the dataframe
  filename <- testthat::test_path("fixtures","data_others.xlsx")
  test_data <- readxl::read_excel(filename, col_types = 'text')%>%
    dplyr::rename(uuid = `_uuid`) %>%  # keep only the uuids I'll be changing
    dplyr::filter(uuid %in% c('00126b85-7083-4099-908b-0b93cd5fdde9','0024a26b-2bae-4f70-8a90-23a4b7e75c2d','00289e2e-4f0d-4155-a6e1-932833e3ab71'))


  # test 1 general functionality

  actual_output <- make.logical.check.entry(check = test_data,
                                            id = 2,
                                            is.loop = F,
                                            question.names = c("q2_4_3_main_cause"),
                                            issue = 'test',
                                            date = 'start')


  expected_output <- data.frame(
    uuid = test_data$uuid,
    survey.date = test_data$start,
    check.id = '2',
    variable = 'q2_4_3_main_cause',
    issue = 'test',
    old.value = c(NA_character_, 'security_considerations','security_considerations'),
    new.value = NA_character_,
    invalid = NA_character_,
    explanation = NA_character_
  ) %>%
    dplyr::tibble()


  testthat::expect_equal(actual_output, expected_output)

  # test 2 - more data

  actual_output <- make.logical.check.entry(check = test_data,
                                            id = 2,
                                            is.loop = F,
                                            question.names = c("q2_4_3_main_cause",'q2_4_4_main_reason'),
                                            issue = 'test',
                                            date = 'start')


  expected_output <- data.frame(
    uuid = rep(test_data$uuid,2),
    survey.date = rep(test_data$start,2),
    check.id = '2',
    variable = c(rep('q2_4_3_main_cause',3),rep('q2_4_4_main_reason',3)),
    issue = 'test',
    old.value = c(NA_character_, 'security_considerations','security_considerations',
                  NA_character_,'relative_safety availability_of_accommodation','relative_safety availability_of_accommodation'),
    new.value = NA_character_,
    invalid = NA_character_,
    explanation = NA_character_
  ) %>%
    dplyr::tibble() %>%
    dplyr::arrange(uuid)


  testthat::expect_equal(actual_output, expected_output)


  # test 3 - what if we use a loop ?

  filename <- testthat::test_path("fixtures","data_others.xlsx")
  test_data <- readxl::read_excel(filename, col_types = 'text', sheet = 'loop')%>%
    dplyr::rename(loop_index = `_index`,
                  uuid = `_submission__uuid`) %>%
    dplyr::filter(loop_index %in% c('904',"276","999"))



  actual_output <- make.logical.check.entry(check = test_data,
                                            id = 2,
                                            question.names = c('q2_3_4_employment_situation_last_month','q2_3_3_employment_situation'),
                                            issue = 'test',
                                            is.loop = T)

  expected_output <- data.frame(
    uuid = rep(test_data$uuid,2),
    loop_index = rep(test_data$loop_index,2),
    check.id = '2',
    variable = c(rep('q2_3_4_employment_situation_last_month',3),rep('q2_3_3_employment_situation',3)),
    issue = 'test',
    old.value = c(NA,'doing_housework_looking_after_children_or_other_persons','officially_employed_permanen_job',
                  NA,'doing_housework_looking_after_children_or_other_persons','officially_employed_permanen_job'),
    new.value = NA_character_,
    invalid = NA_character_,
    explanation = NA_character_
  ) %>%
    dplyr::tibble() %>%
    dplyr::arrange(uuid)

  testthat::expect_equal(actual_output, expected_output)

  # test 4 - warning
  testthat::expect_warning(make.logical.check.entry(check = test_data,
                                                    id = 2,
                                                    question.names = c('q2_3_4_employment_situation_last_month','q2_3_3_employment_situation'),
                                                    issue = 'test',
                                                    is.loop = F))


  # test 5 what if the data doesn't have a loop_index column but is a loop?

  test_data <- test_data %>% dplyr::select(-loop_index)

  testthat::expect_error(make.logical.check.entry(check = test_data,
                                                  id = 2,
                                                  question.names = c('q2_3_4_employment_situation_last_month','q2_3_3_employment_situation'),
                                                  issue = 'test',
                                                  is.loop = T))


})


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
    col_enum = c('9297bf6b-e35d-45c9-ae19-02e92f53a678', 'da2516dd-a175-4c75-b941-ffcd7fe247cb', '4ee0da4f-1262-4f50-82d3-7ea66f62ca02'),
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





testthat::test_that("add.to.cleaning.log.other.remove works", {


  filename <- testthat::test_path("fixtures","data_others.xlsx")
  test_data <- readxl::read_excel(filename, col_types = 'text')%>%
    dplyr::rename(uuid = `_uuid`)

  filename <- testthat::test_path("fixtures","other_requests_short.xlsx")
  other_requests <- readxl::read_excel(filename, col_types = 'text')%>%
    dplyr::filter(invalid.v == 'yes',
                  is.na(loop_index))


  # test 1 - general functionality for select_one works - non-loop

  actual_output <- add.to.cleaning.log.other.remove(data = test_data, other_requests = other_requests[1,], is.loop = F)

  expected_output <- data.frame(
    uuid = rep('bd005032-6f63-455f-8202-313583a128b1',2),
    loop_index = rep(NA,2),
    variable = c('q7_2_2_1_initiate_compensation_other','q7_2_2_initiate_compensation'),
    issue = rep('Invalid other response',2),
    old.value = c('Ніхто не знає чи хтось там живе', 'other'),
    new.value = NA
  )

  testthat::expect_equal(actual_output,expected_output)


  # test 2 - general functionality for select_multiple works - loop

  actual_output <- add.to.cleaning.log.other.remove(data = test_data, other_requests = other_requests[3,], is.loop = F)

  expected_output <- data.frame(
    uuid = '644ec1bf-4fec-4e93-b088-676dd2ae52ec',
    loop_index = NA,
    variable = c('q10_1_3_1_relationship_negativ_factors_other',test_data %>%
                   dplyr::select(dplyr::starts_with('q10_1_3_relationship_negativ_factors')) %>%
                   names()),
    issue = 'Invalid other response',
    old.value = c('Ничего не влияет', 'other', rep(0,6),1,0,0),
    new.value = NA
  )
  testthat::expect_equal(actual_output,expected_output)


  # test 3 - we've indicated loop on non-loop data

  testthat::expect_error(
    add.to.cleaning.log.other.remove(data = test_data, other_requests = other_requests[1,], is.loop = T)
  )


  # test 4 - select_multiple functionality with loop data that doesn't need all binaries to be transformed into 0

  # I didn't prepare any cases for this, so I'll insert it here
  other_requests <- rbind(other_requests,
                          data.frame(
                            uuid = "07ceefd6-fd0a-4915-8899-1f250f8bd0ae",
                            loop_index = NA,
                            name = 'q2_4_3_1_main_cause_other',
                            ref.name = 'q2_4_3_main_cause',
                            full.label = 'test',
                            ref.type = 'select_multiple',
                            choices.label = 'test',
                            response.uk = "Ракета влучила в дах",
                            response.en = 'test',
                            true.v = NA,
                            existing.v = NA,
                            invalid.v = 'yes'
                          ))


  actual_output <- add.to.cleaning.log.other.remove(data = test_data, other_requests = other_requests[5,], is.loop = F)

  expected_output <- data.frame(
    uuid = rep('07ceefd6-fd0a-4915-8899-1f250f8bd0ae',3),
    loop_index = rep(NA,3),
    variable = c('q2_4_3_1_main_cause_other','q2_4_3_main_cause','q2_4_3_main_cause/other'),
    issue = rep('Invalid other response',3),
    old.value = c('Ракета влучила в дах', "security_considerations bad_standards_of_living other",'1'),
    new.value = c(NA,"security_considerations bad_standards_of_living",0)
  )

  testthat::expect_equal(actual_output,expected_output)

  # test 5 - basic select_one functionality with a loop

  filename <- testthat::test_path("fixtures","data_others.xlsx")
  test_data <- readxl::read_excel(filename, col_types = 'text', sheet = 'loop')%>%
    dplyr::rename(loop_index = `_index`,
                  uuid = `_submission__uuid`)

  filename <- testthat::test_path("fixtures","other_requests_short.xlsx")
  other_requests <- readxl::read_excel(filename, col_types = 'text')%>%
    dplyr::filter(invalid.v == 'yes',
                  !is.na(loop_index))


  actual_output <- add.to.cleaning.log.other.remove(data = test_data, other_requests = other_requests[1,], is.loop = T)

  expected_output <- data.frame(
    uuid = rep('c25aa98d-36e7-4788-bd3b-b4da3b4a6415',2),
    loop_index = rep('331',2),
    variable = c('q2_3_7_1_sector_working_other','q2_3_7_sector_working'),
    issue = rep('Invalid other response',2),
    old.value = c('В декретном отпуске', 'other'),
    new.value = NA
  )

  testthat::expect_equal(actual_output,expected_output)

  # test 6 - basic select_multiple functionality with a loop

  actual_output <- add.to.cleaning.log.other.remove(data = test_data, other_requests = other_requests[3,], is.loop = T)

  expected_output <- data.frame(
    uuid = 'e489957a-65d5-4777-b40b-6084a9559b82',
    loop_index = '1210',
    variable = c('q2_1_4_1_members_vulnerabilities_other',test_data %>%
                   dplyr::select(dplyr::starts_with('q2_1_4_members_vulnerabilities')) %>%
                   names()),
    issue = 'Invalid other response',
    old.value = c('У лікарні не був дуже давно', 'other', rep(0,8),1,0,0),
    new.value = NA
  )

  testthat::expect_equal(actual_output,expected_output)

  # test 7 - shoots a warning if you're working with a loop but didn't say it's a loop

  testthat::expect_warning(
    add.to.cleaning.log.other.remove(data = test_data, other_requests = other_requests[3,], is.loop = F)
  )

  # test 8 - wrong entry

  other_requests <- rbind(other_requests,
                          data.frame(
                            uuid = "07ceefd6-fd0a-4915-8899-1f250f8bd0ae",
                            loop_index = NA,
                            name = 'fake',
                            ref.name = 'fake',
                            full.label = 'test',
                            ref.type = 'select_multiple',
                            choices.label = 'test',
                            response.uk = "Ракета влучила в дах",
                            response.en = 'test',
                            true.v = NA,
                            existing.v = NA,
                            invalid.v = 'yes'
                          ))

  testthat::expect_error(
    add.to.cleaning.log.other.remove(data = test_data, other_requests = other_requests[5,], is.loop = T)
  )
})





testthat::test_that("vectorized.add.to.cleaning.log.other.remove works", {

  filename <- testthat::test_path("fixtures","data_others.xlsx")
  test_data <- readxl::read_excel(filename, col_types = 'text')%>%
    dplyr::rename(uuid = `_uuid`)

  test_data_l <- readxl::read_excel(filename, col_types = 'text', sheet = 'loop')%>%
    dplyr::rename(loop_index = `_index`,
                  uuid = `_submission__uuid`)

  filename <- testthat::test_path("fixtures","other_requests_short.xlsx")
  other_requests <- readxl::read_excel(filename, col_types = 'text')%>%
    dplyr::filter(invalid.v == 'yes')


  # test 1 - basic functionality - non-loop data

  actual_output <- vectorized.add.to.cleaning.log.other.remove(data = test_data,is.loop=F,
                                                               other_requests = other_requests%>% dplyr::filter(is.na(loop_index)))


  expected_output <- data.frame(
    uuid = c(rep('bd005032-6f63-455f-8202-313583a128b1',2),rep('f903166e-abc9-4258-848b-77ada6987d31',2),
             rep('644ec1bf-4fec-4e93-b088-676dd2ae52ec',11), rep('2a6bacd0-6a4d-420f-9463-cbf8a66cdb48',11)),
    loop_index = rep(NA,26),
    variable = c(rep(c('q7_2_2_1_initiate_compensation_other','q7_2_2_initiate_compensation'),2),
                 rep(c(
                   'q10_1_3_1_relationship_negativ_factors_other',test_data %>%
                     dplyr::select(dplyr::starts_with('q10_1_3_relationship_negativ_factors')) %>%
                     names()),2)
    ),
    issue = rep('Invalid other response',26),
    old.value = c('Ніхто не знає чи хтось там живе', 'other','Респондент не верит в помощь от государства','other',
                  'Ничего не влияет', 'other', rep(0,6),1,0,0, 'Нет негативных факторов','other', rep(0,6),1,0,0),
    new.value = NA
  )

  testthat::expect_equal(actual_output,expected_output)

  # test 2 - called loop functionality on non-loop data

  testthat::expect_error(
  vectorized.add.to.cleaning.log.other.remove(data = test_data,is.loop=T,
                                              other_requests = other_requests%>% dplyr::filter(is.na(loop_index)))
  )

  # test 3 - basic functionality - loop data

  actual_output <- vectorized.add.to.cleaning.log.other.remove(data = test_data_l,is.loop=T,
                                                               other_requests = other_requests%>% dplyr::filter(!is.na(loop_index)))


  expected_output <- data.frame(
    uuid = c(rep('c25aa98d-36e7-4788-bd3b-b4da3b4a6415',2), rep('abef16e7-ac19-47d5-9914-16cc40580b22',2),
             rep('e489957a-65d5-4777-b40b-6084a9559b82',13),rep('2a7ab223-bc9e-4f15-ace2-feb857ac7742',13)),
    loop_index = c(rep('331',2), rep('241',2), rep('1210',13),rep('670',13)),
    variable = c('q2_3_7_1_sector_working_other','q2_3_7_sector_working',
                 'q2_3_8_1_sector_working_currently_other','q2_3_8_sector_working_currently',
                 rep(c('q2_1_4_1_members_vulnerabilities_other',test_data_l %>%
                         dplyr::select(dplyr::starts_with('q2_1_4_members_vulnerabilities')) %>%
                         names()),2)
    ),
    issue = rep('Invalid other response',30),
    old.value = c('В декретном отпуске', 'other','Не працюю','other',
                  'У лікарні не був дуже давно', 'other', rep(0,8),1,0,0,
                  'Важке інфікційне захворювання', 'other', rep(0,8),1,0,0),
    new.value = NA
  )

   testthat::expect_equal(actual_output,expected_output)

  # test 4 - expect error if out other.requests file was not good enough
  other_requests_f <- other_requests
  other_requests_f[1,]$invalid.v <- ''

  testthat::expect_error(
    vectorized.add.to.cleaning.log.other.remove(data = test_data_l, is.loop = T,
                                                other_requests = other_requests_f%>% dplyr::filter(!is.na(loop_index)))

  )

  # test 5 - expect a warning when we run the file with an incorrect other.requests. But it should still give is a correct result

  testthat::expect_warning(
    actual_output <- vectorized.add.to.cleaning.log.other.remove(data = test_data_l,is.loop =T,
                                                                 other_requests = other_requests)
  )

  expected_output <- data.frame(
    uuid = c(rep('c25aa98d-36e7-4788-bd3b-b4da3b4a6415',2), rep('abef16e7-ac19-47d5-9914-16cc40580b22',2),
             rep('e489957a-65d5-4777-b40b-6084a9559b82',13),rep('2a7ab223-bc9e-4f15-ace2-feb857ac7742',13)),
    loop_index = c(rep('331',2), rep('241',2), rep('1210',13),rep('670',13)),
    variable = c('q2_3_7_1_sector_working_other','q2_3_7_sector_working',
                 'q2_3_8_1_sector_working_currently_other','q2_3_8_sector_working_currently',
                 rep(c('q2_1_4_1_members_vulnerabilities_other',test_data_l %>%
                         dplyr::select(dplyr::starts_with('q2_1_4_members_vulnerabilities')) %>%
                         names()),2)
    ),
    issue = rep('Invalid other response',30),
    old.value = c('В декретном отпуске', 'other','Не працюю','other',
                  'У лікарні не був дуже давно', 'other', rep(0,8),1,0,0,
                  'Важке інфікційне захворювання', 'other', rep(0,8),1,0,0),
    new.value = NA
  )

  testthat::expect_equal(actual_output,expected_output)

  # test 6 - provided non-loop data as a loop

  testthat::expect_error(
    vectorized.add.to.cleaning.log.other.remove(data = test_data,is.loop=T,
                                                other_requests = other_requests%>% dplyr::filter(!is.na(loop_index)))
  )

})



testthat::test_that("add.to.cleaning.log.trans.remove works", {


  filename <- testthat::test_path("fixtures","other_requests_short.xlsx")
  other_requests <- readxl::read_excel(filename, col_types = 'text')%>%
    dplyr::filter(invalid.v == 'yes')


  actual_output <- add.to.cleaning.log.trans.remove(other_requests[c(1,3),])

  expected_output <- data.frame(
    uuid = c('c25aa98d-36e7-4788-bd3b-b4da3b4a6415','bd005032-6f63-455f-8202-313583a128b1'),
    loop_index= c('331',NA),
    variable=c('q2_3_7_1_sector_working_other','q7_2_2_1_initiate_compensation_other'),
    issue= "Invalid response",
    old.value=c('В декретном отпуске','Ніхто не знає чи хтось там живе'),
    new.value=NA

  )

  testthat::expect_equal(actual_output,expected_output)

})

testthat::test_that("add.to.cleaning.log.other.recode.one works", {


  filename <- testthat::test_path("fixtures","data_others.xlsx")
  test_data <- readxl::read_excel(filename, col_types = 'text')%>%
    dplyr::rename(uuid = `_uuid`)

  test_data_l <- readxl::read_excel(filename, col_types = 'text', sheet = 'loop')%>%
    dplyr::rename(loop_index = `_index`,
                  uuid = `_submission__uuid`)

  filename <- testthat::test_path("fixtures","other_requests_short.xlsx")
  other_requests <- readxl::read_excel(filename, col_types = 'text')%>%
    dplyr::filter(ref.type == 'select_one',
                  !is.na(existing.v))

  # load the tool data
  filename <- testthat::test_path("fixtures","tool_others.xlsx")
  label_colname <- "label::English"
  tool.survey <- utilityR::load.tool.survey(filename,label_colname)

  # get the tool.choices db
  filename <- testthat::test_path("fixtures","tool_others.xlsx")
  tool.choices <- readxl::read_excel(filename, sheet = 'choices', col_types = 'text')


  # test 1 - basic functionality on non-loop data

  actual_output <- add.to.cleaning.log.other.recode.one(
    data = test_data,
    other_requests = other_requests[3,],
    is.loop = F,
    tool.survey = tool.survey,
    tool.choices = tool.choices
  )

  expected_output <- data.frame(
    uuid = 'a46a1c10-bf18-4594-a0be-99447fa22116',
    loop_index = NA_character_,
    variable = c('q0_4_2_1_center_idp_other','q0_4_2_center_idp'),
    issue= "Recoding other response",
    old.value = c('29','other'),
    new.value = c(NA,'UKRs006888')
  )

  testthat::expect_equal(actual_output,expected_output)

  # test 2 trying to run a loop on non loop data

  testthat::expect_error(
    actual_output <- add.to.cleaning.log.other.recode.one(
      data = test_data,
      other_requests = other_requests[3,],
      is.loop = T,
      tool.survey = tool.survey,
      tool.choices = tool.choices
    )
  )

  # test 3 - basic functionality on loop data

  actual_output <- add.to.cleaning.log.other.recode.one(
    data = test_data_l,
    other_requests = other_requests[1,],
    is.loop = T,
    tool.survey = tool.survey,
    tool.choices = tool.choices
  )

  expected_output <- data.frame(
    uuid = '3dc15f20-8964-419c-9ab0-f7b9d367120e',
    loop_index = '2802',
    variable = c('q2_3_3_1_employment_situation_other','q2_3_3_employment_situation'),
    issue= "Recoding other response",
    old.value = c('17 років, навчання в школі','other'),
    new.value = c(NA,'student_not_working')
  )

  testthat::expect_equal(actual_output,expected_output)


  # test 4 - indicating non loop on loop data

  testthat::expect_warning(
    actual_output <- add.to.cleaning.log.other.recode.one(
      data = test_data_l,
      other_requests = other_requests[1,],
      is.loop = F,
      tool.survey = tool.survey,
      tool.choices = tool.choices
    )
  )

  # test 5 - multiple choices on select_one produce an error
  other_requests_f <- other_requests
  other_requests_f[1,'existing.v'] <- 'fake1;fake2'

  testthat::expect_error(
    add.to.cleaning.log.other.recode.one(
      data = test_data_l,
      other_requests = other_requests_f[1,],
      is.loop = T,
      tool.survey = tool.survey,
      tool.choices = tool.choices
    )
  )

  # test 6 - wrong choices produce an error
  other_requests_f[1,'existing.v'] <- 'fake1'

  testthat::expect_error(
    add.to.cleaning.log.other.recode.one(
      data = test_data_l,
      other_requests = other_requests_f[1,],
      is.loop = T,
      tool.survey = tool.survey,
      tool.choices = tool.choices
    )
  )
  # test 7 - non-existing variables produce an error
  other_requests_f[1,'name'] <- 'fake1'

  testthat::expect_error(
    add.to.cleaning.log.other.recode.one(
      data = test_data_l,
      other_requests = other_requests_f[1,],
      is.loop = T,
      tool.survey = tool.survey,
      tool.choices = tool.choices
    )
  )

  # test 8  - wrong 'existing column
  other_requests_f <- other_requests
  other_requests_f <- other_requests_f %>%  dplyr::rename(existing = existing.v)

  testthat::expect_error(
    add.to.cleaning.log.other.recode.one(
      data = test_data_l,
      other_requests = other_requests_f[1,],
      is.loop = T,
      tool.survey = tool.survey,
      tool.choices = tool.choices
    )
  )

})


testthat::test_that("add.to.cleaning.log.other.recode.multiple works", {


  filename <- testthat::test_path("fixtures","data_others.xlsx")
  test_data <- readxl::read_excel(filename, col_types = 'text')%>%
    dplyr::rename(uuid = `_uuid`)

  test_data_l <- readxl::read_excel(filename, col_types = 'text', sheet = 'loop')%>%
    dplyr::rename(loop_index = `_index`,
                  uuid = `_submission__uuid`)

  filename <- testthat::test_path("fixtures","other_requests_short.xlsx")
  other_requests <- readxl::read_excel(filename, col_types = 'text')%>%
    dplyr::filter(ref.type == 'select_multiple',
                  !is.na(existing.v))

  # load the tool data
  filename <- testthat::test_path("fixtures","tool_others.xlsx")
  label_colname <- "label::English"
  tool.survey <- utilityR::load.tool.survey(filename,label_colname)

  # get the tool.choices db
  filename <- testthat::test_path("fixtures","tool_others.xlsx")
  tool.choices <- readxl::read_excel(filename, sheet = 'choices', col_types = 'text')


  # test 1 - basic functionality on non-loop data

  actual_output <- add.to.cleaning.log.other.recode.multiple(
    data = test_data,
    other_requests = other_requests[1,],
    is.loop = F,
    tool.survey = tool.survey,
    tool.choices = tool.choices
  )

  expected_output <- data.frame(
    uuid = 'f79999d6-192f-4b9b-aee9-5f613bd4e770',
    loop_index = NA_character_,
    variable = c('q10_2_1_1_discrimination_idp_other','q10_2_1_discrimination_idp/other',
                 'q10_2_1_discrimination_idp/yes_we_feel_discriminated_against_when_trying_to_access_basic_services',
                 'q10_2_1_discrimination_idp'
    ),
    issue = "Recoding other response",
    old.value = c('Так зі сторони проживаючих тут студентів','1','0','other'),
    new.value = c(NA,'0','1','yes_we_feel_discriminated_against_when_trying_to_access_basic_services')
  )

  testthat::expect_equal(actual_output,expected_output)

  # test 2 - trying to call loop for non-loop data

  testthat::expect_error(
    add.to.cleaning.log.other.recode.multiple(
      data = test_data,
      other_requests = other_requests[1,],
      is.loop = T,
      tool.survey = tool.survey,
      tool.choices = tool.choices
    )
  )

  # test 3 - loop data, cumulative only has the 'other' response

  actual_output <- add.to.cleaning.log.other.recode.multiple(
    data = test_data_l,
    other_requests = other_requests[3,],
    is.loop = T,
    tool.survey = tool.survey,
    tool.choices = tool.choices
  )

  expected_output <- data.frame(
    uuid = '5f5bac4d-b250-41dc-93de-1b27043a2869',
    loop_index = '494',
    variable = c('q2_1_4_1_members_vulnerabilities_other','q2_1_4_members_vulnerabilities/other',
                 'q2_1_4_members_vulnerabilities/person_with_disabilities',
                 'q2_1_4_members_vulnerabilities'
    ),
    issue = "Recoding other response",
    old.value = c('3 група інвалідності','1','0','other'),
    new.value = c(NA,'0','1','person_with_disabilities')
  )
  testthat::expect_equal(actual_output,expected_output)

  # test 4 - loop data, cumulative has the 'other' response + some others

  actual_output <- add.to.cleaning.log.other.recode.multiple(
    data = test_data_l,
    other_requests = other_requests[4,],
    is.loop = T,
    tool.survey = tool.survey,
    tool.choices = tool.choices
  )

  expected_output <- data.frame(
    uuid = 'efab8f40-dcb4-47c6-ba3f-fd89237a6f14',
    loop_index = '1168',
    variable = c('q2_1_4_1_members_vulnerabilities_other','q2_1_4_members_vulnerabilities/other',
                 'q2_1_4_members_vulnerabilities/person_with_disabilities',
                 'q2_1_4_members_vulnerabilities'
    ),
    issue = "Recoding other response",
    old.value = c('Оформлюють інвалідність','1','0','chronic_illness_which_affects_the_quality_of_life other'),
    new.value = c(NA,'0','1','chronic_illness_which_affects_the_quality_of_life person_with_disabilities')
  )
  testthat::expect_equal(actual_output,expected_output)

  # test 5 calling non-loop on loop value
  testthat::expect_warning(
    add.to.cleaning.log.other.recode.multiple(
      data = test_data_l,
      other_requests = other_requests[4,],
      is.loop = F,
      tool.survey = tool.survey,
      tool.choices = tool.choices
    ))

  # test 6 using a fake variable should produce an error
  other_requests_f <- other_requests
  other_requests_f$name <- 'fake'

  testthat::expect_error(
    add.to.cleaning.log.other.recode.multiple(
      data = test_data_l,
      other_requests = other_requests_f[4,],
      is.loop = T,
      tool.survey = tool.survey,
      tool.choices = tool.choices)
  )

  # test 7, multiple choices from select_multiple fit our requirements

  other_requests_f <- other_requests
  other_requests_f[4,]$existing.v <- paste0(other_requests_f[4,]$existing.v,'; Older person')

  actual_output <- add.to.cleaning.log.other.recode.multiple(
    data = test_data_l,
    other_requests = other_requests_f[4,],
    is.loop = T,
    tool.survey = tool.survey,
    tool.choices = tool.choices)

  expected_output <- data.frame(
    uuid = 'efab8f40-dcb4-47c6-ba3f-fd89237a6f14',
    loop_index = '1168',
    variable = c('q2_1_4_1_members_vulnerabilities_other','q2_1_4_members_vulnerabilities/other',
                 'q2_1_4_members_vulnerabilities/person_with_disabilities',
                 'q2_1_4_members_vulnerabilities/older_person',
                 'q2_1_4_members_vulnerabilities'
    ),
    issue = "Recoding other response",
    old.value = c('Оформлюють інвалідність','1','0','0','chronic_illness_which_affects_the_quality_of_life other'),
    new.value = c(NA,'0','1','1','chronic_illness_which_affects_the_quality_of_life older_person person_with_disabilities')
  )


  testthat::expect_equal(actual_output,expected_output)

  # test 8  - using a wrong choice name when recoding


  other_requests_f <- other_requests
  other_requests_f[4,]$existing.v <- 'fake'

  testthat::expect_error(
    add.to.cleaning.log.other.recode.multiple(
      data = test_data_l,
      other_requests = other_requests_f[4,],
      is.loop = T,
      tool.survey = tool.survey,
      tool.choices = tool.choices)
  )


  # test 9  - wrong 'existing column
  other_requests_f <- other_requests
  other_requests_f <- other_requests_f %>%  dplyr::rename(existing = existing.v)

  testthat::expect_error(
    add.to.cleaning.log.other.recode.multiple(
      data = test_data_l,
      other_requests = other_requests_f[4,],
      is.loop = T,
      tool.survey = tool.survey,
      tool.choices = tool.choices)
  )

})




testthat::test_that("add.to.cleaning.log.other.recode works", {


  filename <- testthat::test_path("fixtures","data_others.xlsx")
  test_data <- readxl::read_excel(filename, col_types = 'text')%>%
    dplyr::rename(uuid = `_uuid`)

  test_data_l <- readxl::read_excel(filename, col_types = 'text', sheet = 'loop')%>%
    dplyr::rename(loop_index = `_index`,
                  uuid = `_submission__uuid`)

  filename <- testthat::test_path("fixtures","other_requests_short.xlsx")
  other_requests <- readxl::read_excel(filename, col_types = 'text')%>%
    dplyr::filter(!is.na(existing.v))

  # load the tool data
  filename <- testthat::test_path("fixtures","tool_others.xlsx")
  label_colname <- "label::English"
  tool.survey <- utilityR::load.tool.survey(filename,label_colname)

  # get the tool.choices db
  filename <- testthat::test_path("fixtures","tool_others.xlsx")
  tool.choices <- readxl::read_excel(filename, sheet = 'choices', col_types = 'text')

  # test 1 - select_one loop

  actual_output <- add.to.cleaning.log.other.recode(
    data = test_data_l, other_requests= other_requests[1,],
    is.loop = T,
    tool.survey=tool.survey,tool.choices=tool.choices
  )

  expected_output <- data.frame(
    uuid = rep('3dc15f20-8964-419c-9ab0-f7b9d367120e',2),
    loop_index = rep('2802',2),
    variable = c('q2_3_3_1_employment_situation_other','q2_3_3_employment_situation'),
    issue = rep('Recoding other response',2),
    old.value = c('17 років, навчання в школі', 'other'),
    new.value = c(NA,'student_not_working')
  )


  testthat::expect_equal(actual_output,expected_output)

  # test 2 - select_multiple loop


  actual_output <- add.to.cleaning.log.other.recode(
    data = test_data_l, other_requests= other_requests[7,],
    is.loop = T,
    tool.survey=tool.survey,tool.choices=tool.choices
  )

  expected_output <- data.frame(
    uuid = '5f5bac4d-b250-41dc-93de-1b27043a2869',
    loop_index = '494',
    variable = c('q2_1_4_1_members_vulnerabilities_other','q2_1_4_members_vulnerabilities/other',
                 'q2_1_4_members_vulnerabilities/person_with_disabilities',
                 'q2_1_4_members_vulnerabilities'
    ),
    issue = "Recoding other response",
    old.value = c('3 група інвалідності','1','0','other'),
    new.value = c(NA,'0','1','person_with_disabilities')
  )
  testthat::expect_equal(actual_output,expected_output)

  # test 3 - non-loop data select_one

  actual_output <- add.to.cleaning.log.other.recode(
    data = test_data, other_requests= other_requests[3,],
    is.loop = F,
    tool.survey=tool.survey,tool.choices=tool.choices
  )

  expected_output <- data.frame(
    uuid = 'a46a1c10-bf18-4594-a0be-99447fa22116',
    loop_index = NA_character_,
    variable = c('q0_4_2_1_center_idp_other','q0_4_2_center_idp'),
    issue= "Recoding other response",
    old.value = c('29','other'),
    new.value = c(NA,'UKRs006888')
  )

  testthat::expect_equal(actual_output,expected_output)

  # test 4 - non-loop data select_one

  actual_output <- add.to.cleaning.log.other.recode(
    data = test_data, other_requests= other_requests[5,],
    is.loop = F,
    tool.survey=tool.survey,tool.choices=tool.choices
  )

  expected_output <- data.frame(
    uuid = 'f79999d6-192f-4b9b-aee9-5f613bd4e770',
    loop_index = NA_character_,
    variable = c('q10_2_1_1_discrimination_idp_other','q10_2_1_discrimination_idp/other',
                 'q10_2_1_discrimination_idp/yes_we_feel_discriminated_against_when_trying_to_access_basic_services',
                 'q10_2_1_discrimination_idp'
    ),
    issue = "Recoding other response",
    old.value = c('Так зі сторони проживаючих тут студентів','1','0','other'),
    new.value = c(NA,'0','1','yes_we_feel_discriminated_against_when_trying_to_access_basic_services')
  )

  testthat::expect_equal(actual_output,expected_output)

})



testthat::test_that("add.to.cleaning.log.other.recode works", {


  filename <- testthat::test_path("fixtures","data_others.xlsx")
  test_data <- readxl::read_excel(filename, col_types = 'text')%>%
    dplyr::rename(uuid = `_uuid`)

  test_data_l <- readxl::read_excel(filename, col_types = 'text', sheet = 'loop')%>%
    dplyr::rename(loop_index = `_index`,
                  uuid = `_submission__uuid`)

  filename <- testthat::test_path("fixtures","other_requests_short.xlsx")
  other_requests <- readxl::read_excel(filename, col_types = 'text')%>%
    dplyr::filter(!is.na(existing.v))

  # load the tool data
  filename <- testthat::test_path("fixtures","tool_others.xlsx")
  label_colname <- "label::English"
  tool.survey <- utilityR::load.tool.survey(filename,label_colname)

  # get the tool.choices db
  filename <- testthat::test_path("fixtures","tool_others.xlsx")
  tool.choices <- readxl::read_excel(filename, sheet = 'choices', col_types = 'text')

  # test 1 - loop

  actual_output <- vectorized.add.to.cleaning.log.other.recode(
    data = test_data_l,
    other_requests= other_requests[c(1,2,7,8),],
    is.loop = T,
    tool.survey=tool.survey,
    tool.choices=tool.choices
  )


  expected_output <- data.frame(
    uuid = c(rep('3dc15f20-8964-419c-9ab0-f7b9d367120e',2),rep('3402ebff-f400-478b-a77e-0153eeb67a86',2),
             rep('5f5bac4d-b250-41dc-93de-1b27043a2869',4),rep('efab8f40-dcb4-47c6-ba3f-fd89237a6f14',4)),
    loop_index = c(rep('2802',2),rep('486',2),rep('494',4),rep('1168',4)),
    variable = c(rep(c('q2_3_3_1_employment_situation_other','q2_3_3_employment_situation'),2),
                 rep(c('q2_1_4_1_members_vulnerabilities_other','q2_1_4_members_vulnerabilities/other',
                 'q2_1_4_members_vulnerabilities/person_with_disabilities',
                 'q2_1_4_members_vulnerabilities'),2)),
    issue= "Recoding other response",
    old.value = c('17 років, навчання в школі','other','Часний підприємиць','other','3 група інвалідності',
                  '1','0','other','Оформлюють інвалідність','1','0','chronic_illness_which_affects_the_quality_of_life other'),
    new.value = c(NA,'student_not_working',NA,'officially_employed_permanen_job',NA,'0','1','person_with_disabilities',
                  NA,'0','1','chronic_illness_which_affects_the_quality_of_life person_with_disabilities')
  )

  testthat::expect_equal(actual_output,expected_output)

  # test 2
  testthat::expect_warning(
    actual_output2 <- vectorized.add.to.cleaning.log.other.recode(
    data = test_data_l,
    other_requests= other_requests[c(1,2,7,8),],
    is.loop = F,
    tool.survey=tool.survey,
    tool.choices=tool.choices
  ))
  # but still equal to the output we need
  testthat::expect_equal(actual_output2,expected_output)


  # test 3 - non_loop

  actual_output <- vectorized.add.to.cleaning.log.other.recode(
    data = test_data,
    other_requests= other_requests[c(3:6),],
    is.loop = F,
    tool.survey=tool.survey,
    tool.choices=tool.choices
  )


  expected_output <- data.frame(
    uuid = c(rep('a46a1c10-bf18-4594-a0be-99447fa22116',2),rep('51862558-1b68-466a-8e71-be2817dce5aa',2),
             rep('f79999d6-192f-4b9b-aee9-5f613bd4e770',8)),
    loop_index = NA_character_,
    variable = c(rep(c('q0_4_2_1_center_idp_other','q0_4_2_center_idp'),2),
                 'q10_2_1_1_discrimination_idp_other',"q10_2_1_discrimination_idp/other" ,
                 "q10_2_1_discrimination_idp/yes_we_feel_discriminated_against_when_trying_to_access_basic_services",
                 'q10_2_1_discrimination_idp',
                 'q2_4_3_1_main_cause_other',"q2_4_3_main_cause/other","q2_4_3_main_cause/security_considerations",
                 'q2_4_3_main_cause'
                 ),
    issue= "Recoding other response",
    old.value = c('29','other','29','other','Так зі сторони проживаючих тут студентів','1','0','other',
                  'Окупована територія','1','0','other'),
    new.value = c(NA,'UKRs006888',NA,'UKRs006888',NA,'0','1',
                  'yes_we_feel_discriminated_against_when_trying_to_access_basic_services',
                  NA,'0','1','security_considerations'
                  )
  )

  testthat::expect_equal(actual_output,expected_output)

  # test 4 - calling loop on non loop data
  testthat::expect_error(
  vectorized.add.to.cleaning.log.other.recode(
    data = test_data,
    other_requests= other_requests[c(3:6),],
    is.loop = T,
    tool.survey=tool.survey,
    tool.choices=tool.choices
  ))

  # test 5 - some of the rows of the existing variable are empty
  other_requests_f <- other_requests
  other_requests_f[3,'existing.v'] <- NA

  testthat::expect_error(
    vectorized.add.to.cleaning.log.other.recode(
      data = test_data,
      other_requests= other_requests_f[c(3:6),],
      is.loop = T,
      tool.survey=tool.survey,
      tool.choices=tool.choices
    )
    )

  # test 6 some of the columns provided in the other_requests are not in the data

  testthat::expect_warning(
  actual_output3 <- vectorized.add.to.cleaning.log.other.recode(
    data = test_data,
    other_requests= other_requests[c(2:6),],
    is.loop = F,
    tool.survey=tool.survey,
    tool.choices=tool.choices
  ))
  # but still it should give us the correct resuls

  testthat::expect_equal(actual_output3,expected_output)



})

