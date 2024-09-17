testthat::test_that('anomaly_test_tmln_int works, test 1 - basic functionality',{

  test_audit_path <- testthat::test_path('fixtures/audits_test')

  test_path <- testthat::test_path('fixtures',"data_others.xlsx")
  test_data <-  readxl::read_excel(test_path, col_types = 'text')%>%
    dplyr::rename(uuid = `_uuid`)

  test_audits <- load.audit.files(dir.audits = test_audit_path,
                                    uuids = test_data$uuid,add.uuid2=F)


  ls_test <- c("q2_1_1_members_age", "q2_1_2_members_sex", "q2_1_3_members_education_level",
               "q2_1_4_members_vulnerabilities")


  actual_result <- anomaly_test_tmln_int(ls = ls_test,audit = test_audits,data = test_data,
                        geo_column = 'q2_4_1_oblast',enum_name = 'q0_2_enum_id',n_limit = 1) %>%
    dplyr::mutate(issues_per_questions= round(issues_per_questions,2))


  expected_result <- data.frame(
    q0_2_enum_id = c('enum_10','enum_13','enum_23'),
    q2_4_1_oblast  = NA_character_,
    total_respondents = 2,
    total_questions = c(20,20,21),
    issues = c(1,0,1),
    issues_per_questions = c(0.03,0,0.04),
    problematic_interview = 0,
    problematic_interviews_perc = 0,
    list_used = 'q2_1'
  )%>%
    dplyr::tibble()


  testthat::expect_equal(actual_result, expected_result)

})

testthat::test_that('anomaly_test_tmln_int works, test 2 - returns an empty frame when needed',{

  test_audit_path <- testthat::test_path('fixtures/audits_test')

  test_path <- testthat::test_path('fixtures',"data_others.xlsx")
  test_data <-  readxl::read_excel(test_path, col_types = 'text')%>%
    dplyr::rename(uuid = `_uuid`)

  test_audits <- load.audit.files(dir.audits = test_audit_path,
                                  uuids = test_data$uuid,add.uuid2=F)


  ls_test <- c("q2_1_1_members_age", "q2_1_2_members_sex", "q2_1_3_members_education_level",
               "q2_1_4_members_vulnerabilities")


  actual_result <- anomaly_test_tmln_int(ls = ls_test,audit = test_audits,data = test_data,
                                         geo_column = 'q2_4_1_oblast',enum_name = 'q0_2_enum_id',n_limit = 10)


  expected_result <- dplyr::tibble()


  testthat::expect_equal(actual_result, expected_result)

})



testthat::test_that('anomaly_test_tmln_int works, test 3 - returns an error if a wrong variable is fed',{

  test_audit_path <- testthat::test_path('fixtures/audits_test')

  test_path <- testthat::test_path('fixtures',"data_others.xlsx")
  test_data <-  readxl::read_excel(test_path, col_types = 'text')%>%
    dplyr::rename(uuid = `_uuid`)

  test_audits <- load.audit.files(dir.audits = test_audit_path,
                                  uuids = test_data$uuid,add.uuid2=F)


  ls_test <- c("test")

  testthat::expect_error(
  anomaly_test_tmln_int(ls = ls_test,audit = test_audits,data = test_data,
                                         geo_column = 'q2_4_1_oblast',enum_name = 'q0_2_enum_id',n_limit = 10),
  "Error: some of the questions you've entered are not present in your data. please double check: test"
  )


})

testthat::test_that('anomaly_test_tmln_int works, test 4 - returns an error if a non numeric parameter is fed',{

  test_audit_path <- testthat::test_path('fixtures/audits_test')

  test_path <- testthat::test_path('fixtures',"data_others.xlsx")
  test_data <-  readxl::read_excel(test_path, col_types = 'text')%>%
    dplyr::rename(uuid = `_uuid`)

  test_audits <- load.audit.files(dir.audits = test_audit_path,
                                  uuids = test_data$uuid,add.uuid2=F)


  ls_test <- c("q2_1_1_members_age")

  testthat::expect_error(
    anomaly_test_tmln_int(ls = ls_test,audit = test_audits,data = test_data,
                          geo_column = 'q2_4_1_oblast',enum_name = 'q0_2_enum_id',n_limit = '10'),
    "The ipq_n or n_limit you've chosen is non-numeric. Please fix this"
  )
})


testthat::test_that('anomaly_test_tmln_int works, test 5 - returns an error uuid is not present',{

  test_audit_path <- testthat::test_path('fixtures/audits_test')

  test_path <- testthat::test_path('fixtures',"data_others.xlsx")
  test_data <-  readxl::read_excel(test_path, col_types = 'text')

  test_audits <- load.audit.files(dir.audits = test_audit_path,
                                  uuids = test_data$`_uuid`,add.uuid2=F)


  ls_test <- c("q2_1_1_members_age")

  testthat::expect_error(
    anomaly_test_tmln_int(ls = ls_test,audit = test_audits,data = test_data,
                          geo_column = 'q2_4_1_oblast',enum_name = 'q0_2_enum_id',n_limit = 10),
    'uuid is not present in your data or audit objects please double check'
  )
})


testthat::test_that('anomaly_test_tmln_int works, test 6 - returns an error when uuids dont match',{

  test_audit_path <- testthat::test_path('fixtures/audits_test')

  test_path <- testthat::test_path('fixtures',"data_others.xlsx")
  test_data <-  readxl::read_excel(test_path, col_types = 'text') %>%
    dplyr::mutate(uuid = 'test')

  test_audits <- load.audit.files(dir.audits = test_audit_path,
                                  uuids = test_data$`_uuid`,add.uuid2=F)


  ls_test <- c("q2_1_1_members_age")

  testthat::expect_error(
    anomaly_test_tmln_int(ls = ls_test,audit = test_audits,data = test_data,
                          geo_column = 'q2_4_1_oblast',enum_name = 'q0_2_enum_id',n_limit = 10),
    'None of the uuids present in the audit files are present in the dataframe, please double check'
  )
})

testthat::test_that('anomaly_test_tmln_int works, test 7 - returns an error when geo_column doesnt exist',{

  test_audit_path <- testthat::test_path('fixtures/audits_test')

  test_path <- testthat::test_path('fixtures',"data_others.xlsx")
  test_data <-  readxl::read_excel(test_path, col_types = 'text') %>%
    dplyr::rename(uuid = `_uuid`)

  test_audits <- load.audit.files(dir.audits = test_audit_path,
                                  uuids = test_data$`uuid`,add.uuid2=F)


  ls_test <- c("q2_1_1_members_age")

  testthat::expect_error(
    anomaly_test_tmln_int(ls = ls_test,audit = test_audits,data = test_data,
                          geo_column = 'test',enum_name = 'q0_2_enum_id',n_limit = 10),
    'The following variables are not present in your dataframe:test'
  )
})



testthat::test_that('anomaly_test_tmln_int works, test 8 - returns an error ipq is too large',{

  test_audit_path <- testthat::test_path('fixtures/audits_test')

  test_path <- testthat::test_path('fixtures',"data_others.xlsx")
  test_data <-  readxl::read_excel(test_path, col_types = 'text') %>%
    dplyr::rename(uuid = `_uuid`)

  test_audits <- load.audit.files(dir.audits = test_audit_path,
                                  uuids = test_data$`uuid`,add.uuid2=F)


  ls_test <- c("q2_1_1_members_age")

  testthat::expect_error(
    anomaly_test_tmln_int(ls = ls_test,audit = test_audits,data = test_data,
                          geo_column = 'q2_4_1_oblast',enum_name = 'q0_2_enum_id',n_limit = 1, ipq_n = 1.2),
    "The ipq_n is not between 0 and 1. This is wrong."
  )
})


testthat::test_that('anomaly_test_enum_diffr works, test 1 - basic functionality',{

  test_audit_path <- testthat::test_path('fixtures/audits_test')

  test_path <- testthat::test_path('fixtures',"data_others.xlsx")
  test_data <-  readxl::read_excel(test_path, col_types = 'text')%>%
    dplyr::rename(uuid = `_uuid`)

  test_audits <- load.audit.files(dir.audits = test_audit_path,
                                  uuids = test_data$uuid,add.uuid2=F)


  ls_test <- c("q2_1_1_members_age", "q2_1_2_members_sex", "q2_1_3_members_education_level",
               "q2_1_4_members_vulnerabilities")

  test_data$oblast <- substr(test_data$q0_4_settlement,1,4)

  actual_result <- anomaly_test_enum_diffr(variable_list = ls_test,
                                           audit = test_audits,data_frame = test_data,
                                         geo_column = 'oblast',enum_name = 'q0_2_enum_id',
                                         n_limit = 1, sig_thr = 0.5)


  expected_result <-dplyr::tibble(
    oblast  = character(),
    q0_2_enum_id = character(),
    differences_from_enums = integer(),
    total_enums = integer(),
    perc_issues = double(),
    list_used = character()
  )

  testthat::expect_equal(actual_result, expected_result)

})


testthat::test_that('anomaly_test_enum_diffr works, test 2 - basic functionality - no differences found',{

  test_audit_path <- testthat::test_path('fixtures/audits_test')

  test_path <- testthat::test_path('fixtures',"data_others.xlsx")
  test_data <-  readxl::read_excel(test_path, col_types = 'text')%>%
    dplyr::rename(uuid = `_uuid`)

  test_audits <- load.audit.files(dir.audits = test_audit_path,
                                  uuids = test_data$uuid,add.uuid2=F)


  ls_test <- c("q2_1_1_members_age", "q2_1_2_members_sex", "q2_1_3_members_education_level",
               "q2_1_4_members_vulnerabilities")

  test_data$oblast <- substr(test_data$q0_4_settlement,1,4)

  actual_result <- anomaly_test_enum_diffr(variable_list = ls_test,
                                           audit = test_audits,data_frame = test_data,
                                           geo_column = 'oblast',enum_name = 'q0_2_enum_id',
                                           n_limit = 1, sig_thr = 0.1)


  expected_result <-dplyr::tibble()

  testthat::expect_equal(actual_result, expected_result)

})



testthat::test_that('anomaly_test_enum_diffr works, test 3 - basic functionality - no large samples found',{

  test_audit_path <- testthat::test_path('fixtures/audits_test')

  test_path <- testthat::test_path('fixtures',"data_others.xlsx")
  test_data <-  readxl::read_excel(test_path, col_types = 'text')%>%
    dplyr::rename(uuid = `_uuid`)

  test_audits <- load.audit.files(dir.audits = test_audit_path,
                                  uuids = test_data$uuid,add.uuid2=F)


  ls_test <- c("q2_1_1_members_age", "q2_1_2_members_sex", "q2_1_3_members_education_level",
               "q2_1_4_members_vulnerabilities")

  test_data$oblast <- substr(test_data$q0_4_settlement,1,4)

  actual_result <- anomaly_test_enum_diffr(variable_list = ls_test,
                                           audit = test_audits,data_frame = test_data,
                                           geo_column = 'oblast',enum_name = 'q0_2_enum_id',
                                           n_limit = 10, sig_thr = 0.1)


  expected_result <-dplyr::tibble()

  testthat::expect_equal(actual_result, expected_result)

})


testthat::test_that('anomaly_test_enum_diffr works, test 4 - returns an error if a wrong variable is fed',{

  test_audit_path <- testthat::test_path('fixtures/audits_test')

  test_path <- testthat::test_path('fixtures',"data_others.xlsx")
  test_data <-  readxl::read_excel(test_path, col_types = 'text')%>%
    dplyr::rename(uuid = `_uuid`)

  test_data$oblast <- substr(test_data$q0_4_settlement,1,4)

  test_audits <- load.audit.files(dir.audits = test_audit_path,
                                  uuids = test_data$uuid,add.uuid2=F)


  ls_test <- c("test")

  testthat::expect_error(
    anomaly_test_enum_diffr(variable_list = ls_test,
                            audit = test_audits,data_frame = test_data,
                            geo_column = 'oblast',enum_name = 'q0_2_enum_id',
                            n_limit = 10, sig_thr = 0.1),
    "Error: some of the questions you've entered are not present in your audit data. please double check: test"
  )


})

testthat::test_that('anomaly_test_enum_diffr works, test 5 - returns an error if a non numeric parameter is fed',{

  test_audit_path <- testthat::test_path('fixtures/audits_test')

  test_path <- testthat::test_path('fixtures',"data_others.xlsx")
  test_data <-  readxl::read_excel(test_path, col_types = 'text')%>%
    dplyr::rename(uuid = `_uuid`)

  test_data$oblast <- substr(test_data$q0_4_settlement,1,4)

  test_audits <- load.audit.files(dir.audits = test_audit_path,
                                  uuids = test_data$uuid,add.uuid2=F)


  ls_test <- c("q2_1_1_members_age")

  testthat::expect_error(
    anomaly_test_enum_diffr(variable_list = ls_test,
                            audit = test_audits,data_frame = test_data,
                            geo_column = 'oblast',enum_name = 'q0_2_enum_id',
                            n_limit = 10, sig_thr = '0.1'),
    "The n_limit or sig_thr you've chosen is non-numeric. Please fix this"
  )
})


testthat::test_that('anomaly_test_enum_diffr works, test 6 - returns an error uuid is not present',{

  test_audit_path <- testthat::test_path('fixtures/audits_test')

  test_path <- testthat::test_path('fixtures',"data_others.xlsx")
  test_data <-  readxl::read_excel(test_path, col_types = 'text')

  test_data$oblast <- substr(test_data$q0_4_settlement,1,4)

  test_audits <- load.audit.files(dir.audits = test_audit_path,
                                  uuids = test_data$`_uuid`,add.uuid2=F)


  ls_test <- c("q2_1_1_members_age")

  testthat::expect_error(
    anomaly_test_enum_diffr(variable_list = ls_test,
                            audit = test_audits,data_frame = test_data,
                            geo_column = 'oblast',enum_name = 'q0_2_enum_id',
                            n_limit = 10, sig_thr = 0.1),
    'uuid is not present in your data or audit objects please double check'
  )
})


testthat::test_that('anomaly_test_enum_diffr works, test 7 - returns an error when uuids dont match',{

  test_audit_path <- testthat::test_path('fixtures/audits_test')

  test_path <- testthat::test_path('fixtures',"data_others.xlsx")
  test_data <-  readxl::read_excel(test_path, col_types = 'text') %>%
    dplyr::mutate(uuid = 'test')

  test_audits <- load.audit.files(dir.audits = test_audit_path,
                                  uuids = test_data$`_uuid`,add.uuid2=F)

  test_data$oblast <- substr(test_data$q0_4_settlement,1,4)

  ls_test <- c("q2_1_1_members_age")

  testthat::expect_error(
    anomaly_test_enum_diffr(variable_list = ls_test,
                            audit = test_audits,data_frame = test_data,
                            geo_column = 'oblast',enum_name = 'q0_2_enum_id',
                            n_limit = 10, sig_thr = 0.1),
    'None of the uuids present in the audit files are present in the dataframe, please double check'
  )
})


testthat::test_that('anomaly_test_enum_diffr works, test 8 - returns an error when geo_column doesnt exist',{

  test_audit_path <- testthat::test_path('fixtures/audits_test')

  test_path <- testthat::test_path('fixtures',"data_others.xlsx")
  test_data <-  readxl::read_excel(test_path, col_types = 'text') %>%
    dplyr::mutate(uuid = 'test')

  test_audits <- load.audit.files(dir.audits = test_audit_path,
                                  uuids = test_data$`_uuid`,add.uuid2=F)

  test_data$oblast <- substr(test_data$q0_4_settlement,1,4)

  ls_test <- c("q2_1_1_members_age")

  testthat::expect_error(
    anomaly_test_enum_diffr(variable_list = ls_test,
                            audit = test_audits,data_frame = test_data,
                            geo_column = 'test',enum_name = 'q0_2_enum_id',
                            n_limit = 10, sig_thr = 0.1),
    'The following variables are not present in your dataframe:test'
  )
})




testthat::test_that('anomaly_test_enum_diffr works, test 9 - wrong significance',{

  test_audit_path <- testthat::test_path('fixtures/audits_test')

  test_path <- testthat::test_path('fixtures',"data_others.xlsx")
  test_data <-  readxl::read_excel(test_path, col_types = 'text') %>%
    dplyr::mutate(uuid = 'test')

  test_audits <- load.audit.files(dir.audits = test_audit_path,
                                  uuids = test_data$`_uuid`,add.uuid2=F)

  test_data$oblast <- substr(test_data$q0_4_settlement,1,4)

  ls_test <- c("q2_1_1_members_age")

  testthat::expect_error(
    anomaly_test_enum_diffr(variable_list = ls_test,
                            audit = test_audits,data_frame = test_data,
                            geo_column = 'oblast',enum_name = 'q0_2_enum_id',
                            n_limit = 10, sig_thr = 1.2),
    "The sig_thr is not between 0 and 1. This is wrong."
  )
})



