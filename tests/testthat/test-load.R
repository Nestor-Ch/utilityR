testthat::test_that("load.label_colname works", {
  testdata <- testthat::test_path("fixtures","tool.xlsx")
  actual_output <- load.label.colname(testdata)
  testthat::expect_equal(actual_output,"label::English")
})

testthat::test_that("load.tool.choices works - test 1 - small data", {
  ## colnames are correct
  testdata <- testthat::test_path("fixtures","tool.xlsx")
  label_colname <- "label::English"
  actual_output <- load.tool.choices(testdata,label_colname) %>% names
  expected_output <- data.frame(list_name = c("test","test","test2","test2"),
                                name = c("yes","no","dont_know","dont_know"),
                                `label::English` = c("Yes", "No", "Dont know", "Dont know")) %>%
    dplyr::rename("label::English" = `label..English`)%>% names

  testthat::expect_equal(actual_output,expected_output)
})

testthat::test_that("load.tool.choices works - test 2 - full data", {

  ## Correct output
  testdata <- testthat::test_path("fixtures","tool.xlsx")
  label_colname <- "label::English"
  actual_output <- load.tool.choices(testdata,label_colname)
  expected_output <- data.frame(list_name = c("yn","yn","partner","partner","partner","partner","partner",
                                              "partner","partner","partner","partner","partner","partner",
                                              "partner","partner","partner","partner","partner","partner",
                                              "partner","partner","partner","partner","partner","partner",
                                              "partner","partner","partner","affect","affect","affect"),
                                name = c("yes","no","Caritas","Save_the_Children","CORE","ACTED","KIIS",
                                         "JERU","FAO","ACF","PIN","IRC","WFP","REACH", "UFF_ERC","TGH",
                                         "Mercy_Corps","URCS","HEKS_EPER","Equilibrium","ZT","UNOPS","LASKA",
                                         "Dorcas","New_Partner","Additional_Partner","WVI","Global_Communities",
                                         "no_impact","movement_restrictions","fighting_shelling"),
                                `label::English` = c("Yes","No","Caritas","Save_the_Children","CORE","ACTED",
                                                     "KIIS","JERU","FAO","ACF","PIN","IRC","WFP","REACH",
                                                     "UFF_ERC","TGH","Mercy_Corps","URCS","HEKS_EPER","Equilibrium",
                                                     "ZT","UNOPS","LASKA","Dorcas","New_Partner",
                                                     "Additional_Partner","WVI","Global_Communities",
                                                     "No impact on physical access to stores or marketplaces",
                                                     "Movement restrictions related to martial law",
                                                     "Active fighting or shelling in the area")) %>%
    dplyr::rename("label::English" = `label..English`)
  testthat::expect_equal(actual_output,expected_output)

})


testthat::test_that("load.tool.survey works, general functionality", {
  testdata <- testthat::test_path("fixtures","tool.xlsx")
  label_colname <- "label::English"
  actual_output <- load.tool.survey(testdata,label_colname, keep_cols=F)
  expected_output <- data.frame(type = c("start","end","today","deviceid","audit","begin_group",
                                         "select_one partner","end_group","begin_repeat","text",
                                         "end_repeat","begin_group","select_one yn","select_multiple affect",
                                         "text","end_group"),
                                name = c("start","end","date_assessment","deviceid","audit","a_basic_information",
                                         "a2_partner",NA_character_,"individuals","individuals_comments",NA_character_,
                                         "a_basic_information","b3_is_this_a_test","b17_access_stores",
                                         "b17_1_access_stores_other",NA_character_),
                                `label::English` = c(NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,"A_Basic information","A2_Partner name",
                                                   NA_character_,NA_character_,"Any comments",NA_character_,"A_Basic information",
                                                   "Is this a test?",
                                                   "B17_How have the war and its related developments affected your ability to access your usual store or marketplace this month?",
                                                    "B17_1_Other (specify)",NA_character_),
                                `hint::English` = c(NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,"Select one",NA_character_,
                                                    NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,"Select all that apply",
                                                    "Fill",NA_character_),
                                required = c(NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,"yes",NA_character_,NA_character_,"yes",NA_character_,NA_character_,"yes","yes",
                                             "yes",NA_character_),
                                `required_message::English` = c(NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,"Answer is required",
                                                              NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,
                                                              NA_character_,"Answer is required","Answer is required",NA_character_),
                                appearance = c(NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_),
                                choice_filter = c(NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_),
                                relevant = c(NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,
                                             "selected(${b17_access_stores}, 'other')",NA_character_),
                                constraint = c(NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,
                                               "not((selected(., 'prefer_not_answer') and count-selected(.)>1) or (selected(., 'no_impact') and count-selected(.)>1))",NA_character_,NA_character_),
                                `constraint_message::English` = c(NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,
                                                                  'Dont select any other options if youve selected "Prefer not to answer" or "No impact on physical access to stores or marketplaces"',NA_character_,NA_character_),
                                parameters = c(NA_character_,NA_character_,NA_character_,NA_character_,"track-changes=true",NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_),
                                q.type = c("start","end","today","deviceid","audit","begin_group",
                                           "select_one","end_group","begin_repeat","text",
                                           "end_repeat","begin_group","select_one",
                                           "select_multiple","text","end_group"),
                                list_name = c(NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,"partner",NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,"yn","affect",NA_character_,NA_character_),
                                datasheet = c("main","main","main","main","main",NA_character_,
                                              "main",NA_character_,NA_character_,"individuals",
                                              NA_character_,NA_character_,"main","main","main",NA_character_)) %>%
    dplyr::rename("label::English" = `label..English`,
                  "hint::English" = `hint..English`,
                  "required_message::English" = `required_message..English`,
                  "constraint_message::English" = `constraint_message..English`)
  testthat::expect_equal(actual_output,expected_output)

})



testthat::test_that("load.tool.survey works,  general functionality", {
  testdata <- testthat::test_path("fixtures","tool.xlsx")
  label_colname <- "label::English"
  actual_output <- load.tool.survey(testdata,label_colname, keep_cols=F)
  expected_output <- data.frame(type = c("start","end","today","deviceid","audit","begin_group",
                                         "select_one partner","end_group","begin_repeat","text",
                                         "end_repeat","begin_group","select_one yn","select_multiple affect",
                                         "text","end_group"),
                                name = c("start","end","date_assessment","deviceid","audit","a_basic_information",
                                         "a2_partner",NA_character_,"individuals","individuals_comments",NA_character_,
                                         "a_basic_information","b3_is_this_a_test","b17_access_stores",
                                         "b17_1_access_stores_other",NA_character_),
                                `label::English` = c(NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,"A_Basic information","A2_Partner name",
                                                     NA_character_,NA_character_,"Any comments",NA_character_,"A_Basic information",
                                                     "Is this a test?",
                                                     "B17_How have the war and its related developments affected your ability to access your usual store or marketplace this month?",
                                                     "B17_1_Other (specify)",NA_character_),
                                `hint::English` = c(NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,"Select one",NA_character_,
                                                    NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,"Select all that apply",
                                                    "Fill",NA_character_),
                                required = c(NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,"yes",NA_character_,NA_character_,"yes",NA_character_,NA_character_,"yes","yes",
                                             "yes",NA_character_),
                                `required_message::English` = c(NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,"Answer is required",
                                                                NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,
                                                                NA_character_,"Answer is required","Answer is required",NA_character_),
                                appearance = c(NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_),
                                choice_filter = c(NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_),
                                relevant = c(NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,
                                             "selected(${b17_access_stores}, 'other')",NA_character_),
                                constraint = c(NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,
                                               "not((selected(., 'prefer_not_answer') and count-selected(.)>1) or (selected(., 'no_impact') and count-selected(.)>1))",NA_character_,NA_character_),
                                `constraint_message::English` = c(NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,
                                                                  'Dont select any other options if youve selected "Prefer not to answer" or "No impact on physical access to stores or marketplaces"',NA_character_,NA_character_),
                                parameters = c(NA_character_,NA_character_,NA_character_,NA_character_,"track-changes=true",NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_),
                                q.type = c("start","end","today","deviceid","audit","begin_group",
                                           "select_one","end_group","begin_repeat","text",
                                           "end_repeat","begin_group","select_one",
                                           "select_multiple","text","end_group"),
                                list_name = c(NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,"partner",NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,"yn","affect",NA_character_,NA_character_),
                                datasheet = c("main","main","main","main","main",NA_character_,
                                              "main",NA_character_,NA_character_,"individuals",
                                              NA_character_,NA_character_,"main","main","main",NA_character_)) %>%
    dplyr::rename("label::English" = `label..English`,
                  "hint::English" = `hint..English`,
                  "required_message::English" = `required_message..English`,
                  "constraint_message::English" = `constraint_message..English`)
  testthat::expect_equal(actual_output,expected_output)

})




testthat::test_that("load.requests works, test 1 general functionality", {

  expected_output <- data.frame(uuid = c('a46a1c10-bf18-4594-a0be-99447fa22116','51862558-1b68-466a-8e71-be2817dce5aa','51862558-1b68-466a-8e71-be2817dce5aa',
                                         '0cc0f1d0-ff99-46e7-87c3-5699174dc033','6d4506b5-f3ab-4d88-abf3-48c1eb5171f6'),
                                loop_index = as.character(rep(NA,5)),
                                name = c(rep('q0_4_2_1_center_idp_other',2), 'q0_4_2_1_center_idp_other_test',rep('q0_4_2_1_center_idp_other',2)),
                                ref.name = c(rep('q0_4_2_center_idp',2), 'q0_4_2_center_idp_test',rep('q0_4_2_center_idp',2)),
                                full.label = rep('0.4.2 Center IDP - 0.4.2.1 Other, please specify',5),
                                ref.type = c('select_one',rep('select_multiple',3),'select_one'),
                                choices.label = rep('test',5),
                                response.uk = c('29','29','29','test2','test2'),
                                response.en = c('29','29','29','by place of work', 'dormitories'),
                                true.v = c(NA,'test',NA,'test',NA),
                                existing.v = c(NA,NA,NA,NA,'test'),
                                invalid.v = c('test',NA,NA,NA,'test')
  ) %>% dplyr::tibble()

  test_dir = testthat::test_path('fixtures')

  actual_output <- load.requests(dir = test_dir,filename.pattern ='test_requests_1', sheet = 'Sheet2')

  testthat::expect_equal(expected_output, actual_output)
})

testthat::test_that("load.requests works test 2 - expect error if the filename is wrong", {
  test_dir = testthat::test_path('fixtures')
  # expect error if the filename is wrong

  testthat::expect_warning(load.requests(dir = test_dir,filename.pattern ='fake_error',
                                         sheet = 'Sheet2', validate = TRUE),
                           "Files with fake error requests not found!")
})

testthat::test_that("load.requests works test 3 - expect warning for the case if the ncol is different", {
  test_dir = testthat::test_path('fixtures')

  testthat::expect_warning(load.requests(dir = test_dir,filename.pattern ='test_requests_', sheet = 'Sheet2'),
                           "Number of columns differs between files! Check them to make sure everything is correct, please!")
})

testthat::test_that("load.requests works test 4 - expect warning in the case of validation - one of the columns is missing all entries", {
  test_dir = testthat::test_path('fixtures')

  testthat::expect_warning(load.requests(dir = test_dir,filename.pattern ='test_requests_1_1',
                                         sheet = 'Sheet2', validate = TRUE),
                           'Missing entries:\n 51862558-1b68-466a-8e71-be2817dce5aa')
})

testthat::test_that("load.requests works test 5 - expect warning, one column has multiple entries", {
  test_dir = testthat::test_path('fixtures')

  testthat::expect_warning(load.requests(dir = test_dir,filename.pattern ='test_requests_1_2',
                                         sheet = 'Sheet2', validate = TRUE),
                           'Multiple columns selected:\n 6d4506b5-f3ab-4d88-abf3-48c1eb5171f6')
})

testthat::test_that("load.requests works test 6 - expect warning, missing TRUE column", {
  test_dir = testthat::test_path('fixtures')

  testthat::expect_error(load.requests(dir = test_dir,
                                       filename.pattern ='test_requestsmissing_v_column',
                                       sheet = 'Sheet2', validate = TRUE),
                         "One or more of 'true', 'existing', 'invalid' columns not found in requests files.")

})

testthat::test_that("load.requests works test 7 - no ref.type column but should still work", {
  test_dir = testthat::test_path('fixtures')

  expected_output_2 <- data.frame(uuid = c('0cc0f1d0-ff99-46e7-87c3-5699174dc033','6d4506b5-f3ab-4d88-abf3-48c1eb5171f6'),
                                  loop_index = as.character(rep(NA,2)),
                                  name = rep('q0_4_2_1_center_idp_other',2),
                                  ref.name = rep('q0_4_2_center_idp',2),
                                  full.label = rep('0.4.2 Center IDP - 0.4.2.1 Other, please specify',2),
                                  choices.label = rep('test',2),
                                  response.uk = c('test2','test2'),
                                  response.en = c('by place of work', 'dormitories'),
                                  true.v = c('test',NA),
                                  existing.v = c(NA,'test'),
                                  invalid.v = as.character(c(NA,NA)),
                                  check = c(2,2)
  ) %>% dplyr::tibble()

  actual_output_2 <- load.requests(dir = test_dir,filename.pattern ='test_requestsnoref_column',
                                   sheet = 'Sheet2', validate=T)

  testthat::expect_equal(expected_output_2, actual_output_2)


})



testthat::test_that('load.audit.files works, tests of general functionality',{

  test_audit_path <- testthat::test_path('fixtures/audits_test')

  test_path <- testthat::test_path('fixtures',"data_others.xlsx")
  test_data <-  readxl::read_excel(test_path, col_types = 'text')%>%
    dplyr::rename(uuid = `_uuid`)

  actual_result <- load.audit.files(dir.audits = test_audit_path,
                                    uuids = test_data$uuid,add.uuid2=F)

  # expected result is a bit difficult to get, tbh. Have to kinda mimic the function itself.

  ls <- list.files(test_audit_path, pattern="audit.csv", recursive=TRUE, full.names=TRUE)
  data_test <- data.frame()
  for (link in ls){
    sp <- stringr::str_split(link, "\\/")[[1]]
    uuid <- sp[length(sp)-1]
    tata_temp <- readr::read_csv(link, show_col_types = FALSE, locale = readr::locale(encoding = "UTF-8")) %>%
      dplyr::mutate(uuid=uuid, .before=1)
    data_test = rbind(data_test,tata_temp)
  }


  expected_output <- data_test %>%
    dplyr::mutate(dplyr::across(uuid:`new-value`,~ ifelse(.x=="",NA_character_,.x)),
                  dplyr::across(`old-value`:`new-value`,~ stringr::str_squish(.x))) %>%
    dplyr::mutate(old.value = NA_character_,
                  new.value = NA_character_) %>%
    dplyr::group_by(uuid) %>%
    dplyr::mutate(inter_q_duration = (start-dplyr::lag(end))/1000) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(duration = (end-start)/1000,
                  group = sapply(stringr::str_split(node, '\\/'), function(x){
                    id.group <- ifelse("G_survey" %in% x, 4, 3)
                    return(x[id.group])}),
                  question = gsub(".*/([^/]+)$", "\\1", node),
                  event=stringr::str_replace_all(event, " ", ".")
    ) %>%
    dplyr::mutate(across(dplyr::ends_with('value'),~ ifelse(.x == '' |.x == ' '|.x == '\n',NA_character_, .x )))
  testthat::expect_equal(actual_result,expected_output)

  # test 2

  actual_result <- load.audit.files(dir.audits = test_audit_path,
                                    uuids = test_data$uuid, track.changes = T,add.uuid2=F)

  expected_output <- data_test %>%
    dplyr::mutate(dplyr::across(uuid:`new-value`,~ ifelse(.x=="",NA_character_,.x)),
                  dplyr::across(`old-value`:`new-value`,~ stringr::str_squish(.x))) %>%
    dplyr::rename("old.value" = `old-value`, "new.value" = `new-value`) %>%
    dplyr::group_by(uuid) %>%
    dplyr::mutate(inter_q_duration = (start-dplyr::lag(end))/1000) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(duration = (end-start)/1000,
                  group = sapply(stringr::str_split(node, '\\/'), function(x){
                    id.group <- ifelse("G_survey" %in% x, 4, 3)
                    return(x[id.group])}),
                  question = gsub(".*/([^/]+)$", "\\1", node),
                  event=stringr::str_replace_all(event, " ", ".")
    ) %>%
    dplyr::mutate(across(dplyr::ends_with('value'),~ ifelse(.x == '' |.x == ' '|.x == '\n',NA_character_, .x )))
  testthat::expect_equal(actual_result,expected_output)
})

testthat::test_that('load.audit.files works, warning if the dir is wrong',{

  test_path <- testthat::test_path('fixtures',"data_others.xlsx")
  test_data <-  readxl::read_excel(test_path, col_types = 'text')%>%
    dplyr::rename(uuid = `_uuid`)

  test_audit_path <- testthat::test_path('fixtures/audits_test/empty_test')
  testthat::expect_warning(
  load.audit.files(dir.audits = test_audit_path,add.uuid2=F,
                   uuids = test_data$uuid),
  "No relevant audit logs found!"
  )
})

testthat::test_that('load.audit.files works, should work if we specify add.uuid2 = T',{

  test_path <- testthat::test_path('fixtures',"data_others.xlsx")
  test_data <-  readxl::read_excel(test_path, col_types = 'text')%>%
    dplyr::rename(uuid = `_uuid`)

  # test 4 if add uuid2
  test_audit_path <- testthat::test_path('fixtures/audits_test')

  ls <- list.files(test_audit_path, pattern="audit.csv", recursive=TRUE, full.names=TRUE)
  data_test <- data.frame()
  for (link in ls){
    sp <- stringr::str_split(link, "\\/")[[1]]
    uuid <- sp[length(sp)-1]
    tata_temp <- readr::read_csv(link, show_col_types = FALSE, locale = readr::locale(encoding = "UTF-8")) %>%
      dplyr::mutate(uuid=uuid, .before=1)
    data_test = rbind(data_test,tata_temp)
  }


  actual_result <- load.audit.files(dir.audits = test_audit_path, track.changes = T,
                                    uuids = test_data$uuid, add.uuid2 =T)


  expected_output <- data_test %>%
    dplyr::mutate(dplyr::across(uuid:`new-value`,~ ifelse(.x=="",NA_character_,.x)),
                  dplyr::across(`old-value`:`new-value`,~ stringr::str_squish(.x))) %>%
    dplyr::rename("old.value" = `old-value`, "new.value" = `new-value`) %>%
    dplyr::mutate(uuid2 = uuid) %>%
    dplyr::group_by(uuid) %>%
    dplyr::mutate(inter_q_duration = (start-dplyr::lag(end))/1000) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(duration = (end-start)/1000,
                  group = sapply(stringr::str_split(node, '\\/'), function(x){
                    id.group <- ifelse("G_survey" %in% x, 4, 3)
                    return(x[id.group])}),
                  question = gsub(".*/([^/]+)$", "\\1", node),
                  event=stringr::str_replace_all(event, " ", ".")
    ) %>%
    dplyr::mutate(across(dplyr::ends_with('value'),~ ifelse(.x == '' |.x == ' '|.x == '\n',NA_character_, .x )))
  testthat::expect_equal(actual_result,expected_output)


})

