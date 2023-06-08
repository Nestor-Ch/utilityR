testthat::test_that("load.label_colname works", {
  testdata <- testthat::test_path("fixtures","tool.xlsx")
  actual_output <- load.label.colname(testdata)
  testthat::expect_equal(actual_output,"label::English")
})

testthat::test_that("load.tool.choices works", {
  ## colnames are correct
  testdata <- testthat::test_path("fixtures","tool.xlsx")
  label_colname <- "label::English"
  actual_output <- load.tool.choices(testdata,label_colname) %>% names
  expected_output <- data.frame(list_name = c("test","test","test2","test2"),
                                name = c("yes","no","dont_know","dont_know"),
                                `label::English` = c("Yes", "No", "Dont know", "Dont know")) %>%
    dplyr::rename("label::English" = `label..English`)%>% names

  testthat::expect_equal(actual_output,expected_output)

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


testthat::test_that("load.tool.survey works", {
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
