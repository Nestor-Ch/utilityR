testthat::test_that("recode.set.NA.if works", {

  #upload the relevant data
  test_path <- testthat::test_path("fixtures","utilityR_raw_data.xlsx")
  test_data <- readxl::read_excel(test_path)[1:10,]

  # Test 1

  # set up the variable names and relevant codes to be dropped
  num_test <- c('b11_gas_heating_price','b10_gas_vehicle_price','b9_diesel_official_price','b8_petrol_official_price')
  code_test_1 <- c('7.96','22','22','50')

  actual_output <- recode.set.NA.if(test_data, num_test, code_test_1, issue= 'test_issue')

  expected_output_1 <- dplyr::tibble(variable = c('b11_gas_heating_price','b10_gas_vehicle_price','b10_gas_vehicle_price','b10_gas_vehicle_price',
                                                  'b9_diesel_official_price','b8_petrol_official_price'),
                                     old.value = c('7.96','22','22','22','50','50')) %>%
    dplyr::mutate(new.value=NA,
                  issue = 'test_issue')

  testthat::expect_equal(actual_output,expected_output_1)

  # Test 2
  text_test <- c('Fake_var_for_test','Fake_var_2','b7_vehicle_fuel','b18_financial_factors','c3_ukrposhta_available')

  testthat::expect_error(recode.set.NA.if(test_data,text_test, code_test_1, issue= 'test_issue'))

  # Test 3

  text_test_2 <- c('b7_vehicle_fuel','b18_financial_factors')
  code_test_2 <- c('none','not_afford')

  actual_output <- recode.set.NA.if(test_data, text_test_2, code_test_2, issue= 'test_issue', ignore_case = T)

  expected_output_2 <- dplyr::tibble(variable = c('b7_vehicle_fuel','b7_vehicle_fuel','b18_financial_factors'),
                                     old.value = c('none','none','not_afford')) %>%
    dplyr::mutate(new.value=NA,
                  issue = 'test_issue')

  testthat::expect_equal(expected_output_2,actual_output)

  # Test 4

  text_test_3 <- c('a2_partner')
  code_test_3 <- c('JERU')

  actual_output <- recode.set.NA.if(test_data, text_test_3, code_test_3, issue= 'test_issue', ignore_case = F)

  expected_output_3 <- dplyr::tibble(variable = c('a2_partner','a2_partner','a2_partner'),
                                     old.value = c('JERU','JERU','JERU')) %>%
    dplyr::mutate(new.value=NA,
                  issue = 'test_issue')

  testthat::expect_equal(expected_output_3,actual_output )

})


testthat::test_that("recode.set.NA.regex works", {

  #upload the relevant data
  test_path <- testthat::test_path("fixtures","utilityR_raw_data.xlsx")
  test_data <- readxl::read_excel(test_path)[1:10,]

  # Test 1

  text_test <- c('Fake_var_for_test','Fake_var_2','b7_vehicle_fuel','b18_financial_factors','c3_ukrposhta_available')
  testthat::expect_error(recode.set.NA.regex(test_data,text_test, pattern = '^J.*', issue= 'test_issue'))

  # Test 2

  text_test_2 <- c('a2_partner','a2_1_enum_id')

  actual_output <- recode.set.NA.regex(test_data, text_test_2, pattern = '^J.*', issue= 'test_issue')

  expected_output_2 <- dplyr::tibble(variable = c('a2_partner','a2_partner','a2_partner','a2_1_enum_id','a2_1_enum_id','a2_1_enum_id'),
                                     old.value = c('JERU','JERU','JERU','JERU_001','JERU_001','JERU_001')) %>%
    dplyr::mutate(new.value=NA,
                  issue = 'test_issue')

  testthat::expect_equal(expected_output_2,actual_output)

})

testthat::test_that("recode.set.NA works", {

  #upload the relevant data
  test_path <- testthat::test_path("fixtures","utilityR_raw_data.xlsx")
  test_data <- readxl::read_excel(test_path)[1:10,]

  # Test 1

  text_test <- c('Fake_var_for_test','Fake_var_2','b7_vehicle_fuel','b18_financial_factors','c3_ukrposhta_available')
  testthat::expect_error(recode.set.NA(test_data,text_test, issue= 'test_issue'))

  # Test 2

  text_test_2 <- c('a2_partner')

  actual_output <- recode.set.NA(test_data, text_test_2, issue= 'test_issue')

  expected_output_2 <- dplyr::tibble(variable = rep('a2_partner',10),
                                     old.value = c(rep('JERU',3),rep('ACTED',5),rep('REACH',2) )) %>%
    dplyr::mutate(new.value=NA,
                  issue = 'test_issue')

  testthat::expect_equal(expected_output_2,actual_output)

})


testthat::test_that("recode.set.value.regex works", {

  #upload the relevant data
  test_path <- testthat::test_path("fixtures","utilityR_raw_data.xlsx")
  test_data <- readxl::read_excel(test_path)[11:20,]

  # Test 1

  text_test <- c('Fake_var_for_test','Fake_var_2','b7_vehicle_fuel','b18_financial_factors','c3_ukrposhta_available')
  testthat::expect_error(recode.set.value.regex(test_data,text_test, pattern = '^J.*',
                                                new.value='testo', issue = 'test_issue'))

  # Test 2

  text_test_2 <- c('b2_hygiene','b3_vehicle_fuel')

  actual_output <- recode.set.value.regex(test_data, c('b2_hygiene','b3_vehicle_fuel'), pattern = 'dont_know',
                                          new.value='testo', issue= 'test_issue')

  expected_output_2 <- dplyr::tibble(variable = c('b2_hygiene','b3_vehicle_fuel','b3_vehicle_fuel'),
                                     old.value = c('dont_know','dont_know','dont_know')) %>%
    dplyr::mutate(new.value=NA,
                  issue = 'test_issue',
                  new.value='testo')

  testthat::expect_equal(expected_output_2,actual_output)


  # Test 3

  text_test_2 <- c('b8_petrol_official_price','b9_diesel_official_price')

  actual_output <- recode.set.value.regex(test_data, text_test_2, pattern = '48|49',
                                          new.value='testo', issue= 'test_issue',affect_na = TRUE)

  expected_output_2 <- dplyr::tibble(variable = c(rep('b8_petrol_official_price',7), rep('b9_diesel_official_price',8)),
                                     old.value = c('48',rep(NA,5),'48',rep(NA,6),'49',NA)) %>%
    dplyr::mutate(new.value=NA,
                  issue = 'test_issue',
                  new.value='testo')

  testthat::expect_equal(expected_output_2,actual_output)


})


testthat::test_that("recode.multiple.set.NA works", {

  #upload the relevant data
  test_path <- testthat::test_path("fixtures","utilityR_raw_data.xlsx")
  test_data <- readxl::read_excel(test_path)[41:50,] %>%
    dplyr::rename(uuid = `_uuid`)
  # set up loop index to test if it works
  test_data$loop_index = 1:10

  # test 1 - basic case

  actual_output <- recode.multiple.set.NA(test_data, 'b7_vehicle_fuel', issue = 'test_issue')

  expected_output <- data.frame(uuid = unlist(rep(test_data[,'uuid'],7)),
                                loop_index = rep(1:10,7),
                                variable = c(rep('b7_vehicle_fuel',10), rep("b7_vehicle_fuel/petrol_vehicles",10), rep("b7_vehicle_fuel/diesel_vehicles",10),
                                             rep("b7_vehicle_fuel/gas_vehicles",10),rep("b7_vehicle_fuel/electricity_vehicles",10),
                                             rep("b7_vehicle_fuel/none_vehicles",10),rep("b7_vehicle_fuel/none",10) ),
                                old.value = c(as.vector(as.matrix(test_data[,c("b7_vehicle_fuel","b7_vehicle_fuel/petrol_vehicles","b7_vehicle_fuel/diesel_vehicles",
                                                                               "b7_vehicle_fuel/gas_vehicles","b7_vehicle_fuel/electricity_vehicles",
                                                                               "b7_vehicle_fuel/none_vehicles","b7_vehicle_fuel/none")]))),
                                new.value = rep(NA,70),
                                issue = rep('test_issue',70)
  ) %>%
    dplyr::tibble()



  testthat::expect_equal(expected_output ,actual_output)


  # test 2 - there's an '_other' column

  actual_output2 <- recode.multiple.set.NA(test_data, 'b18_financial_factors', issue = 'test_issue', other_var_name = 'b18_1_financial_factors_other')

  expected_output2 <- data.frame(uuid = unlist(rep(test_data[,'uuid'],11)),
                                 loop_index = rep(1:10,11),
                                 variable = c(rep("b18_financial_factors",10) ,rep("b18_financial_factors/not_affect",10) ,rep("b18_financial_factors/items_not_available",10) ,
                                              rep("b18_financial_factors/not_afford",10) ,rep("b18_financial_factors/not_enough_cash",10) ,rep("b18_financial_factors/public_transportation",10) ,
                                              rep("b18_financial_factors/fuel_expensive",10) ,rep("b18_financial_factors/prices_increased",10) ,rep("b18_financial_factors/other",10) ,
                                              rep("b18_1_financial_factors_other",10),rep("b18_financial_factors/prefer_not_answer",10)),
                                 old.value = c(as.vector(as.matrix(test_data[,c("b18_financial_factors","b18_financial_factors/not_affect","b18_financial_factors/items_not_available",
                                                                                "b18_financial_factors/not_afford","b18_financial_factors/not_enough_cash","b18_financial_factors/public_transportation",
                                                                                "b18_financial_factors/fuel_expensive","b18_financial_factors/prices_increased","b18_financial_factors/other",
                                                                                "b18_1_financial_factors_other","b18_financial_factors/prefer_not_answer")]))),
                                 new.value = rep(NA,110),
                                 issue = rep('test_issue',110)
  ) %>%
    dplyr::tibble() %>%
    dplyr::filter(!is.na(old.value))

  testthat::expect_equal(expected_output2 ,actual_output2)

  # test 3 - throws error when needed

  testthat::expect_error(recode.multiple.set.NA(test_data, 'fakevar', issue = 'test_issue', other_var_name = 'b18_1_financial_factors_other'))

  # test 4 returns empty df when passed an empty df

  test_df_empty <-  data.frame(matrix(nrow = 0 , ncol = ncol(test_data)))
  names(test_df_empty) <- names(test_data)

  actual_output3 <- recode.multiple.set.NA(test_df_empty, 'b18_financial_factors', issue = 'test_issue', other_var_name = 'b18_1_financial_factors_other')

  testthat::expect_equal(actual_output3, data.frame())

})



testthat::test_that("recode.multiple.set.choices works", {
  # upload the data

  test_path <- testthat::test_path("fixtures","utilityR_raw_data.xlsx")
  test_data <- readxl::read_excel(test_path)[41:50,] %>%
    dplyr::rename(uuid = `_uuid`)
  # set up loop index to test if it works
  test_data$loop_index = 1:10

  # load the tool data

  filename <- testthat::test_path("fixtures","tool.survey_full.xlsx")
  label_colname <- "label::English"
  tool.survey <- utilityR::load.tool.survey(filename,label_colname)


  # Test 0 - expect error with wrong names

  testthat::expect_error(recode.multiple.set.choices(test_data, 'b7_1_heating_fuel','fake_option',issue='test_issue', tool.survey = tool.survey,
                                                     tool.choices =tool.choices))


  # Test 1 - test if it works as expected with 1 variable


  # rename the tool.choices db
  tool.choices <- utilityR::utilityR_choices


  expected_output <- data.frame(uuid = c(test_data[!test_data$b7_1_heating_fuel=='gas_heating',]$uuid,test_data[test_data$`b7_1_heating_fuel/gas_heating`==0,]$uuid,
                                         test_data[test_data$`b7_1_heating_fuel/electricity_heating`==1,]$uuid),
                                loop_index=c(test_data[!test_data$b7_1_heating_fuel=='gas_heating',]$loop_index,test_data[test_data$`b7_1_heating_fuel/gas_heating`==0,]$loop_index,
                                             test_data[test_data$`b7_1_heating_fuel/electricity_heating`==1,]$loop_index),
                                old.value = c(test_data[!test_data$b7_1_heating_fuel=='gas_heating',]$b7_1_heating_fuel,0,1,1),
                                variable = c(rep('b7_1_heating_fuel',2),rep('b7_1_heating_fuel/gas_heating',1),rep('b7_1_heating_fuel/electricity_heating',2) ),
                                new.value = c(rep('gas_heating',2),1,0,0),
                                issue = rep('test_issue',5)
  ) %>% dplyr::tibble()
  actual_output <- recode.multiple.set.choices(test_data, 'b7_1_heating_fuel','gas_heating',issue='test_issue', tool.survey = tool.survey,
                                               tool.choices =tool.choices)


  testthat::expect_equal(expected_output, actual_output)

  # test 4 test if the function recodes multiple choices correctly + works with _other variables

  # set up the data
  test_data <- test_data %>%
    dplyr::select(uuid, loop_index, b17_access_stores, `b17_access_stores/no_impact`, `b17_access_stores/road_damage`,
                  `b17_access_stores/other`, b17_1_access_stores_other) %>%
    dplyr::mutate(`b17_access_stores/road_damage` = ifelse(loop_index < 8, 1,`b17_access_stores/road_damage`),
                  `b17_access_stores/other` = ifelse(loop_index==4, 1 ,`b17_access_stores/other`),
                  b17_1_access_stores_other = ifelse(loop_index==4, 'test' ,b17_1_access_stores_other)
    )

  expected_output2 <- data.frame(uuid = c(test_data$uuid, test_data[test_data$`b17_access_stores/no_impact`==0,]$uuid,
                                          test_data[test_data$`b17_access_stores/road_damage`==0,]$uuid,
                                          test_data[test_data$`b17_access_stores/other`==1,]$uuid,
                                          test_data[!is.na(test_data$b17_1_access_stores_other),]$uuid),
                                 loop_index = c(test_data$loop_index, test_data[test_data$`b17_access_stores/no_impact`==0,]$loop_index,
                                                test_data[test_data$`b17_access_stores/road_damage`==0,]$loop_index,
                                                test_data[test_data$`b17_access_stores/other`==1,]$loop_index,
                                                test_data[!is.na(test_data$b17_1_access_stores_other),]$loop_index),
                                 old.value = c(test_data$b17_access_stores, test_data[test_data$`b17_access_stores/no_impact`==0,]$`b17_access_stores/no_impact`,
                                               test_data[test_data$`b17_access_stores/road_damage`==0,]$`b17_access_stores/road_damage`,
                                               test_data[test_data$`b17_access_stores/other`==1,]$`b17_access_stores/other`,
                                               test_data[!is.na(test_data$b17_1_access_stores_other),]$b17_1_access_stores_other),
                                 variable = c(rep('b17_access_stores',10), rep('b17_access_stores/no_impact',4), rep('b17_access_stores/road_damage',3),
                                              rep('b17_access_stores/other',1),'b17_1_access_stores_other'),
                                 new.value = c(rep('no_impact road_damage',10), rep(1,7),0,NA),
                                 issue = rep('test_issue',19)
  ) %>% dplyr::tibble()

  actual_output2 <- recode.multiple.set.choices(test_data, 'b17_access_stores',c('no_impact','road_damage'),issue='test_issue', tool.survey = tool.survey,
                                                tool.choices =tool.choices)

  testthat::expect_equal(expected_output2, actual_output2)


  # test 5 - returns an empty df when no changes needed

  test_data <- readxl::read_excel(test_path)[41:50,] %>%
    dplyr::rename(uuid = `_uuid`)
  test_data$loop_index = 1:10

  test_data2 <- test_data[test_data$b7_1_heating_fuel=='gas_heating',]

  actual_output3 <- recode.multiple.set.choices(test_data2, 'b7_1_heating_fuel','gas_heating',issue='test_issue', tool.survey = tool.survey,
                                                tool.choices =tool.choices)

  testthat::expect_equal(data.frame(), actual_output3)


})


testthat::test_that("recode.multiple.add.choices works", {

  test_path <- testthat::test_path("fixtures","utilityR_raw_data.xlsx")
  test_data <- readxl::read_excel(test_path)[41:50,] %>%
    dplyr::rename(uuid = `_uuid`)
  # set up loop index to test if it works
  test_data$loop_index = 1:10

  expected_output <- data.frame(uuid = c(test_data[!test_data$b17_access_stores %in% c('air_alert power_outages','power_outages air_alert'),]$uuid,
                                         test_data[test_data$`b17_access_stores/air_alert` ==0,]$uuid,
                                         test_data[test_data$`b17_access_stores/power_outages` ==0,]$uuid
  ),
  loop_index = c(test_data[!test_data$b17_access_stores %in% c('air_alert power_outages','power_outages air_alert'),]$loop_index,
                 test_data[test_data$`b17_access_stores/air_alert` ==0,]$loop_index,
                 test_data[test_data$`b17_access_stores/power_outages` ==0,]$loop_index),
  old.value = c(test_data[!test_data$b17_access_stores %in% c('air_alert power_outages','power_outages air_alert'),]$b17_access_stores,
                rep(0,6), rep(0,6)),
  variable = c(rep('b17_access_stores',6), rep('b17_access_stores/air_alert',6), rep('b17_access_stores/power_outages',6)),
  new.value = c(paste0(test_data[!test_data$b17_access_stores %in% c('air_alert power_outages','power_outages air_alert'),]$b17_access_stores, ' air_alert power_outages'),
                rep(1,6), rep(1,6)),
  issue = rep('test_issue',18)
  ) %>% dplyr::tibble()

  actual_output <- recode.multiple.add.choices(test_data, "b17_access_stores", c("air_alert","power_outages"),issue='test_issue' )

  testthat::expect_equal(expected_output, actual_output)


  # test 2 - test if it breaks like its supposed to
  testthat::expect_error(recode.multiple.add.choices(test_data, "b17_access_stores", c("air_alert","fake_variable"),issue='test_issue' ))

  # test 3 - test if it breaks like its supposed to
  testthat::expect_error(recode.multiple.add.choices(test_data, "fake_column", c("air_alert"),issue='test_issue' ))

  # test 4 - test that it produces an empty df when there's nothing to change

  test_data2 <- test_data[test_data$b17_access_stores == 'no_impact',]

  actual_output <- recode.multiple.add.choices(test_data2, "b17_access_stores", c("no_impact"),issue='test_issue' )

  testthat::expect_equal(actual_output, data.frame())

})



testthat::test_that("recode.multiple.remove.choices works", {


  # test 1 check if the general functionality works

  test_path <- testthat::test_path("fixtures","utilityR_raw_data.xlsx")
  test_data <- readxl::read_excel(test_path)[41:50,] %>%
    dplyr::rename(uuid = `_uuid`)
  # set up loop index to test if it works
  test_data$loop_index = 1:10


  expected_output <- data.frame(uuid = c(test_data[grepl('(air_alert.*power_outages|power_outages.*air_alert)',test_data$b17_access_stores),]$uuid,
                                         test_data[test_data$`b17_access_stores/air_alert` ==1,]$uuid,
                                         test_data[test_data$`b17_access_stores/power_outages` ==1,]$uuid),
                                loop_index = c(test_data[grepl('(air_alert.*power_outages|power_outages.*air_alert)',test_data$b17_access_stores),]$loop_index,
                                               test_data[test_data$`b17_access_stores/air_alert` ==1,]$loop_index,
                                               test_data[test_data$`b17_access_stores/power_outages` ==1,]$loop_index),
                                old.value = c(test_data[grepl('(air_alert.*power_outages|power_outages.*air_alert)',test_data$b17_access_stores),]$b17_access_stores,
                                              rep(1,4), rep(1,4)),
                                variable = c(rep('b17_access_stores',4), rep('b17_access_stores/air_alert',4), rep('b17_access_stores/power_outages',4)),
                                new.value = c(rep("",4),rep(0,4), rep(0,4)),
                                issue = rep('test_issue',12)
  ) %>% dplyr::tibble()




  actual_output <- recode.multiple.remove.choices(test_data, "b17_access_stores", c("air_alert","power_outages"),issue='test_issue' )

  testthat::expect_equal(expected_output,actual_output)

  # Test 2 - test if the functionality of 'other' works


  test_data$`b17_access_stores/other`[5] = '1'
  test_data$b17_1_access_stores_other[5] = 'test'
  test_data$b17_access_stores[5] = paste(test_data$b17_access_stores[5],'other')


  expected_output2 <- data.frame(uuid = c(test_data[grepl('air_alert',test_data$b17_access_stores),]$uuid,
                                          test_data[grepl('other',test_data$b17_access_stores),]$uuid,
                                          test_data[test_data$`b17_access_stores/air_alert` ==1,]$uuid,
                                          test_data[test_data$`b17_access_stores/other` ==1,]$uuid,
                                          test_data[test_data$`b17_access_stores/other` ==1,]$uuid),
                                 loop_index = c(test_data[grepl('air_alert',test_data$b17_access_stores),]$loop_index,
                                                test_data[grepl('other',test_data$b17_access_stores),]$loop_index,
                                                test_data[test_data$`b17_access_stores/air_alert` ==1,]$loop_index,
                                                test_data[test_data$`b17_access_stores/other` ==1,]$loop_index,
                                                test_data[test_data$`b17_access_stores/other` ==1,]$loop_index),
                                 old.value = c(test_data[grepl('air_alert',test_data$b17_access_stores),]$b17_access_stores,
                                               test_data[grepl('other',test_data$b17_access_stores),]$b17_access_stores,
                                               rep(1,4), 1,'test'),
                                 variable = c(rep('b17_access_stores',5), rep('b17_access_stores/air_alert',4), rep('b17_access_stores/other',1), 'b17_1_access_stores_other'),
                                 new.value = c(rep("power_outages",4),'no_impact',rep(0,4), 0,NA ),
                                 issue = rep('test_issue',11)
  ) %>% dplyr::tibble()


  actual_output2 <- recode.multiple.remove.choices(test_data, "b17_access_stores", c("air_alert","other"),issue='test_issue' )

  testthat::expect_equal(expected_output2,actual_output2)


  # test 3 - test if it breaks like its supposed to
  testthat::expect_error(recode.multiple.remove.choices(test_data, "fake_column", c("air_alert","other"),issue='test_issue' ))

  # test 4 - test if it breaks like its supposed to
  testthat::expect_error(recode.multiple.remove.choices(test_data, "b17_access_stores", c("fake_choice","other"),issue='test_issue' ))

  # test 5 - test if it produces a df that only has the 'other'

  test_data2 <- test_data[!test_data$b17_access_stores == 'no_impact',]

  actual_output <- recode.multiple.remove.choices(test_data2, "b17_access_stores", c("no_impact"),issue='test_issue' )

  expected_output <- data.frame(
    uuid=test_data[grepl('other',test_data$b17_access_stores),]$uuid,
    loop_index=test_data[grepl('other',test_data$b17_access_stores),]$loop_index,
    old.value = c('no_impact other','1'),
    variable = c('b17_access_stores','b17_access_stores/no_impact'),
    new.value = c('other','0'),
    issue = 'test_issue'
  ) %>% dplyr::tibble()

  testthat::expect_equal(expected_output,actual_output)


  # test 6 - test if it produces an empty df when needed

  test_data2 <- test_data2[!test_data2$b17_access_stores == 'no_impact other',]
  actual_output <- recode.multiple.remove.choices(test_data2, "b17_access_stores", c("no_impact"),issue='test_issue' )


  testthat::expect_equal(actual_output, data.frame())

})




testthat::test_that("recode.others_select_one works", {
  # load the tool data

  filename <- testthat::test_path("fixtures","tool_others.xlsx")
  label_colname <- "label::English"
  tool.survey <- utilityR::load.tool.survey(filename,label_colname)

  # get the tool.choices db
  filename <- testthat::test_path("fixtures","tool_others.xlsx")

  tool.choices <- readxl::read_excel(filename, sheet = 'choices')

  # get the filled-out others file

  filename <- testthat::test_path("fixtures","other_requests_short.xlsx")
  other_requests <- readxl::read_excel(filename) %>%
    dplyr::filter(ref.type == 'select_one')

  actual_output <- recode.others_select_one(other_requests, tool.survey_others=tool.survey, tool.choices_others = tool.choices)

  expected_output <-  data.frame(uuid = c(other_requests$uuid[1:2],other_requests$uuid[7:8], # true
                                          rep(other_requests$uuid[3]),rep(other_requests$uuid[4]),
                                          rep(other_requests$uuid[9]),rep(other_requests$uuid[10]), # invalid
                                          rep(other_requests$uuid[3]),rep(other_requests$uuid[4]),
                                          rep(other_requests$uuid[9]),rep(other_requests$uuid[10]), # invalid other variable
                                          rep(other_requests$uuid[5]),rep(other_requests$uuid[6]),
                                          rep(other_requests$uuid[11]),rep(other_requests$uuid[12]), # existing
                                          rep(other_requests$uuid[5]),rep(other_requests$uuid[6]),
                                          rep(other_requests$uuid[11]),rep(other_requests$uuid[12]) # existing other variable
                                          # existing
  ),
  loop_index = c(other_requests$loop_index[1:2],other_requests$loop_index[7:8], # true
                 rep(other_requests$loop_index[3]),rep(other_requests$loop_index[4]),
                 rep(other_requests$loop_index[9]),rep(other_requests$loop_index[10]), # invalid
                 rep(other_requests$loop_index[3]),rep(other_requests$loop_index[4]),
                 rep(other_requests$loop_index[9]),rep(other_requests$loop_index[10]), # invalid other variable
                 rep(other_requests$loop_index[5]),rep(other_requests$loop_index[6]),
                 rep(other_requests$loop_index[11]),rep(other_requests$loop_index[12]), # existing
                 rep(other_requests$loop_index[5]),rep(other_requests$loop_index[6]),
                 rep(other_requests$loop_index[11]),rep(other_requests$loop_index[12]) # existing other variable
                 # existing
  ),
  variable = c(other_requests$name[1:2],other_requests$name[7:8], # true
               rep(other_requests$name[3]),rep(other_requests$name[4]),
               rep(other_requests$name[9]),rep(other_requests$name[10]), # invalid
               rep(other_requests$ref.name[3]),rep(other_requests$ref.name[4]),
               rep(other_requests$ref.name[9]),rep(other_requests$ref.name[10]), # invalid other variable
               rep(other_requests$name[5]),rep(other_requests$name[6]),
               rep(other_requests$name[11]),rep(other_requests$name[12]), # existing
               rep(other_requests$ref.name[5]),rep(other_requests$ref.name[6]),
               rep(other_requests$ref.name[11]),rep(other_requests$ref.name[12]) # existing other variable
  ),
  old.value = c(other_requests$response.uk[1:2],other_requests$response.uk[7:8], # true
                rep(other_requests$response.uk[3]),rep(other_requests$response.uk[4]),
                rep(other_requests$response.uk[9]),rep(other_requests$response.uk[10]), # invalid
                rep('other',4), # invalid other variable
                rep(other_requests$response.uk[5]),rep(other_requests$response.uk[6]),
                rep(other_requests$response.uk[11]),rep(other_requests$response.uk[12]), # existing
                rep('other',4) # existing other variable
  ),
  new.value = c(other_requests$true.v[1:2],other_requests$true.v[7:8], # true
                rep(NA,8), # invalid  variable
                rep(NA, 4), # existing
                'student_not_working','officially_employed_permanen_job','UKRs006888','UKRs006888'   # existing other variable
  ),
  issue = c(rep('Translating other response',4), # true
            rep('Invalid other response', 8 ),
            rep('Recoding other response', 8 )
  )
  ) %>%
    dplyr::tibble()

  testthat::expect_equal(actual_output, expected_output)

  # Test that if the entry has no recode needs it doesn't break

  actual_output <- recode.others_select_one(other_requests %>% dplyr::filter(is.na(existing.v)), tool.survey_others=tool.survey, tool.choices_others = tool.choices)

  expected_output <-  data.frame(uuid = c(other_requests$uuid[1:2],other_requests$uuid[7:8], # true
                                          rep(other_requests$uuid[3]),rep(other_requests$uuid[4]),
                                          rep(other_requests$uuid[9]),rep(other_requests$uuid[10]), # invalid
                                          rep(other_requests$uuid[3]),rep(other_requests$uuid[4]),
                                          rep(other_requests$uuid[9]),rep(other_requests$uuid[10]) # invalid other variable
                                          # existing
  ),
  loop_index = c(other_requests$loop_index[1:2],other_requests$loop_index[7:8], # true
                 rep(other_requests$loop_index[3]),rep(other_requests$loop_index[4]),
                 rep(other_requests$loop_index[9]),rep(other_requests$loop_index[10]), # invalid
                 rep(other_requests$loop_index[3]),rep(other_requests$loop_index[4]),
                 rep(other_requests$loop_index[9]),rep(other_requests$loop_index[10]) # invalid other variable
  ),
  variable = c(other_requests$name[1:2],other_requests$name[7:8], # true
               rep(other_requests$name[3]),rep(other_requests$name[4]),
               rep(other_requests$name[9]),rep(other_requests$name[10]), # invalid
               rep(other_requests$ref.name[3]),rep(other_requests$ref.name[4]),
               rep(other_requests$ref.name[9]),rep(other_requests$ref.name[10]) # invalid other variable
  ),
  old.value = c(other_requests$response.uk[1:2],other_requests$response.uk[7:8], # true
                rep(other_requests$response.uk[3]),rep(other_requests$response.uk[4]),
                rep(other_requests$response.uk[9]),rep(other_requests$response.uk[10]), # invalid
                rep('other',4) # invalid other variable
  ),
  new.value = c(other_requests$true.v[1:2],other_requests$true.v[7:8], # true
                rep(NA,8) # invalid  variable
                # existing other variable
  ),
  issue = c(rep('Translating other response',4), # true
            rep('Invalid other response', 8 )
  )
  ) %>%
    dplyr::tibble()

  testthat::expect_equal(actual_output, expected_output)


  # test if it throws an error when needed

  other_requests$existing.v[5] <- 'test_fake'

  testthat::expect_error(recode.others_select_one(other_requests, tool.survey_others=tool.survey, tool.choices_others = tool.choices))

})



testthat::test_that("recode.others_select_multiple works", {
  # load the tool data
  filename <- testthat::test_path("fixtures","tool_others.xlsx")
  label_colname <- "label::English"
  tool.survey <- utilityR::load.tool.survey(filename,label_colname)

  # get the tool.choices db
  filename <- testthat::test_path("fixtures","tool_others.xlsx")
  tool.choices <- readxl::read_excel(filename, sheet = 'choices')

  # get the dataframe
  filename <- testthat::test_path("fixtures","data_others.xlsx")
  test_data <- readxl::read_excel(filename)%>%
    dplyr::rename(uniqui = `_uuid`)


  # get the filled-out others file

  filename <- testthat::test_path("fixtures","other_requests_short.xlsx")
  other_requests <- readxl::read_excel(filename) %>%
    dplyr::filter(ref.type == 'select_multiple',
                  is.na(loop_index)) %>%
    dplyr::rename(uniqui = uuid) %>%
    dplyr::mutate(check = 2)

  actual_output <- recode.others_select_multiple(test_data,other_requests, tool.survey_others=tool.survey, tool.choices_others = tool.choices, is.loop =F)

  expected_output <-  data.frame(uniqui = c(other_requests$uniqui[1:2], rep(c('644ec1bf-4fec-4e93-b088-676dd2ae52ec','2a6bacd0-6a4d-420f-9463-cbf8a66cdb48'), 11),
                                            rep('f79999d6-192f-4b9b-aee9-5f613bd4e770',8)),
                                 loop_index = as.character(rep(NA,32)),
                                 variable = c(other_requests$name[1:2], 'q10_1_3_relationship_negativ_factors','q10_1_3_relationship_negativ_factors',
                                              'q10_1_3_relationship_negativ_factors/a_lack_of_sense_of_trust_between_the_idps_and_the_nonidps',
                                              'q10_1_3_relationship_negativ_factors/a_lack_of_sense_of_trust_between_the_idps_and_the_nonidps',
                                              'q10_1_3_relationship_negativ_factors/different_cultural_identities','q10_1_3_relationship_negativ_factors/different_cultural_identities',
                                              'q10_1_3_relationship_negativ_factors/different_language','q10_1_3_relationship_negativ_factors/different_language',
                                              'q10_1_3_relationship_negativ_factors/stereotypes_against_each_other','q10_1_3_relationship_negativ_factors/stereotypes_against_each_other',
                                              'q10_1_3_relationship_negativ_factors/a_lack_of_willingness_from_both_groups_to_interac','q10_1_3_relationship_negativ_factors/a_lack_of_willingness_from_both_groups_to_interac',
                                              'q10_1_3_relationship_negativ_factors/a_perceived_lack_of_proactivity_from_the_idps_in_trying_to_find_work',
                                              'q10_1_3_relationship_negativ_factors/a_perceived_lack_of_proactivity_from_the_idps_in_trying_to_find_work',
                                              'q10_1_3_relationship_negativ_factors/other','q10_1_3_relationship_negativ_factors/other',
                                              'q10_1_3_1_relationship_negativ_factors_other','q10_1_3_1_relationship_negativ_factors_other',
                                              'q10_1_3_relationship_negativ_factors/do_not_know','q10_1_3_relationship_negativ_factors/do_not_know',
                                              'q10_1_3_relationship_negativ_factors/prefer_not_to_answer','q10_1_3_relationship_negativ_factors/prefer_not_to_answer',
                                              'q10_2_1_discrimination_idp','q10_2_1_discrimination_idp/yes_we_feel_discriminated_against_when_trying_to_access_basic_services',
                                              'q10_2_1_discrimination_idp/other','q10_2_1_1_discrimination_idp_other','q2_4_3_main_cause',
                                              'q2_4_3_main_cause/security_considerations','q2_4_3_main_cause/other','q2_4_3_1_main_cause_other'
                                 ),
                                 old.value = c(other_requests$response.uk[1:2],'other','other','0','0','0','0','0','0','0','0','0','0','0',
                                               '0','1','1','Ничего не влияет','Нет негативных факторов','0','0','0','0','other','0','1',
                                               'Так зі сторони проживаючих тут студентів','other','0','1','Окупована територія'
                                 ),
                                 new.value = c(other_requests$true.v[1:2], rep(NA,22),'yes_we_feel_discriminated_against_when_trying_to_access_basic_services',
                                               '1','0',NA,'security_considerations','1','0',NA
                                 ),
                                 issue = c(rep('Translating other response',2),rep('Invalid other response',22),
                                           rep('Recoding other response',8))
  ) %>%
    dplyr::tibble()

  testthat::expect_equal(actual_output, expected_output)

  # test if it throws an error when needed
  other_requests2 <- other_requests
  other_requests2$existing.v[5] <- 'test_fake'

  testthat::expect_error(recode.others_select_multiple(test_data,other_requests2, tool.survey_others=tool.survey, tool.choices_others = tool.choices, is.loop =F))


  # if the multiple choice also exists
  other_requests$true.v[5]= 'Security considerations'

  expected_output <-  data.frame(uniqui = c(other_requests$uniqui[1:2], 'f79999d6-192f-4b9b-aee9-5f613bd4e770' ,rep(c('644ec1bf-4fec-4e93-b088-676dd2ae52ec','2a6bacd0-6a4d-420f-9463-cbf8a66cdb48'), 11),
                                            rep('f79999d6-192f-4b9b-aee9-5f613bd4e770',7)),
                                 loop_index = as.character(rep(NA,32)),
                                 variable = c(other_requests$name[1:2], 'q10_2_1_1_discrimination_idp_other','q10_1_3_relationship_negativ_factors','q10_1_3_relationship_negativ_factors',
                                              'q10_1_3_relationship_negativ_factors/a_lack_of_sense_of_trust_between_the_idps_and_the_nonidps',
                                              'q10_1_3_relationship_negativ_factors/a_lack_of_sense_of_trust_between_the_idps_and_the_nonidps',
                                              'q10_1_3_relationship_negativ_factors/different_cultural_identities','q10_1_3_relationship_negativ_factors/different_cultural_identities',
                                              'q10_1_3_relationship_negativ_factors/different_language','q10_1_3_relationship_negativ_factors/different_language',
                                              'q10_1_3_relationship_negativ_factors/stereotypes_against_each_other','q10_1_3_relationship_negativ_factors/stereotypes_against_each_other',
                                              'q10_1_3_relationship_negativ_factors/a_lack_of_willingness_from_both_groups_to_interac','q10_1_3_relationship_negativ_factors/a_lack_of_willingness_from_both_groups_to_interac',
                                              'q10_1_3_relationship_negativ_factors/a_perceived_lack_of_proactivity_from_the_idps_in_trying_to_find_work',
                                              'q10_1_3_relationship_negativ_factors/a_perceived_lack_of_proactivity_from_the_idps_in_trying_to_find_work',
                                              'q10_1_3_relationship_negativ_factors/other','q10_1_3_relationship_negativ_factors/other',
                                              'q10_1_3_1_relationship_negativ_factors_other','q10_1_3_1_relationship_negativ_factors_other',
                                              'q10_1_3_relationship_negativ_factors/do_not_know','q10_1_3_relationship_negativ_factors/do_not_know',
                                              'q10_1_3_relationship_negativ_factors/prefer_not_to_answer','q10_1_3_relationship_negativ_factors/prefer_not_to_answer',
                                              'q10_2_1_1_discrimination_idp_other','q10_2_1_discrimination_idp','q10_2_1_discrimination_idp/yes_we_feel_discriminated_against_when_trying_to_access_basic_services',
                                              'q2_4_3_main_cause','q2_4_3_main_cause/security_considerations','q2_4_3_main_cause/other',
                                              'q2_4_3_1_main_cause_other'
                                 ),
                                 old.value = c(other_requests$response.uk[1:2],'Так зі сторони проживаючих тут студентів','other','other','0','0','0',
                                               '0','0','0','0','0','0','0','0',
                                               '0','1','1','Ничего не влияет','Нет негативных факторов','0','0','0','0','Так зі сторони проживаючих тут студентів',
                                               'other','0','other','0','1','Окупована територія'
                                 ),
                                 new.value = c(other_requests$true.v[1:2], 'Security considerations',rep(NA,22),'Security considerations',
                                               'other yes_we_feel_discriminated_against_when_trying_to_access_basic_services',
                                               '1','security_considerations','1','0',NA
                                 ),
                                 issue = c(rep('Translating other response',3),rep('Invalid other response',22),'Translating other response',
                                           rep('Recoding other response',6))
  ) %>%
    dplyr::tibble()

  actual_output <- recode.others_select_multiple(test_data,other_requests, tool.survey_others=tool.survey, tool.choices_others = tool.choices, is.loop =F)

  testthat::expect_equal(actual_output, expected_output)


  # test if the function drops invalid IDs

  other_requests$uniqui[4:5]= 'fake_test'

  actual_output <- recode.others_select_multiple(test_data,other_requests, tool.survey_others=tool.survey, tool.choices_others = tool.choices, is.loop =F)

  expected_output <-  data.frame(uniqui = c(other_requests$uniqui[1:2], rep(c('644ec1bf-4fec-4e93-b088-676dd2ae52ec'), 11),
                                            rep('f79999d6-192f-4b9b-aee9-5f613bd4e770',4)
  ),
  loop_index = as.character(rep(NA,17)),
  variable = c(other_requests$name[1:2], 'q10_1_3_relationship_negativ_factors',
               'q10_1_3_relationship_negativ_factors/a_lack_of_sense_of_trust_between_the_idps_and_the_nonidps',
               'q10_1_3_relationship_negativ_factors/different_cultural_identities',
               'q10_1_3_relationship_negativ_factors/different_language',
               'q10_1_3_relationship_negativ_factors/stereotypes_against_each_other',
               'q10_1_3_relationship_negativ_factors/a_lack_of_willingness_from_both_groups_to_interac',
               'q10_1_3_relationship_negativ_factors/a_perceived_lack_of_proactivity_from_the_idps_in_trying_to_find_work',
               'q10_1_3_relationship_negativ_factors/other',
               'q10_1_3_1_relationship_negativ_factors_other',
               'q10_1_3_relationship_negativ_factors/do_not_know',
               'q10_1_3_relationship_negativ_factors/prefer_not_to_answer','q2_4_3_main_cause',
               'q2_4_3_main_cause/security_considerations','q2_4_3_main_cause/other',
               'q2_4_3_1_main_cause_other'
  ),
  old.value = c(other_requests$response.uk[1:2],'other','0','0','0','0','0','0',
                '1','Ничего не влияет','0','0','other','0','1','Окупована територія'
  ),
  new.value = c(other_requests$true.v[1:2],rep(NA,11),'security_considerations','1','0',NA
  ),
  issue = c(rep('Translating other response',2),rep('Invalid other response',11),
            rep('Recoding other response',4))
  ) %>%
    dplyr::tibble()

  testthat::expect_equal(actual_output, expected_output)

  # test if it doesn't break if a loop is uploaded



  # get the dataframe
  filename <- testthat::test_path("fixtures","data_others.xlsx")
  test_data <- suppressWarnings(readxl::read_excel(filename, sheet = 'loop'))
  test_data <- test_data %>%
    dplyr::rename(uniqui = `_index`)


  # get the filled-out others file

  filename <- testthat::test_path("fixtures","other_requests_short.xlsx")
  other_requests <- readxl::read_excel(filename) %>%
    dplyr::filter(ref.type == 'select_multiple',
                  !is.na(loop_index)) %>%
    dplyr::rename(uniqui = loop_index) %>%
    dplyr::mutate(check = 2)


  # expect error if uuid in the loop isn't provided

  testthat::expect_error(recode.others_select_multiple(test_data,other_requests, tool.survey_others=tool.survey, tool.choices_others = tool.choices, is.loop =T))

  # test if it runs well if everything is correct

  test_data <- test_data %>%
    dplyr::rename(uuid = `_submission__uuid`)

  actual_output <- recode.others_select_multiple(test_data,other_requests,
                                                 tool.survey_others=tool.survey,
                                                 tool.choices_others = tool.choices,
                                                 is.loop =F)


  expected_output <-  data.frame(uuid = c(other_requests$uuid[1:2], rep(c('2a7ab223-bc9e-4f15-ace2-feb857ac7742','e489957a-65d5-4777-b40b-6084a9559b82'), 13),
                                          rep('5f5bac4d-b250-41dc-93de-1b27043a2869',4),rep('efab8f40-dcb4-47c6-ba3f-fd89237a6f14',4)
  ),
  uniqui = c(other_requests$uniqui[1:2], rep(c(670,1210), 13),
             rep(494,4),rep(1168,4)
  ),
  variable = c(other_requests$name[1:2], rep('q2_1_4_members_vulnerabilities',2),
               rep('q2_1_4_members_vulnerabilities/none',2), rep('q2_1_4_members_vulnerabilities/chronic_illness_which_affects_the_quality_of_life',2),
               rep('q2_1_4_members_vulnerabilities/mental_health_concerns',2), rep('q2_1_4_members_vulnerabilities/person_with_disabilities',2),
               rep('q2_1_4_members_vulnerabilities/older_person',2), rep('q2_1_4_members_vulnerabilities/ethnic_minorities',2),
               rep('q2_1_4_members_vulnerabilities/pregnant_or_lactating',2),rep('q2_1_4_members_vulnerabilities/seperated_or_orphan_child',2),
               rep('q2_1_4_members_vulnerabilities/other',2), rep('q2_1_4_1_members_vulnerabilities_other',2),
               rep('q2_1_4_members_vulnerabilities/dont_know',2), rep('q2_1_4_members_vulnerabilities/prefer_not_to_answer',2),
               'q2_1_4_members_vulnerabilities','q2_1_4_members_vulnerabilities/person_with_disabilities',
               'q2_1_4_members_vulnerabilities/other','q2_1_4_1_members_vulnerabilities_other',
               'q2_1_4_members_vulnerabilities','q2_1_4_members_vulnerabilities/person_with_disabilities',
               'q2_1_4_members_vulnerabilities/other','q2_1_4_1_members_vulnerabilities_other'
  ),
  old.value = c(other_requests$response.uk[1:2],'other','other','0','0','0','0','0','0','0','0','0','0',
                '0','0','0','0','0','0','1','1','Важке інфікційне захворювання','У лікарні не був дуже давно',
                '0','0','0','0','other','0','1','3 група інвалідності',
                'chronic_illness_which_affects_the_quality_of_life other','0','1','Оформлюють інвалідність'
  ),
  new.value = c('IDP cannot run the business because of the war','There is a disease, can not work',rep(NA,26),'person_with_disabilities','1','0',NA,
                'chronic_illness_which_affects_the_quality_of_life person_with_disabilities','1','0',NA
  ),
  issue = c(rep('Translating other response',2),rep('Invalid other response',26),rep('Recoding other response',8)),
  loop_index = NA_character_

  ) %>%
    dplyr::tibble()

  testthat::expect_equal(actual_output, expected_output)


})



testthat::test_that("recode.others works", {


  # load the tool data
  filename <- testthat::test_path("fixtures","tool_others.xlsx")
  label_colname <- "label::English"
  tool.survey <- utilityR::load.tool.survey(filename,label_colname)

  # get the tool.choices db
  filename <- testthat::test_path("fixtures","tool_others.xlsx")
  tool.choices <- readxl::read_excel(filename, sheet = 'choices')

  # get the dataframe
  filename <- testthat::test_path("fixtures","data_others.xlsx")
  test_data <- readxl::read_excel(filename)%>%
    dplyr::rename(uuid = `_uuid`)


  # get the filled-out others file

  filename <- testthat::test_path("fixtures","other_requests_short.xlsx")
  other_requests <- readxl::read_excel(filename) %>%
    dplyr::filter(is.na(loop_index)) %>%
    dplyr::mutate(check = 2)


  actual_output <- recode.others(test_data,other_requests, tool.choices = tool.choices, tool.survey=tool.survey, is.loop =F)

  expected_output <-  data.frame(uuid = c('2343f19e-819c-4f1f-b827-cff4d9c7a953','db187669-7f5d-4d8b-9cba-aa212fd44da9',
                                          rep(c('bd005032-6f63-455f-8202-313583a128b1','f903166e-abc9-4258-848b-77ada6987d31'),2),
                                          rep(c('a46a1c10-bf18-4594-a0be-99447fa22116','51862558-1b68-466a-8e71-be2817dce5aa'),2),
                                          '10cef1b0-82ab-4cc2-bd59-bf9ade4fd1b6','7a526721-e59d-4bef-aa69-d80f9a9558b5',
                                          rep(c('644ec1bf-4fec-4e93-b088-676dd2ae52ec','2a6bacd0-6a4d-420f-9463-cbf8a66cdb48'),11),
                                          rep('f79999d6-192f-4b9b-aee9-5f613bd4e770',8)
  ),
  uniqui = c('2343f19e-819c-4f1f-b827-cff4d9c7a953','db187669-7f5d-4d8b-9cba-aa212fd44da9',
             rep(c('bd005032-6f63-455f-8202-313583a128b1','f903166e-abc9-4258-848b-77ada6987d31'),2),
             rep(c('a46a1c10-bf18-4594-a0be-99447fa22116','51862558-1b68-466a-8e71-be2817dce5aa'),2),
             '10cef1b0-82ab-4cc2-bd59-bf9ade4fd1b6','7a526721-e59d-4bef-aa69-d80f9a9558b5',
             rep(c('644ec1bf-4fec-4e93-b088-676dd2ae52ec','2a6bacd0-6a4d-420f-9463-cbf8a66cdb48'),11),
             rep('f79999d6-192f-4b9b-aee9-5f613bd4e770',8)
  ),
  loop_index =NA_character_,
  variable = c(rep('q0_4_2_1_center_idp_other',2),rep('q7_2_2_1_initiate_compensation_other',2),
               rep('q7_2_2_initiate_compensation',2), rep('q0_4_2_1_center_idp_other',2),
               rep('q0_4_2_center_idp',2), rep('q2_4_3_1_main_cause_other',2),
               rep('q10_1_3_relationship_negativ_factors',2),
               rep('q10_1_3_relationship_negativ_factors/a_lack_of_sense_of_trust_between_the_idps_and_the_nonidps',2),
               rep('q10_1_3_relationship_negativ_factors/different_cultural_identities',2),
               rep('q10_1_3_relationship_negativ_factors/different_language',2),
               rep('q10_1_3_relationship_negativ_factors/stereotypes_against_each_other',2),
               rep('q10_1_3_relationship_negativ_factors/a_lack_of_willingness_from_both_groups_to_interac',2),
               rep('q10_1_3_relationship_negativ_factors/a_perceived_lack_of_proactivity_from_the_idps_in_trying_to_find_work',2),
               rep('q10_1_3_relationship_negativ_factors/other',2),rep('q10_1_3_1_relationship_negativ_factors_other',2),
               rep('q10_1_3_relationship_negativ_factors/do_not_know',2),rep('q10_1_3_relationship_negativ_factors/prefer_not_to_answer',2),
               'q10_2_1_discrimination_idp','q10_2_1_discrimination_idp/yes_we_feel_discriminated_against_when_trying_to_access_basic_services',
               'q10_2_1_discrimination_idp/other','q10_2_1_1_discrimination_idp_other',
               'q2_4_3_main_cause','q2_4_3_main_cause/security_considerations',
               'q2_4_3_main_cause/other','q2_4_3_1_main_cause_other'
  ),
  old.value = c('Релігійна громада першої християнської церкви живого Бога м. Мукачево',
                'Релігійна громада першої Християнської Євангельської церкви Живого Бога у м.Мукачева',
                'Ніхто не знає чи хтось там живе','Респондент не верит в помощь от государства','other',
                'other','29','29','other','other','Евакуировали из-за травмы','В целях обследования','other',
                'other','0','0','0','0','0','0','0','0','0','0','0','0','1','1','Ничего не влияет',
                'Нет негативных факторов','0','0','0','0','other','0','1',
                'Так зі сторони проживаючих тут студентів','other','0','1','Окупована територія'),
  new.value = c('Religious community of the First Christian Church of the Living God in Mukachevo',
                'Religious community of the First Christian Evangelical Church of the Living God in Mukachevo',
                rep(NA,6),'UKRs006888','UKRs006888','evacuated due to injury','For the purpose of the survey',
                rep(NA,22),'yes_we_feel_discriminated_against_when_trying_to_access_basic_services','1','0',
                NA,'security_considerations','1','0',NA),
  issue = c(rep('Translating other response',2),rep('Invalid other response',4),rep('Recoding other response',4),
            rep('Translating other response',2),rep('Invalid other response',22),rep('Recoding other response',8)
  )
  ) %>%
    dplyr::tibble()

  testthat::expect_equal(actual_output, expected_output)



  # test if renaming breaks if a fake name is provided
  other_requests <- other_requests %>%
    dplyr::rename(fake_column = existing.v)

  testthat::expect_error(recode.others(test_data,other_requests, tool.choices = tool.choices, tool.survey=tool.survey, is.loop = F))


  # test if renaming works

  other_requests <- other_requests %>%
    dplyr::rename(existing.other = fake_column,
                  invalid.other = invalid.v,
                  true.other = true.v)

  actual_output <- recode.others(test_data,other_requests, tool.choices = tool.choices, tool.survey=tool.survey, is.loop =F)

  testthat::expect_equal(actual_output, expected_output)


  # test if it breaks if I remove the check column

  other_requests_test <- other_requests %>%
    dplyr::select(-check)

  testthat::expect_error(recode.others(test_data,other_requests_test, tool.choices = tool.choices, tool.survey=tool.survey, is.loop = F))


  # check if it works with fake IDs


  other_requests$uuid[10:11] <- c('fake_id','fake_id_2')

  actual_output <- suppressWarnings(recode.others(test_data,other_requests, tool.choices = tool.choices, tool.survey=tool.survey, is.loop = F))

  expected_output <-  data.frame(uuid = c('2343f19e-819c-4f1f-b827-cff4d9c7a953','db187669-7f5d-4d8b-9cba-aa212fd44da9',
                                          rep(c('bd005032-6f63-455f-8202-313583a128b1','f903166e-abc9-4258-848b-77ada6987d31'),2),
                                          rep(c('a46a1c10-bf18-4594-a0be-99447fa22116','51862558-1b68-466a-8e71-be2817dce5aa'),2),
                                          '10cef1b0-82ab-4cc2-bd59-bf9ade4fd1b6','7a526721-e59d-4bef-aa69-d80f9a9558b5',
                                          rep(c('644ec1bf-4fec-4e93-b088-676dd2ae52ec'),11),
                                          rep('f79999d6-192f-4b9b-aee9-5f613bd4e770',4)
  ),
  uniqui = c('2343f19e-819c-4f1f-b827-cff4d9c7a953','db187669-7f5d-4d8b-9cba-aa212fd44da9',
             rep(c('bd005032-6f63-455f-8202-313583a128b1','f903166e-abc9-4258-848b-77ada6987d31'),2),
             rep(c('a46a1c10-bf18-4594-a0be-99447fa22116','51862558-1b68-466a-8e71-be2817dce5aa'),2),
             '10cef1b0-82ab-4cc2-bd59-bf9ade4fd1b6','7a526721-e59d-4bef-aa69-d80f9a9558b5',
             rep(c('644ec1bf-4fec-4e93-b088-676dd2ae52ec'),11),
             rep('f79999d6-192f-4b9b-aee9-5f613bd4e770',4)
  ),
  loop_index =NA_character_,
  variable = c(rep('q0_4_2_1_center_idp_other',2),rep('q7_2_2_1_initiate_compensation_other',2),
               rep('q7_2_2_initiate_compensation',2), rep('q0_4_2_1_center_idp_other',2),
               rep('q0_4_2_center_idp',2), rep('q2_4_3_1_main_cause_other',2),
               'q10_1_3_relationship_negativ_factors',
               'q10_1_3_relationship_negativ_factors/a_lack_of_sense_of_trust_between_the_idps_and_the_nonidps',
               'q10_1_3_relationship_negativ_factors/different_cultural_identities',
               'q10_1_3_relationship_negativ_factors/different_language',
               'q10_1_3_relationship_negativ_factors/stereotypes_against_each_other',
               'q10_1_3_relationship_negativ_factors/a_lack_of_willingness_from_both_groups_to_interac',
               'q10_1_3_relationship_negativ_factors/a_perceived_lack_of_proactivity_from_the_idps_in_trying_to_find_work',
               'q10_1_3_relationship_negativ_factors/other','q10_1_3_1_relationship_negativ_factors_other',
               'q10_1_3_relationship_negativ_factors/do_not_know','q10_1_3_relationship_negativ_factors/prefer_not_to_answer',
               'q2_4_3_main_cause','q2_4_3_main_cause/security_considerations',
               'q2_4_3_main_cause/other','q2_4_3_1_main_cause_other'
  ),
  old.value = c('Релігійна громада першої християнської церкви живого Бога м. Мукачево','Релігійна громада першої Християнської Євангельської церкви Живого Бога у м.Мукачева',
                'Ніхто не знає чи хтось там живе','Респондент не верит в помощь от государства','other','other','29','29','other',
                'other','Евакуировали из-за травмы','В целях обследования','other','0','0','0','0','0','0','1','Ничего не влияет','0',
                '0','other','0','1','Окупована територія'),
  new.value = c('Religious community of the First Christian Church of the Living God in Mukachevo',
                'Religious community of the First Christian Evangelical Church of the Living God in Mukachevo',
                rep(NA,6),'UKRs006888','UKRs006888','evacuated due to injury','For the purpose of the survey',
                rep(NA,11),'security_considerations','1','0',NA
  ),
  issue = c(rep('Translating other response',2),rep('Invalid other response',4),rep('Recoding other response',4),
            rep('Translating other response',2),rep('Invalid other response',11),rep('Recoding other response',4)
  )
  ) %>%
    dplyr::tibble()


  testthat::expect_equal(actual_output, expected_output)


  # test if it breaks if it's all fake IDs

  other_requests$uuid <- 'fake_id_3'


  testthat::expect_error(recode.others(test_data,other_requests, tool.choices = tool.choices, tool.survey=tool.survey, is.loop = F))



  # test if it works fine with loops ------------------------------


  # # get the dataframe
  filename <- testthat::test_path("fixtures","data_others.xlsx")
  test_data <- suppressWarnings(readxl::read_excel(filename, sheet = 'loop'))
  test_data <- test_data %>%
    dplyr::rename(uuid = `_submission__uuid`)

  # get the filled-out others file

  filename <- testthat::test_path("fixtures","other_requests_short.xlsx")
  other_requests <- readxl::read_excel(filename) %>%
    dplyr::filter(!is.na(loop_index)) %>%
    dplyr::mutate(check = 2)


  # expect error if loop_index columns isn't provided

  testthat::expect_error(recode.others(test_data,other_requests, tool.choices = tool.choices, tool.survey=tool.survey, is.loop = T))

  # add the loop_index column

  test_data <- test_data %>%
    dplyr::rename(loop_index = `_index`)


  actual_output <- recode.others(test_data,other_requests, tool.choices = tool.choices, tool.survey=tool.survey, is.loop = T)


  expected_output <-  data.frame(uuid = c('5d89dcd0-f1e5-449a-b0c1-8bc92cf8f2ba','8764de9a-6a89-4eb8-82ae-f2b1e2428c03',
                                          rep(c('c25aa98d-36e7-4788-bd3b-b4da3b4a6415','abef16e7-ac19-47d5-9914-16cc40580b22'),2),
                                          rep(c('3dc15f20-8964-419c-9ab0-f7b9d367120e','3402ebff-f400-478b-a77e-0153eeb67a86'),2),
                                          'fa786ce0-df19-4af9-bca1-8b2d6f520589','fbc3020e-04f2-4ab5-b644-0d6934a947b7',
                                          rep(c('2a7ab223-bc9e-4f15-ace2-feb857ac7742','e489957a-65d5-4777-b40b-6084a9559b82'),13),
                                          rep('5f5bac4d-b250-41dc-93de-1b27043a2869',4), rep('efab8f40-dcb4-47c6-ba3f-fd89237a6f14',4)
  ),
  uniqui = c(1040,95, rep(c(331,241),2), rep(c(2802,486),2), 2718,666, rep(c(670,1210),13),
             rep(494,4), rep(1168,4)
  ),
  loop_index = c(1040,95, rep(c(331,241),2), rep(c(2802,486),2), 2718,666, rep(c(670,1210),13),
                 rep(494,4), rep(1168,4)
  ),
  variable = c(rep('q2_3_4_1_employment_situation_last_week_other',2),
               'q2_3_7_1_sector_working_other','q2_3_8_1_sector_working_currently_other','q2_3_7_sector_working',
               'q2_3_8_sector_working_currently',rep('q2_3_3_1_employment_situation_other',2),
               rep('q2_3_3_employment_situation',2),'q2_3_5_1_unemployed_reasons_other','q2_3_6_1_underemployment_other',
               rep('q2_1_4_members_vulnerabilities',2),rep('q2_1_4_members_vulnerabilities/none',2),
               rep('q2_1_4_members_vulnerabilities/chronic_illness_which_affects_the_quality_of_life',2),
               rep('q2_1_4_members_vulnerabilities/mental_health_concerns',2),
               rep('q2_1_4_members_vulnerabilities/person_with_disabilities',2),
               rep('q2_1_4_members_vulnerabilities/older_person',2),
               rep('q2_1_4_members_vulnerabilities/ethnic_minorities',2),
               rep('q2_1_4_members_vulnerabilities/pregnant_or_lactating',2),
               rep('q2_1_4_members_vulnerabilities/seperated_or_orphan_child',2),
               rep('q2_1_4_members_vulnerabilities/other',2),
               rep('q2_1_4_1_members_vulnerabilities_other',2),
               rep('q2_1_4_members_vulnerabilities/dont_know',2),rep('q2_1_4_members_vulnerabilities/prefer_not_to_answer',2),
               'q2_1_4_members_vulnerabilities','q2_1_4_members_vulnerabilities/person_with_disabilities',
               'q2_1_4_members_vulnerabilities/other','q2_1_4_1_members_vulnerabilities_other','q2_1_4_members_vulnerabilities',
               'q2_1_4_members_vulnerabilities/person_with_disabilities','q2_1_4_members_vulnerabilities/other',
               'q2_1_4_1_members_vulnerabilities_other'
  ),
  old.value = c('Працевлаштований, але зараз підприємство знаходиться на простої','Числиться за місцем проживання офіційно',
                'В декретном отпуске','Не працюю','other','other','17 років, навчання в школі','Часний підприємиць','other','other',
                'Не може вести свій бізнес через війну','Є захворювання не може працювати','other','other',
                rep('0',16),'1','1','Важке інфікційне захворювання','У лікарні не був дуже давно',
                '0','0','0','0','other','0','1','3 група інвалідності',
                'chronic_illness_which_affects_the_quality_of_life other','0','1','Оформлюють інвалідність'),
  new.value = c('Employed, but now the enterprise is idle','listed at the place of residence officially',
                rep(NA,6),'student_not_working','officially_employed_permanen_job','IDP cannot run the business because of the war',
                'There is a disease, can not work',rep(NA,26),'person_with_disabilities','1','0',NA,
                'chronic_illness_which_affects_the_quality_of_life person_with_disabilities','1','0',NA
  ),
  issue = c(rep('Translating other response',2),rep('Invalid other response',4),rep('Recoding other response',4),
            rep('Translating other response',2),rep('Invalid other response',26),rep('Recoding other response',8)
  )
  ) %>%
    dplyr::tibble() %>%
    dplyr::mutate(uniqui = as.character(uniqui),
                  loop_index = as.character(loop_index))

  testthat::expect_equal(actual_output, expected_output)


  # test if it works if there are no select_multiple rows in the requests.

  # get the dataframe
  filename <- testthat::test_path("fixtures","data_others.xlsx")
  test_data <- readxl::read_excel(filename)%>%
    dplyr::rename(uuid = `_uuid`)


  # get the filled-out others file

  filename <- testthat::test_path("fixtures","other_requests_short.xlsx")
  other_requests <- readxl::read_excel(filename) %>%
    dplyr::filter(is.na(loop_index),
                  ref.type == 'select_one') %>%
    dplyr::mutate(check = 2)



  actual_output <- recode.others(test_data,other_requests, tool.choices = tool.choices, tool.survey=tool.survey, is.loop = F)

  expected_output <- data.frame(uuid = c('2343f19e-819c-4f1f-b827-cff4d9c7a953','db187669-7f5d-4d8b-9cba-aa212fd44da9',
                                         rep(c('bd005032-6f63-455f-8202-313583a128b1','f903166e-abc9-4258-848b-77ada6987d31'),2),
                                         rep(c('a46a1c10-bf18-4594-a0be-99447fa22116','51862558-1b68-466a-8e71-be2817dce5aa'),2)
  ),
  uniqui = c('2343f19e-819c-4f1f-b827-cff4d9c7a953','db187669-7f5d-4d8b-9cba-aa212fd44da9',
             rep(c('bd005032-6f63-455f-8202-313583a128b1','f903166e-abc9-4258-848b-77ada6987d31'),2),
             rep(c('a46a1c10-bf18-4594-a0be-99447fa22116','51862558-1b68-466a-8e71-be2817dce5aa'),2)

  ),
  loop_index = NA_character_,
  variable = c(rep('q0_4_2_1_center_idp_other',2),rep('q7_2_2_1_initiate_compensation_other',2),
               rep('q7_2_2_initiate_compensation',2), rep('q0_4_2_1_center_idp_other',2),
               rep('q0_4_2_center_idp',2)
  ),
  old.value = c('Релігійна громада першої християнської церкви живого Бога м. Мукачево','Релігійна громада першої Християнської Євангельської церкви Живого Бога у м.Мукачева',
                'Ніхто не знає чи хтось там живе','Респондент не верит в помощь от государства','other','other','29',
                '29','other','other'
  ),
  new.value = c('Religious community of the First Christian Church of the Living God in Mukachevo','Religious community of the First Christian Evangelical Church of the Living God in Mukachevo',
                rep(NA,6),'UKRs006888','UKRs006888'),
  issue = c(rep('Translating other response',2),rep('Invalid other response',4),rep('Recoding other response',4))
  ) %>%
    dplyr::tibble()

  testthat::expect_equal(actual_output,expected_output)


  # test if it works if there are no select_one rows in the requests.

  # get the filled-out others file

  filename <- testthat::test_path("fixtures","other_requests_short.xlsx")
  other_requests <- readxl::read_excel(filename) %>%
    dplyr::filter(is.na(loop_index),
                  ref.type == 'select_multiple') %>%
    dplyr::mutate(check = 2)


  actual_output <- recode.others(test_data,other_requests, tool.choices = tool.choices, tool.survey=tool.survey, is.loop =F)


  expected_output <- data.frame(uuid = c('10cef1b0-82ab-4cc2-bd59-bf9ade4fd1b6','7a526721-e59d-4bef-aa69-d80f9a9558b5',
                                         rep(c('644ec1bf-4fec-4e93-b088-676dd2ae52ec','2a6bacd0-6a4d-420f-9463-cbf8a66cdb48'),11),
                                         rep('f79999d6-192f-4b9b-aee9-5f613bd4e770',8)
  ),
  uniqui = c('10cef1b0-82ab-4cc2-bd59-bf9ade4fd1b6','7a526721-e59d-4bef-aa69-d80f9a9558b5',
             rep(c('644ec1bf-4fec-4e93-b088-676dd2ae52ec','2a6bacd0-6a4d-420f-9463-cbf8a66cdb48'),11),
             rep('f79999d6-192f-4b9b-aee9-5f613bd4e770',8)
  ),
  loop_index = NA_character_,
  variable = c(rep('q2_4_3_1_main_cause_other',2),
               rep('q10_1_3_relationship_negativ_factors',2),
               rep('q10_1_3_relationship_negativ_factors/a_lack_of_sense_of_trust_between_the_idps_and_the_nonidps',2),
               rep('q10_1_3_relationship_negativ_factors/different_cultural_identities',2),
               rep('q10_1_3_relationship_negativ_factors/different_language',2),
               rep('q10_1_3_relationship_negativ_factors/stereotypes_against_each_other',2),
               rep('q10_1_3_relationship_negativ_factors/a_lack_of_willingness_from_both_groups_to_interac',2),
               rep('q10_1_3_relationship_negativ_factors/a_perceived_lack_of_proactivity_from_the_idps_in_trying_to_find_work',2),
               rep('q10_1_3_relationship_negativ_factors/other',2),rep('q10_1_3_1_relationship_negativ_factors_other',2),
               rep('q10_1_3_relationship_negativ_factors/do_not_know',2),rep('q10_1_3_relationship_negativ_factors/prefer_not_to_answer',2),
               'q10_2_1_discrimination_idp','q10_2_1_discrimination_idp/yes_we_feel_discriminated_against_when_trying_to_access_basic_services',
               'q10_2_1_discrimination_idp/other','q10_2_1_1_discrimination_idp_other',
               'q2_4_3_main_cause','q2_4_3_main_cause/security_considerations',
               'q2_4_3_main_cause/other','q2_4_3_1_main_cause_other'
  ),
  old.value = c('Евакуировали из-за травмы','В целях обследования','other',
                'other','0','0','0','0','0','0','0','0','0','0','0','0','1','1','Ничего не влияет',
                'Нет негативных факторов','0','0','0','0','other','0','1',
                'Так зі сторони проживаючих тут студентів','other','0','1','Окупована територія'),
  new.value = c('evacuated due to injury','For the purpose of the survey',
                rep(NA,22),'yes_we_feel_discriminated_against_when_trying_to_access_basic_services','1','0',
                NA,'security_considerations','1','0',NA),
  issue = c(rep('Translating other response',2),rep('Invalid other response',22),rep('Recoding other response',8)
  )
  ) %>%
    dplyr::tibble()

  testthat::expect_equal(actual_output,expected_output)


  # will it break if we have a loop + only one type of question?


  # # get the dataframe
  filename <- testthat::test_path("fixtures","data_others.xlsx")
  test_data <- suppressWarnings(readxl::read_excel(filename, sheet = 'loop'))
  test_data <- test_data %>%
    dplyr::rename(uuid = `_submission__uuid`,
                  loop_index = `_index`)

  # get the filled-out others file

  filename <- testthat::test_path("fixtures","other_requests_short.xlsx")
  other_requests <- readxl::read_excel(filename) %>%
    dplyr::filter(!is.na(loop_index),
                  ref.type == 'select_one') %>%
    dplyr::mutate(check = 2)


  actual_output <- recode.others(test_data,other_requests, tool.choices = tool.choices, tool.survey=tool.survey, is.loop = T)

  expected_output <- data.frame(uuid = c('5d89dcd0-f1e5-449a-b0c1-8bc92cf8f2ba','8764de9a-6a89-4eb8-82ae-f2b1e2428c03',
                                         rep(c('c25aa98d-36e7-4788-bd3b-b4da3b4a6415','abef16e7-ac19-47d5-9914-16cc40580b22'),2),
                                         rep(c('3dc15f20-8964-419c-9ab0-f7b9d367120e','3402ebff-f400-478b-a77e-0153eeb67a86'),2)),
                                uniqui = c('1040','95', rep(c('331','241'),2), rep(c('2802','486'),2)),
                                loop_index = c('1040','95', rep(c('331','241'),2), rep(c('2802','486'),2)),
                                variable = c(rep('q2_3_4_1_employment_situation_last_week_other',2),
                                             'q2_3_7_1_sector_working_other','q2_3_8_1_sector_working_currently_other','q2_3_7_sector_working',
                                             'q2_3_8_sector_working_currently',rep('q2_3_3_1_employment_situation_other',2),
                                             rep('q2_3_3_employment_situation',2)),
                                old.value = c('Працевлаштований, але зараз підприємство знаходиться на простої','Числиться за місцем проживання офіційно',
                                              'В декретном отпуске','Не працюю','other','other','17 років, навчання в школі','Часний підприємиць','other','other'),
                                new.value = c('Employed, but now the enterprise is idle','listed at the place of residence officially',
                                              rep(NA,6),'student_not_working','officially_employed_permanen_job'),
                                issue = c(rep('Translating other response',2),rep('Invalid other response',4),rep('Recoding other response',4))


  ) %>%
    dplyr::tibble()

  testthat::expect_equal(actual_output,expected_output)


  # loop + no select_one

  filename <- testthat::test_path("fixtures","other_requests_short.xlsx")
  other_requests <- readxl::read_excel(filename) %>%
    dplyr::filter(!is.na(loop_index),
                  ref.type == 'select_multiple') %>%
    dplyr::mutate(check = 2)



  actual_output <- recode.others(test_data,other_requests, tool.choices = tool.choices, tool.survey=tool.survey, is.loop = T)


  expected_output <-  data.frame(uuid = c('fa786ce0-df19-4af9-bca1-8b2d6f520589','fbc3020e-04f2-4ab5-b644-0d6934a947b7',
                                          rep(c('2a7ab223-bc9e-4f15-ace2-feb857ac7742','e489957a-65d5-4777-b40b-6084a9559b82'),13),
                                          rep('5f5bac4d-b250-41dc-93de-1b27043a2869',4), rep('efab8f40-dcb4-47c6-ba3f-fd89237a6f14',4)
  ),
  uniqui = c(2718,666, rep(c(670,1210),13),rep(494,4), rep(1168,4)),
  loop_index = c(2718,666, rep(c(670,1210),13),rep(494,4), rep(1168,4)),
  variable = c('q2_3_5_1_unemployed_reasons_other','q2_3_6_1_underemployment_other',
               rep('q2_1_4_members_vulnerabilities',2),rep('q2_1_4_members_vulnerabilities/none',2),
               rep('q2_1_4_members_vulnerabilities/chronic_illness_which_affects_the_quality_of_life',2),
               rep('q2_1_4_members_vulnerabilities/mental_health_concerns',2),
               rep('q2_1_4_members_vulnerabilities/person_with_disabilities',2),
               rep('q2_1_4_members_vulnerabilities/older_person',2),
               rep('q2_1_4_members_vulnerabilities/ethnic_minorities',2),
               rep('q2_1_4_members_vulnerabilities/pregnant_or_lactating',2),
               rep('q2_1_4_members_vulnerabilities/seperated_or_orphan_child',2),
               rep('q2_1_4_members_vulnerabilities/other',2),
               rep('q2_1_4_1_members_vulnerabilities_other',2),
               rep('q2_1_4_members_vulnerabilities/dont_know',2),rep('q2_1_4_members_vulnerabilities/prefer_not_to_answer',2),
               'q2_1_4_members_vulnerabilities','q2_1_4_members_vulnerabilities/person_with_disabilities',
               'q2_1_4_members_vulnerabilities/other','q2_1_4_1_members_vulnerabilities_other','q2_1_4_members_vulnerabilities',
               'q2_1_4_members_vulnerabilities/person_with_disabilities','q2_1_4_members_vulnerabilities/other',
               'q2_1_4_1_members_vulnerabilities_other'
  ),
  old.value = c('Не може вести свій бізнес через війну','Є захворювання не може працювати','other','other',
                rep('0',16),'1','1','Важке інфікційне захворювання','У лікарні не був дуже давно',
                '0','0','0','0','other','0','1','3 група інвалідності',
                'chronic_illness_which_affects_the_quality_of_life other','0','1','Оформлюють інвалідність'),
  new.value = c('IDP cannot run the business because of the war',
                'There is a disease, can not work',rep(NA,26),'person_with_disabilities','1','0',NA,
                'chronic_illness_which_affects_the_quality_of_life person_with_disabilities','1','0',NA
  ),
  issue = c(rep('Translating other response',2),rep('Invalid other response',26),rep('Recoding other response',8))
  ) %>%
    dplyr::tibble() %>%
    dplyr::mutate(uniqui = as.character(uniqui),
                  loop_index = as.character(loop_index))

  testthat::expect_equal(actual_output, expected_output)

})

testthat::test_that('recode.trans.requests works',{

  test_data <- data.frame(uuid = c("a0d73ff7-7f8c-4b0e-b13b-a909fe9b0aa7",
                                   "b231339d-1b74-4e5a-bfad-037028fb71d8", "e2a71393-61a1-48bb-bc10-a061c2ec98cc",
                                   "7faf7bf1-a52b-4011-8590-e972b2fa0d0d", "3d5e12e7-c269-4fd6-8162-ce17342b9ca8",
                                   "3495679e-0d16-4658-ae9b-caf0e283f736", "a0d73ff7-7f8c-4b0e-b13b-a909fe9b0aa7",
                                   "9c0ddcef-fef8-4f8f-9f72-59292187090c", "9c4b2e89-1ec7-4b3c-aeee-1395b960b5be",
                                   "27e07fda-fffd-4abe-9dad-ebcc03a7f84f"),
                          loop_index = c(NA, NA, NA, NA, NA, "loop1_1247", NA, NA, NA, NA),
                          name = c("conditions_to_pursue_option_other",
                                   "conditions_to_pursue_option_other", "conditions_to_pursue_option_other",
                                   "conditions_to_pursue_option_other", "conditions_to_pursue_option_other",
                                   "ed_barriers", "reasons_feeling_of_safety", "reasons_feeling_of_safety",
                                   "reasons_feeling_of_safety", "reasons_feeling_of_safety"),
                          responses = c("999",
                                        "мешкає за кордоном ДГ", "на милицях мусить ходити до туалету на інше крило",
                                        "нет порогов, удобно завозить инвалидную коляску ребенка",
                                        "Є робрта і спокійно", "В ДХ нет ноутбука, компьютера для выполнения домашних заданий,",
                                        "999", "в н.п. ви чувствуете спокойно",
                                        "Здатне трещит", "Порівняно з домом 5"),
                          response.en = c("999", "Lives abroad DG", "On crutches he has to go to the toilet on the other wing",
                                          "There are no thresholds, it is convenient to bring a child's wheelchair",
                                          "There is a robrta and calm", "There is no laptop in the house, a computer for homework,",
                                          "999", "In the n.p. vi you feel calm", "It's crackling", "compared to house 5"),
                          true.v = c(NA, "Lives abroad", "He moves with the help of crutches, but he has to go to the toilet on the other wing",
                                     "There are no thresholds(doorstep, first step of a porch) it is convenient to bring in and out a child's wheelchair",
                                     "There's work here and it's quiet", "HH has no laptop to do hometasks.",
                                     NA, "In the inhabited locality (settlement) you feel yourself calm",
                                     "The building is cracking", "in comparison with the 5th house"),
                          invalid.v = c("yes", NA, NA, NA, NA, NA, "yes", NA, NA, NA))
  #Test 1 works fine on it's own

  actual_output <- recode.trans.requests(requests = test_data, response_col = 'responses')
  expected_output <- data.frame(
    uuid = c("a0d73ff7-7f8c-4b0e-b13b-a909fe9b0aa7","a0d73ff7-7f8c-4b0e-b13b-a909fe9b0aa7",
             "b231339d-1b74-4e5a-bfad-037028fb71d8", "e2a71393-61a1-48bb-bc10-a061c2ec98cc",
             "7faf7bf1-a52b-4011-8590-e972b2fa0d0d", "3d5e12e7-c269-4fd6-8162-ce17342b9ca8",
             "3495679e-0d16-4658-ae9b-caf0e283f736",
             "9c0ddcef-fef8-4f8f-9f72-59292187090c", "9c4b2e89-1ec7-4b3c-aeee-1395b960b5be",
             "27e07fda-fffd-4abe-9dad-ebcc03a7f84f"),
    loop_index = c(NA, NA, NA, NA, NA, NA, "loop1_1247", NA, NA, NA),
    variable = c("conditions_to_pursue_option_other","reasons_feeling_of_safety",
                 "conditions_to_pursue_option_other", "conditions_to_pursue_option_other",
                 "conditions_to_pursue_option_other", "conditions_to_pursue_option_other",
                 "ed_barriers", "reasons_feeling_of_safety",
                 "reasons_feeling_of_safety", "reasons_feeling_of_safety"),
    old.value = c("999","999",
                  "мешкає за кордоном ДГ", "на милицях мусить ходити до туалету на інше крило",
                  "нет порогов, удобно завозить инвалидную коляску ребенка",
                  "Є робрта і спокійно", "В ДХ нет ноутбука, компьютера для выполнения домашних заданий,",
                  "в н.п. ви чувствуете спокойно",
                  "Здатне трещит", "Порівняно з домом 5"),
    new.value = c(NA,NA, "Lives abroad", "He moves with the help of crutches, but he has to go to the toilet on the other wing",
                  "There are no thresholds(doorstep, first step of a porch) it is convenient to bring in and out a child's wheelchair",
                  "There's work here and it's quiet", "HH has no laptop to do hometasks.",
                  "In the inhabited locality (settlement) you feel yourself calm",
                  "The building is cracking", "in comparison with the 5th house"),
    issue = c('Invalid response', 'Invalid response',rep('Translating other response',8))
  )
  testthat::expect_equal(actual_output,expected_output)

  # test 2 should work fine without any invalid entries

  test_data2 <- test_data[is.na(test_data$invalid.v),]

  actual_output <- recode.trans.requests(requests = test_data2, response_col = 'responses')
  expected_output <- data.frame(
    uuid = c(
      "b231339d-1b74-4e5a-bfad-037028fb71d8", "e2a71393-61a1-48bb-bc10-a061c2ec98cc",
      "7faf7bf1-a52b-4011-8590-e972b2fa0d0d", "3d5e12e7-c269-4fd6-8162-ce17342b9ca8",
      "3495679e-0d16-4658-ae9b-caf0e283f736",
      "9c0ddcef-fef8-4f8f-9f72-59292187090c", "9c4b2e89-1ec7-4b3c-aeee-1395b960b5be",
      "27e07fda-fffd-4abe-9dad-ebcc03a7f84f"),
    loop_index = c( NA, NA, NA, NA, "loop1_1247", NA, NA, NA),
    variable = c(
      "conditions_to_pursue_option_other", "conditions_to_pursue_option_other",
      "conditions_to_pursue_option_other", "conditions_to_pursue_option_other",
      "ed_barriers", "reasons_feeling_of_safety",
      "reasons_feeling_of_safety", "reasons_feeling_of_safety"),
    old.value = c(
      "мешкає за кордоном ДГ", "на милицях мусить ходити до туалету на інше крило",
      "нет порогов, удобно завозить инвалидную коляску ребенка",
      "Є робрта і спокійно", "В ДХ нет ноутбука, компьютера для выполнения домашних заданий,",
      "в н.п. ви чувствуете спокойно",
      "Здатне трещит", "Порівняно з домом 5"),
    new.value = c("Lives abroad", "He moves with the help of crutches, but he has to go to the toilet on the other wing",
                  "There are no thresholds(doorstep, first step of a porch) it is convenient to bring in and out a child's wheelchair",
                  "There's work here and it's quiet", "HH has no laptop to do hometasks.",
                  "In the inhabited locality (settlement) you feel yourself calm",
                  "The building is cracking", "in comparison with the 5th house"),
    issue = c(rep('Translating other response',8))
  )
  testthat::expect_equal(actual_output,expected_output)

  # test 3 should work fine without any valid entries

  test_data2 <- test_data[!is.na(test_data$invalid.v),]

  actual_output <- recode.trans.requests(requests = test_data2, response_col = 'responses')
  expected_output <- data.frame(
    uuid = c("a0d73ff7-7f8c-4b0e-b13b-a909fe9b0aa7","a0d73ff7-7f8c-4b0e-b13b-a909fe9b0aa7"),
    loop_index = c(NA_character_, NA_character_),
    variable = c("conditions_to_pursue_option_other","reasons_feeling_of_safety"),
    old.value = c("999","999"),
    new.value = c(NA,NA ),
    issue = c('Invalid response', 'Invalid response')
  )
  testthat::expect_equal(actual_output,expected_output)

  # test 4 - throws an error if we pass an incomplete frame

  test_data2 <- test_data[, -which(names(test_data) %in% c('invalid.v'))]
  testthat::expect_error(
    recode.trans.requests(requests = test_data2, response_col = 'responses'),
    'invalid.v column is not in the present in your translated requests file.
         Please double check and make sure to load it with the load.requests function'
  )

  # test 5 - throws another error if we pass an incomplete frame

  test_data2 <- test_data[, -which(names(test_data) %in% c('true.v'))]
  testthat::expect_error(
    recode.trans.requests(requests = test_data2, response_col = 'responses'),
    'true.v column is not in the present in your translated requests file.
         Please double check and make sure to load it with the load.requests function'
  )


})

testthat::test_that('recode.others.elsewhere works', {

  raw.main.filename <- testthat::test_path("fixtures", "test_recode_elsewhere_main.xlsx")
  tool.filename <- testthat::test_path("fixtures", "tool_elsewhere.xlsx")
  res.filename <- testthat::test_path("fixtures", "res_elsewhere.xlsx")
  requests.filename <- testthat::test_path("fixtures")
  label_colname <- "label::English"
  tool.survey <- utilityR::load.tool.survey(tool.filename, label_colname)
  tool.shoices <- utilityR::load.tool.choices(tool.filename ,label_colname)
  raw.main <- as.data.frame(readxl::read_excel(raw.main.filename))
  or.edited <- utilityR::load.requests(requests.filename, "requests_elsewhere", "Sheet1", validate = T)
  expected_res <- as.data.frame(readxl::read_excel(res.filename))

  # firstly let check edge conditions
  # test not existing YES in the invalid.v
  or.edited$invalid.v <- NA
  or.edited.sm <- or.edited %>% dplyr::filter(ref.type == "select_multiple")
  or.edited.so <- or.edited %>% dplyr::filter(ref.type == "select_one")

  testthat::expect_error(recode.others.elsewhere(raw.main, tool.survey, or.edited.sm, is.loop = F))
  testthat::expect_error(recode.others.elsewhere(raw.main, tool.survey, or.edited.so, is.loop = F))

  # test error when passed or.edited without uuid column
  or.edited <- or.edited %>% dplyr::select(-uuid)
  testthat::expect_error(recode.others.elsewhere(raw.main, tool.survey, or.edited, is.loop = F))

  # test error when passed raw.main without uuid column
  or.edited <- utilityR::load.requests(requests.filename, "requests_elsewhere", "Sheet1")
  raw.main <- raw.main %>% dplyr::select(-uuid)
  testthat::expect_error(recode.others.elsewhere(raw.main, tool.survey, or.edited, is.loop = F))

  raw.main <- readxl::read_excel(raw.main.filename)

  # test if pass the data with is.loop = T without loop_index column
  raw.main <- as.data.frame(readxl::read_excel(raw.main.filename))
  or.edited <- utilityR::load.requests(requests.filename, "requests_elsewhere", "Sheet1")

  testthat::expect_error(recode.others.elsewhere(raw.main, tool.survey, or.edited, is.loop = T))

  # test correctness of the output

  actual_result <- recode.others.elsewhere(raw.main, tool.survey, or.edited, is.loop = F)
  expected_res$loop_index <- as.character(expected_res$loop_index)
  testthat::expect_equal(actual_result, expected_res)

  # test correctness of the output with is.loop = T
  raw.main$loop_index <- as.character(raw.main$uuid)
  or.edited$loop_index <- as.character(or.edited$uuid)
  actual_result <- recode.others.elsewhere(raw.main, tool.survey, or.edited, is.loop = F)
  expected_res$loop_index <- as.character(expected_res$uuid)
  testthat::expect_equal(actual_result, expected_res)
})



testthat::test_that('recode.other.relevancies',{

  # load the tool data
  filename <- testthat::test_path("fixtures","tool_recode.xlsx")
  label_colname <- "label::English"
  tool.survey <- utilityR::load.tool.survey(filename,label_colname)

  # get the dataframe
  filename <- testthat::test_path("fixtures","data_main_recode.xlsx")
  test_data <- readxl::read_excel(filename)

  # get the cl_log

  filename <- testthat::test_path("fixtures","cl_log_recode_example.xlsx")
  cl_log <- readxl::read_excel(filename)


  var_list <- c('d2_winterization_support')

  relevancy_dictionary <- find.relevances(tool.survey = tool.survey,var_list = var_list)

  #test 1 basic functionality

  actual_output <- recode.other.relevances(data = test_data,
                                           cleaning.log.other = cl_log,
                                           relevancy_dictionary = relevancy_dictionary,
                                           is.loop = F) %>%
    dplyr::arrange(uuid,variable,old.value)


  expected_output <- data.frame(uuid = c('4a0fe558-902c-4c19-aa07-c03cac26e30a','559110ec-fec7-44a8-a132-c14ac4a8542a',
                                         '61279cb7-1c09-420c-9b82-bc2872ecb44e','61991004-5a0d-46b7-9084-19ca28379fe0',
                                         'cd44ecdc-2140-401e-943f-eaa9328e6a75','d5bd44dd-2979-4e0f-a3e8-bd707fcd8671',
                                         '7551c0f2-f22c-4e74-ab6d-104864fa6d66','bc07518c-dc73-4b2b-8117-4a6255e5ee90',
                                         'd26cb056-0788-4234-a920-8c57f0f3d7a8','fbd82700-21ec-4244-9a3e-041acfaaacdb', # invalid
                                         '61279cb7-1c09-420c-9b82-bc2872ecb44e','61991004-5a0d-46b7-9084-19ca28379fe0',
                                         'cd44ecdc-2140-401e-943f-eaa9328e6a75','d5bd44dd-2979-4e0f-a3e8-bd707fcd8671' # existing recode
  ),
  uniqui = c('4a0fe558-902c-4c19-aa07-c03cac26e30a','559110ec-fec7-44a8-a132-c14ac4a8542a',
             '61279cb7-1c09-420c-9b82-bc2872ecb44e','61991004-5a0d-46b7-9084-19ca28379fe0',
             'cd44ecdc-2140-401e-943f-eaa9328e6a75','d5bd44dd-2979-4e0f-a3e8-bd707fcd8671',
             '7551c0f2-f22c-4e74-ab6d-104864fa6d66','bc07518c-dc73-4b2b-8117-4a6255e5ee90',
             'd26cb056-0788-4234-a920-8c57f0f3d7a8','fbd82700-21ec-4244-9a3e-041acfaaacdb', # invalid
             '61279cb7-1c09-420c-9b82-bc2872ecb44e','61991004-5a0d-46b7-9084-19ca28379fe0',
             'cd44ecdc-2140-401e-943f-eaa9328e6a75','d5bd44dd-2979-4e0f-a3e8-bd707fcd8671' # existing recode
  ),
  loop_index = NA,
  variable = c(rep('d2_2_8_sufficient_winterization_support_oth',10),
               'd2_2_4_sufficient_winterization_support_fuel_generator',
               'd2_2_5_sufficient_winterization_support_alternative_heating_source',
               'd2_2_5_sufficient_winterization_support_alternative_heating_source',
               'd2_2_4_sufficient_winterization_support_fuel_generator'),
  issue = c(rep('Change in the relevancy. Invalid entry',10),
            rep('Change in the relevancy. Recoding the entry',4)),
  old.value = c('partially','no','partially',
                'partially','yes','yes','partially','yes',
                'partially','partially', rep(NA,4)),
  new.value = c(rep(NA,10),'partially','partially','yes','yes')
  ) %>%
    dplyr::tibble() %>%
    dplyr::arrange(uuid,variable,old.value)

  testthat::expect_equal(actual_output,expected_output)

  # test 2 - runs well if we have no invalid entries

  cl_log_recode <- cl_log[grepl('d2_winterization_support/',cl_log$variable) & cl_log$new.value%==%1,]

  actual_output <- recode.other.relevances(data = test_data,
                                           cleaning.log.other = cl_log_recode,
                                           relevancy_dictionary = relevancy_dictionary,
                                           is.loop = F) %>%
    dplyr::arrange(uuid,variable,old.value)


  expected_output <- data.frame(uuid = c('61279cb7-1c09-420c-9b82-bc2872ecb44e','61991004-5a0d-46b7-9084-19ca28379fe0',
                                         'cd44ecdc-2140-401e-943f-eaa9328e6a75','d5bd44dd-2979-4e0f-a3e8-bd707fcd8671' # existing recode
  ),
  uniqui = c('61279cb7-1c09-420c-9b82-bc2872ecb44e','61991004-5a0d-46b7-9084-19ca28379fe0',
             'cd44ecdc-2140-401e-943f-eaa9328e6a75','d5bd44dd-2979-4e0f-a3e8-bd707fcd8671' # existing recode
  ),
  loop_index = NA,
  variable = c('d2_2_4_sufficient_winterization_support_fuel_generator',
               'd2_2_5_sufficient_winterization_support_alternative_heating_source',
               'd2_2_5_sufficient_winterization_support_alternative_heating_source',
               'd2_2_4_sufficient_winterization_support_fuel_generator'),
  issue = c(rep('Change in the relevancy. Recoding the entry',4)),
  old.value = c(rep(NA_character_,4)),
  new.value = c('partially','partially','yes','yes')
  ) %>%
    dplyr::tibble() %>%
    dplyr::arrange(uuid,variable,old.value)

  testthat::expect_equal(actual_output,expected_output)


  # test 3 - runs well if we have ONLY invalid entries

  cl_log_invalid <- cl_log[grepl('d2_winterization_support/',cl_log$variable) & !cl_log$new.value%in%1,]

  actual_output <- recode.other.relevances(data = test_data,
                                           cleaning.log.other = cl_log_invalid,
                                           relevancy_dictionary = relevancy_dictionary,
                                           is.loop = F) %>%
    dplyr::arrange(uuid,variable,old.value)


  expected_output <- data.frame(uuid = c('4a0fe558-902c-4c19-aa07-c03cac26e30a','559110ec-fec7-44a8-a132-c14ac4a8542a',
                                         '61279cb7-1c09-420c-9b82-bc2872ecb44e','61991004-5a0d-46b7-9084-19ca28379fe0',
                                         'cd44ecdc-2140-401e-943f-eaa9328e6a75','d5bd44dd-2979-4e0f-a3e8-bd707fcd8671',
                                         '7551c0f2-f22c-4e74-ab6d-104864fa6d66','bc07518c-dc73-4b2b-8117-4a6255e5ee90',
                                         'd26cb056-0788-4234-a920-8c57f0f3d7a8','fbd82700-21ec-4244-9a3e-041acfaaacdb' # invalid
  ),
  uniqui = c('4a0fe558-902c-4c19-aa07-c03cac26e30a','559110ec-fec7-44a8-a132-c14ac4a8542a',
             '61279cb7-1c09-420c-9b82-bc2872ecb44e','61991004-5a0d-46b7-9084-19ca28379fe0',
             'cd44ecdc-2140-401e-943f-eaa9328e6a75','d5bd44dd-2979-4e0f-a3e8-bd707fcd8671',
             '7551c0f2-f22c-4e74-ab6d-104864fa6d66','bc07518c-dc73-4b2b-8117-4a6255e5ee90',
             'd26cb056-0788-4234-a920-8c57f0f3d7a8','fbd82700-21ec-4244-9a3e-041acfaaacdb' # invalid
  ),
  loop_index = NA,
  variable = c(rep('d2_2_8_sufficient_winterization_support_oth',10)),
  new.value = c(rep(NA_character_,10)),
  issue = c(rep('Change in the relevancy. Invalid entry',10)),
  old.value = c('partially','no','partially',
                'partially','yes','yes','partially','yes',
                'partially','partially')
  ) %>%
    dplyr::tibble() %>%
    dplyr::arrange(uuid,variable,old.value)

  testthat::expect_equal(actual_output,expected_output)

  # test 4 - should break if we try to run it on non-loop data
  testthat::expect_error(
    recode.other.relevances(data = test_data,
                            cleaning.log.other = cl_log,
                            relevancy_dictionary = relevancy_dictionary,
                            is.loop = T),
    "Parameter is.loop = TRUE, but column loop_index was not found in data!"
  )

  # test 5 - missing vars

  relevancy_dictionary <- rbind(relevancy_dictionary,data.frame(name='test',relevant='test'))

  testthat::expect_warning(
    recode.other.relevances(data = test_data,
                            cleaning.log.other = cl_log,
                            relevancy_dictionary = relevancy_dictionary,
                            is.loop = F),
    "Some of the variables in your relevancy_dictionary are not present in the data:test"
  )


  # test 6. Loops.

  test_data$loop_index <- paste0('loop',test_data$uuid)
  cl_log$loop_index <- paste0('loop',cl_log$uuid)

  var_list <- 'e3_wash_support'
  relevancy_dictionary <- find.relevances(tool.survey = tool.survey,var_list = var_list)

  actual_output <- recode.other.relevances(data = test_data,
                                           cleaning.log.other = cl_log,
                                           relevancy_dictionary = relevancy_dictionary,
                                           is.loop = T) %>%
    dplyr::arrange(uuid,variable,old.value)

  expected_output <- data.frame(uuid = c('6d0dbe00-9e20-4f48-9c90-50af8f443f63','1c97b817-5be6-420c-ad09-0e0835d87904',
                                         '40b0277f-dd88-4e95-9602-f66ba2a6d65c','bc07518c-dc73-4b2b-8117-4a6255e5ee90',
                                         'fbd82700-21ec-4244-9a3e-041acfaaacdb','74771da6-0321-4da0-b40b-d159bf1f5da6', #invalid
                                         '1c97b817-5be6-420c-ad09-0e0835d87904','40b0277f-dd88-4e95-9602-f66ba2a6d65c' #recode
                                         ),
  uniqui = c('loop6d0dbe00-9e20-4f48-9c90-50af8f443f63','loop1c97b817-5be6-420c-ad09-0e0835d87904',
             'loop40b0277f-dd88-4e95-9602-f66ba2a6d65c','loopbc07518c-dc73-4b2b-8117-4a6255e5ee90',
             'loopfbd82700-21ec-4244-9a3e-041acfaaacdb','loop74771da6-0321-4da0-b40b-d159bf1f5da6', #invalid
             'loop1c97b817-5be6-420c-ad09-0e0835d87904','loop40b0277f-dd88-4e95-9602-f66ba2a6d65c' #recode
  ),
  loop_index = c('loop6d0dbe00-9e20-4f48-9c90-50af8f443f63','loop1c97b817-5be6-420c-ad09-0e0835d87904',
                 'loop40b0277f-dd88-4e95-9602-f66ba2a6d65c','loopbc07518c-dc73-4b2b-8117-4a6255e5ee90',
                 'loopfbd82700-21ec-4244-9a3e-041acfaaacdb','loop74771da6-0321-4da0-b40b-d159bf1f5da6', #invalid
                 'loop1c97b817-5be6-420c-ad09-0e0835d87904','loop40b0277f-dd88-4e95-9602-f66ba2a6d65c' #recode
  ),
  variable = c(rep('e3_1_14_sufficient_wash_support_oth',6),
  rep('e3_1_1_sufficient_wash_support_repairs_water_supply_infrastructure_drainage_system',2)),
  issue = c(rep('Change in the relevancy. Invalid entry',6),rep('Change in the relevancy. Recoding the entry',2)),
  old.value = c('yes','yes',"partially",'yes','yes',"partially",NA,NA),
  new.value = c(rep(NA_character_,6),"yes","partially")
  ) %>%
    dplyr::tibble() %>%
    dplyr::arrange(uuid,variable,old.value)

  testthat::expect_equal(actual_output,expected_output)

})











