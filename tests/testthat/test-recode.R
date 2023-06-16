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



testthat::test_that("recode.multiple.set.NA works", {
# upload the data

  test_path <- testthat::test_path("fixtures","utilityR_raw_data.xlsx")
  test_data <- readxl::read_excel(test_path)[41:50,] %>%
    dplyr::rename(uuid = `_uuid`)
  # set up loop index to test if it works
  test_data$loop_index = 1:10

  # Test 1 - test if it throws the tool.survey error

  testthat::expect_error(testthat::expect_message(recode.multiple.set.choices(test_data, 'b7_1_heating_fuel','gas_heating',issue='test_issue')),
                         'tool.survey is not present in the environment, please upload it with load.tool.survey function')
# load the tool data


filename <- testthat::test_path("fixtures","tool.survey_full.xlsx")
label_colname <- "label::English"
tool.survey <<- utilityR::load.tool.survey(filename,label_colname)

# Test 2 - test if it throws the tool.choices error

testthat::expect_error(testthat::expect_message(recode.multiple.set.choices(test_data, 'b7_1_heating_fuel','gas_heating',issue='test_issue')),
                       'tool.choices is not present in the environment, please upload it with load.tool.choices function')


# Test 3 - test if it works as expected with 1 variable


# rename the tool.choices db
tool.choices <<- utilityR::utilityR_choices


expected_output <- data.frame(uuid = c(test_data[!test_data$b7_1_heating_fuel=='gas_heating',]$uuid,test_data[test_data$`b7_1_heating_fuel/gas_heating`==0,]$uuid,
                                       test_data[test_data$`b7_1_heating_fuel/electricity_heating`==1,]$uuid),
                              loop_index=c(test_data[!test_data$b7_1_heating_fuel=='gas_heating',]$loop_index,test_data[test_data$`b7_1_heating_fuel/gas_heating`==0,]$loop_index,
                                           test_data[test_data$`b7_1_heating_fuel/electricity_heating`==1,]$loop_index),
                              old.value = c(test_data[!test_data$b7_1_heating_fuel=='gas_heating',]$b7_1_heating_fuel,0,1,1),
                              variable = c(rep('b7_1_heating_fuel',2),rep('b7_1_heating_fuel/gas_heating',1),rep('b7_1_heating_fuel/electricity_heating',2) ),
                              new.value = c(rep('gas_heating',2),1,0,0),
                              issue = rep('test_issue',5)
                                ) %>% dplyr::tibble()
actual_output <- recode.multiple.set.choices(test_data, 'b7_1_heating_fuel','gas_heating',issue='test_issue')


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

actual_output2 <- recode.multiple.set.choices(test_data, 'b17_access_stores',c('no_impact','road_damage'),issue='test_issue')

testthat::expect_equal(expected_output2, actual_output2)
})

rm(tool.choices,tool.survey)


testthat::test_that("recode.multiple.set.NA works", {

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

})



testthat::test_that("recode.multiple.set.NA works", {


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


expected_output2 <- data.frame(uuid = c(test_data[grepl('air_alert',test_data$b17_access_stores),]$uuid,
                                       test_data[test_data$`b17_access_stores/air_alert` ==1,]$uuid,
                                       test_data[test_data$`b17_access_stores/other` ==1,]$uuid,
                                       test_data[test_data$`b17_access_stores/other` ==1,]$uuid),
                              loop_index = c(test_data[grepl('air_alert',test_data$b17_access_stores),]$loop_index,
                                             test_data[test_data$`b17_access_stores/air_alert` ==1,]$loop_index,
                                             test_data[test_data$`b17_access_stores/other` ==1,]$loop_index,
                                             test_data[test_data$`b17_access_stores/other` ==1,]$loop_index),
                              old.value = c(test_data[grepl('air_alert',test_data$b17_access_stores),]$b17_access_stores,
                                            rep(1,4), 1,'test'),
                              variable = c(rep('b17_access_stores',4), rep('b17_access_stores/air_alert',4), rep('b17_access_stores/other',1), 'b17_1_access_stores_other'),
                              new.value = c(rep("power_outages",4),rep(0,4), 0,NA ),
                              issue = rep('test_issue',10)
) %>% dplyr::tibble()


actual_output2 <- recode.multiple.remove.choices(test_data, "b17_access_stores", c("air_alert","other"),issue='test_issue' )

testthat::expect_equal(expected_output2,actual_output2)


# test 3 - test if it breaks like its supposed to
testthat::expect_error(recode.multiple.remove.choices(test_data, "fake_column", c("air_alert","other"),issue='test_issue' ))

# test 4 - test if it breaks like its supposed to
testthat::expect_error(recode.multiple.remove.choices(test_data, "b17_access_stores", c("fake_choice","other"),issue='test_issue' ))

})







