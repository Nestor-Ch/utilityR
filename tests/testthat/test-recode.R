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

  actual_output <- recode.set.value.regex(test_data, text_test_2, pattern = 'dont_know',
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


