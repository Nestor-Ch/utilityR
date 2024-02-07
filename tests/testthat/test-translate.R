testthat::test_that("find.responses works - different mini tests on fake data.", {
  #test 1
  q.db <- data.frame()
  testdata <- data.frame(age = c(21,32), occupation = c("cook","train conductor"),uuid = c("abc","def"))
  actual_output <- find.responses(testdata,q.db,"responses",is.loop = F) %>%
    suppressWarnings()
  testthat::expect_equal(actual_output,
                         data.frame())

  #test 2
  q.db <- data.frame(name = c("age","occupation"))
  testdata <- data.frame()
  actual_output <- find.responses(testdata,q.db,"responses",is.loop = F) %>%
    suppressWarnings()
  testthat::expect_equal(actual_output,
                         data.frame())

  #test 3
  q.db <- data.frame(name = c("occupation"), ref.name=c("age"))
  testdata <- data.frame(age = c(21,32), occupation = c("cook","train conductor"), uuid = c("abc","def"))
  actual_output <- find.responses(testdata,q.db,"responses",is.loop = F)
  expected_output <- data.frame(uuid = c("abc","def"),
                                ref.name=c("age", "age"),
                                choice=c(21, 32),
                                loop_index =c(NA,NA),
                                name = c("occupation","occupation"),
                                responses=c("cook","train conductor"))
  actual_output <- as.data.frame(actual_output)
  testthat::expect_equal(actual_output,
                         expected_output)
  #test 4
  q.db <- data.frame(name = c("occupation"), ref.name=c("age"))
  testdata <- data.frame(age = c(21,32), occupation = c("cook","train conductor"),
                         uuid = c("abc","def"), loop_index = c("loop_123","loop_456"))
  actual_output <- find.responses(testdata,q.db,"responses",is.loop = T)
  expected_output <- data.frame(loop_index =c("loop_123","loop_456"),
                                ref.name=c("age", "age"),
                                choice=c(21, 32),
                                uuid = c("abc","def"),
                                name = c("occupation","occupation"),
                                responses=c("cook","train conductor"))
  actual_output <- as.data.frame(actual_output)
  testthat::expect_equal(actual_output,
                         expected_output)
  #test 5

  q.db <- data.frame(name = c("occupation"), ref.name=c("age"))
  testdata <- data.frame(age = c(21,32), occupation = c("cook","train conductor"),
                         uuid = c("abc","def"), loop_index = rep(NA, 2))
  actual_output <- find.responses(testdata,q.db,"responses",is.loop = F)
  expected_output <- data.frame(uuid = c("abc","def"),
                                ref.name=c("age", "age"),
                                choice=c(21, 32),
                                loop_index = rep(NA, 2),
                                name = c("occupation","occupation"),
                                responses=c("cook","train conductor"))
  actual_output <- as.data.frame(actual_output)
  testthat::expect_equal(actual_output,
                         expected_output)

  #test 6
  q.db <- data.frame(name = c("occupation"))
  testdata <- data.frame(age = c(21,32), occupation = c("cook","train conductor"),
                         uuid = c("abc","def"), loop_index = c("loop_123","loop_456"))
  actual_output <- find.responses(testdata,q.db,"responses",is.loop = T)
  expected_output <- data.frame(uuid = c("abc","def"),
                                loop_index =c("loop_123","loop_456"),
                                name = c("occupation","occupation"),
                                responses=c("cook","train conductor"),
                                choice=rep(NA, 2))
  actual_output <- as.data.frame(actual_output)
  testthat::expect_equal(actual_output,
                         expected_output)

})



testthat::test_that("translate.responses works test 0  - no api key provided", {

  test_data <-  data.frame(response = c("Тест","Українською","Мовою","Майже","Як","ЗНО"))

  testthat::expect_error(
    translate.responses(test_data, values_from = 'response',directory = tmp_dir),
    'Please input the api key. Usually we store ours in "resources/microsoft.api.key_regional.R", copy it from there and insert.'
  )

})

testthat::test_that("translate.responses works test 1  - normal run", {

  test_data <-  data.frame(response = c("Тест","Українською","Мовою","Майже","Як","ЗНО"))

  api_path <- Sys.getenv("API_KEY")


  tmp_dir <- tempdir()

  actual_result <- translate.responses(test_data, values_from = 'response',directory = tmp_dir,
                                         api.key = api_path
  )

  expected_result <- data.frame(response = c("Тест","Українською","Мовою","Майже","Як","ЗНО"),
                                response.en = c('test','Ukrainian','Language','almost','like','ZNO')
                                )

  testthat::expect_true(file.exists(paste0(tmp_dir, "translate_info.csv")))


  unlink(paste0(tmp_dir, "translate_info.csv"))
})


testthat::test_that("translate.responses works test  2 test that the file will be appended if we run it 2 times", {

  test_data <-  data.frame(response = c("Тест","Українською","Мовою","Майже","Як","ЗНО"))

  api_path <- Sys.getenv("API_KEY")

  tmp_dir <- tempdir()

  test_data <-  data.frame(response = c("Тест","два"))

  actual_result <- translate.responses(test_data, values_from = 'response',directory = tmp_dir,
                                       api.key = api_path
  )

  expected_result <- data.frame(response = c("Тест","два"),
                                response.en = c('test','two')
  )

  testthat::expect_equal(actual_result,expected_result)

  unlink(paste0(tmp_dir, "translate_info.csv"))
})

testthat::test_that("translate.responses works, test 3 - empty df is fed", {

  test_data <-  data.frame(response = c("Тест","Українською","Мовою","Майже","Як","ЗНО"))

  api_path <- Sys.getenv("API_KEY")


  tmp_dir <- tempdir()

  test_data <-  data.frame(response = NA_character_)

  testthat::expect_warning(
    translate.responses(test_data, values_from = 'response',directory = tmp_dir,
                        api.key = api_path),
    "Nothing to be translated"
  )

  testthat::expect_true(file.exists(paste0(tmp_dir, "translate_info.csv")))
  unlink(paste0(tmp_dir, "translate_info.csv"))
})


testthat::test_that("translate.responses works, test X - super large vector, we'll cancel that one.", {

  tmp_dir <- tempdir()
  api_path <- Sys.getenv("API_KEY")


  test_data <-  data.frame(response = OpenRepGrid::randomSentences(10000,nwords = 15))

  actual_result <- translate.responses(test_data, values_from = 'response',directory = tmp_dir,
                                       api.key = api_path
  )

  testthat::expect_equal(actual_result,test_data)

  testthat::expect_true(file.exists(paste0(tmp_dir, "translate_info.csv")))

  unlink(paste0(tmp_dir, "translate_info.csv"))
})




