testthat::test_that("find.responses works", {
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
                                loop_index =c(NA,NA),
                                name = c("occupation","occupation"),
                                responses=c("cook","train conductor"),
                                ref.name=c("age", "age"),
                                choice=c("21", "32"))
  testthat::expect_equal(actual_output,
                         expected_output)
  #test 4
  q.db <- data.frame(name = c("occupation"), ref.name=c("age"))
  testdata <- data.frame(age = c(21,32), occupation = c("cook","train conductor"),
                         uuid = c("abc","def"), loop_index = c("loop_123","loop_456"))
  actual_output <- find.responses(testdata,q.db,"responses",is.loop = T)
  expected_output <- data.frame(uuid = c("abc","def"),
                                loop_index =c("loop_123","loop_456"),
                                name = c("occupation","occupation"),
                                responses=c("cook","train conductor"),
                                ref.name=c("age", "age"),
                                choice=c("21", "32"))
  testthat::expect_equal(actual_output,
                         expected_output)

})



testthat::test_that("translate.responses works", {

  # test  0
  test_data <-  data.frame(response = c("Тест","Українською","Мовою","Майже","Як","ЗНО"))

  testthat::expect_error(
    translate.responses(test_data, values_from = 'response',directory = tmp_dir)
  )


  api_path <- Sys.getenv("API_KEY")

  #test 1 - normal run


  tmp_dir <- tempdir()

  actual_result <- translate.responses(test_data, values_from = 'response',directory = tmp_dir,
                                         api.key = api_path
  )

  expected_result <- data.frame(response = c("Тест","Українською","Мовою","Майже","Як","ЗНО"),
                                response.en = c('test','Ukrainian','Language','almost','like','ZNO')
                                )

  testthat::expect_true(file.exists(paste0(tmp_dir, "translate_info.csv")))


  # test 2 test that the file will be appended if we run it 2 times


  test_data <-  data.frame(response = c("Тест","два"))

  actual_result <- translate.responses(test_data, values_from = 'response',directory = tmp_dir,
                                       api.key = api_path
  )

  expected_result <- data.frame(response = c("Тест","два"),
                                response.en = c('test','two')
  )

  unlink(paste0(tmp_dir, "translate_info.csv"))

#  test 3 - empty df is fed
  tmp_dir <- tempdir()

  test_data <-  data.frame(response = NA_character_)

  testthat::expect_warning(
    translate.responses(test_data, values_from = 'response',directory = tmp_dir,
                        api.key = api_path),
    "Nothing to be translated"
  )

  testthat::expect_true(file.exists(paste0(tmp_dir, "translate_info.csv")))
  unlink(paste0(tmp_dir, "translate_info.csv"))



  #test X - super large vector, we'll cancel that one.
  tmp_dir <- tempdir()

  test_data <-  data.frame(response = OpenRepGrid::randomSentences(10000,nwords = 15))

  actual_result <- translate.responses(test_data, values_from = 'response',directory = tmp_dir,
                                       api.key = api_path
  )

  testthat::expect_equal(actual_result,test_data)

  testthat::expect_true(file.exists(paste0(tmp_dir, "translate_info.csv")))

  unlink(paste0(tmp_dir, "translate_info.csv"))
})




