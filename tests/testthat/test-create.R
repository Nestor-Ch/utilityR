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
