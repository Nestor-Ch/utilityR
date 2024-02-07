testthat::test_that('save.other.requests works - test 1 - no template',{


  test_frame <- data.frame(
    uuid = c('test1','test2'),
    loop_index = c('12345','12345'),
    name = c('variable1','variable2'),
    ref.name = c('main.variable.1', 'main.variable.2'),
    full.label = c('label1','label2'),
    ref.type = c('select_one','select_one'),
    choices.label=c('choice1, choice2', 'choice1a, choice2a'),
    response.uk=c('test1','test2'),
    response.en = c('test1','test2'),
    true = c(NA,NA),
    existing = c(NA,NA),
    invalid = c('test','test')
  )

  temp_dir <- tempdir()

  save.other.requests(
    df = test_frame,
    directory = temp_dir,
    wb_name = 'testo'
  )

  testthat::expect_true(file.exists(paste0(temp_dir, "testo.xlsx")))
  unlink(paste0(temp_dir, "testo.xlsx"))

})

testthat::test_that('save.other.requests works - test 2 - using the template',{


  test_frame <- data.frame(
    uuid = c('test1','test2'),
    loop_index = c('12345','12345'),
    name = c('variable1','variable2'),
    ref.name = c('main.variable.1', 'main.variable.2'),
    full.label = c('label1','label2'),
    ref.type = c('select_one','select_one'),
    choices.label=c('choice1, choice2', 'choice1a, choice2a'),
    response.uk=c('test1','test2'),
    response.en = c('test1','test2'),
    true = c(NA,NA),
    existing = c(NA,NA),
    invalid = c('test','test')
  )

  temp_dir <- tempdir()


  save.other.requests(
    df = test_frame,
    directory = temp_dir,
    wb_name = 'testo2',
    use_template = T,
    template_dir = testthat::test_path("fixtures","tool.xlsx")
  )

  testthat::expect_true(file.exists(paste0(temp_dir, "testo2.xlsx")))


  unlink(paste0(temp_dir, "testo2.xlsx"))

})


testthat::test_that('save.other.requests works -test 1 no template',{

  test_frame <- data.frame(
    uuid = c('test1','test2'),
    loop_index = c('12345','12345'),
    name = c('variable1','variable2'),
    ref.name = c('main.variable.1', 'main.variable.2'),
    full.label = c('label1','label2'),
    ref.type = c('select_one','select_one'),
    choices.label=c('choice1, choice2', 'choice1a, choice2a'),
    response.uk=c('test1','test2'),
    response.en = c('test1','test2'),
    true = c(NA,NA),
    existing = c(NA,NA),
    invalid = c('test','test')
  )

  temp_dir <- tempdir()

  save.trans.requests(
    df = test_frame,
    directory = temp_dir,
    wb_name = 'testo'
  )

  testthat::expect_true(file.exists(paste0(temp_dir, "testo.xlsx")))

  unlink(paste0(temp_dir, "testo.xlsx"))
})

testthat::test_that('save.other.requests works -test 2 - using the template',{

  test_frame <- data.frame(
    uuid = c('test1','test2'),
    loop_index = c('12345','12345'),
    name = c('variable1','variable2'),
    ref.name = c('main.variable.1', 'main.variable.2'),
    full.label = c('label1','label2'),
    ref.type = c('select_one','select_one'),
    choices.label=c('choice1, choice2', 'choice1a, choice2a'),
    response.uk=c('test1','test2'),
    response.en = c('test1','test2'),
    true = c(NA,NA),
    existing = c(NA,NA),
    invalid = c('test','test')
  )

  temp_dir <- tempdir()

  save.trans.requests(
    df = test_frame,
    directory = temp_dir,
    wb_name = 'testo2',
    use_template = T,
    template_dir = testthat::test_path("fixtures","tool.xlsx")
  )

  testthat::expect_true(file.exists(paste0(temp_dir, "testo2.xlsx")))

  unlink(paste0(temp_dir, "testo2.xlsx"))
})

testthat::test_that('save.other.requests works -test 3 -Should break if I provide non-existent blue columns',{

  test_frame <- data.frame(
    uuid = c('test1','test2'),
    loop_index = c('12345','12345'),
    name = c('variable1','variable2'),
    ref.name = c('main.variable.1', 'main.variable.2'),
    full.label = c('label1','label2'),
    ref.type = c('select_one','select_one'),
    choices.label=c('choice1, choice2', 'choice1a, choice2a'),
    response.uk=c('test1','test2'),
    response.en = c('test1','test2'),
    true = c(NA,NA),
    existing = c(NA,NA),
    invalid = c('test','test')
  )

  temp_dir <- tempdir()


  testthat::expect_error(
    save.trans.requests(
      df = test_frame,
      directory = temp_dir,
      wb_name = 'testo2',
      blue_cols = 'Fake'
    ),'Fake not found in df!'
  )
})

testthat::test_that('save.other.requests works -test 4. Shouldnt break if I want to mame an existing column blue',{

  test_frame <- data.frame(
    uuid = c('test1','test2'),
    loop_index = c('12345','12345'),
    name = c('variable1','variable2'),
    ref.name = c('main.variable.1', 'main.variable.2'),
    full.label = c('label1','label2'),
    ref.type = c('select_one','select_one'),
    choices.label=c('choice1, choice2', 'choice1a, choice2a'),
    response.uk=c('test1','test2'),
    response.en = c('test1','test2'),
    true = c(NA,NA),
    existing = c(NA,NA),
    invalid = c('test','test')
  )

  temp_dir <- tempdir()


    save.trans.requests(
      df = test_frame,
      directory = temp_dir,
      wb_name = 'testo3',
      blue_cols = 'full.label'
    )

    testthat::expect_true(file.exists(paste0(temp_dir, "testo3.xlsx")))


  unlink(paste0(temp_dir, "testo3.xlsx"))


})


testthat::test_that('save.follow.up.requests works',{

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

  save.follow.up.requests(
    checks.df = test_frame,
    directory = temp_dir,
    wb_name = 'testo'
  )

  testthat::expect_true(file.exists(paste0(temp_dir, "testo.xlsx")))

  unlink(paste0(temp_dir, "testo.xlsx"))

})




