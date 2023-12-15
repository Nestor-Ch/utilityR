testthat::test_that("detect.outliers works", {


  df_all_na <- data.frame(
    uuid = c("1", "2", "3", "4", "5"),
    number_hh_member_idp = rep(NA, 5),
    responcent_age_non_idp = rep(NA, 5)
  )

  df_non_numeric <- data.frame(
    uuid = c("1", "2", "3", "4", "5"),
    number_hh_member_idp = c("1", "2", "3", "4", "non-numeric"),
    responcent_age_non_idp = rep("1", 5)
  )

  colnames_ <- c("non-existing_column")
  testthat::expect_error(detect.outliers(df=df_all_na, id="uuid", n.sd=2,
                                         method="o1", is.loop=F, colnames=colnames_))

  colnames_ <- c("number_hh_member_idp", "responcent_age_non_idp")

  df_empty <- data.frame(
    uuid = as.character(),
    number_hh_member_idp = as.character(),
    responcent_age_non_idp = as.character()
  )
  testthat::expect_equal(nrow(detect.outliers(df=df_empty, id="uuid", n.sd=2, is.loop=F, colnames=colnames_)), 0)

  df_zeroes <- data.frame(
    uuid = as.character(1:100),
    number_hh_member_idp = append(rep(0, 99), 1000000),
    responcent_age_non_idp = rep(NA, 100)
  )
  testthat::expect_equal(nrow(detect.outliers(df=df_zeroes, id="uuid", n.sd=2,
                                              method="o1", is.loop=F, colnames=colnames_, ignore_0 = T)), 0)
  testthat::expect_equal(nrow(detect.outliers(df=df_zeroes, id="uuid", n.sd=2,
                                              method="o1", is.loop=F, colnames=colnames_, ignore_0 = F)), 1)

  actual_output <- detect.outliers(df=df_all_na, id="uuid", n.sd=2,
                                   method="o1", is.loop=F, colnames=colnames_)

  testthat::expect_equal(nrow(actual_output), 0)
  testthat::expect_error(detect.outliers(df=df_non_numeric, id="uuid", n.sd=2,
                                         method="o1", is.loop=F, colnames=colnames_))
  testthat::expect_error(detect.outliers(df=df_all_na, id="uuid", n.sd=2,
                                         method="o1", is.loop=T, colnames=colnames_))
  testthat::expect_error(detect.outliers(df=df_all_na, id="non-existing_id", n.sd=2,
                                         method="o1", is.loop=F, colnames=colnames_))
  df_without_uuid <- data.frame(
    id = c("1", "2", "3", "4", "5"),
    number_hh_member_idp = rep(NA, 5),
    responcent_age_non_idp = rep(NA, 5)
  )

  testthat::expect_error(detect.outliers(df=df_without_uuid, id="uuid", n.sd=2,
                                         method="o1", is.loop=F, colnames=colnames_))
  loop_df <- data.frame(
    uuid = c("1", "2", "3", "4", "5"),
    loop_index = rep("loop1", 5),
    number_hh_member_idp = rep(NA, 5),
    responcent_age_non_idp = rep(NA, 5)
  )
  testthat::expect_error(detect.outliers(df=loop_df, id="uuid", n.sd=2,
                                         method="o1", is.loop=F, colnames=colnames_))
  set.seed(123)
  # test negative values
  normal_df <- data.frame(
    uuid = as.character(1:100),
    number_hh_member_idp = rep(NA, 100),
    responcent_age_non_idp = round(rnorm(100, mean = 50, sd = 25))
  )
  normal_df <- normal_df %>% dplyr::mutate(responcent_age_non_idp = ifelse(uuid == "100", 150, responcent_age_non_idp))
  true_output = data.frame(
    uuid = c("100"),
    loop_index = rep(NA, 1),
    issue = rep("Outlier", 1),
    variable = rep("responcent_age_non_idp", 1),
    old.value = c(150),
    new.value = rep(NA, 1)
  )
  o1_output <- data.frame(
    uuid = c("18", "100"),
    loop_index = rep(NA, 2),
    issue = rep("Outlier", 2),
    variable = rep("responcent_age_non_idp", 2),
    old.value = c(1, 150),
    new.value = rep(NA, 2)
  )
  testthat::expect_equal(detect.outliers(df=normal_df, id="uuid", n.sd=3,
                                         method="o2", is.loop=F, colnames=colnames_), true_output)
  testthat::expect_equal(detect.outliers(df=normal_df, id="uuid", n.sd=3,
                                         method="o1", is.loop=F, colnames=colnames_), o1_output)
  testthat::expect_equal(detect.outliers(df=normal_df, id="uuid", n.sd=2,
                                         method="o3", is.loop=F, colnames=colnames_), true_output)
  testthat::expect_equal(detect.outliers(df=normal_df, id="uuid", n.sd=3,
                                         method="o4", is.loop=F, colnames=colnames_), true_output)

  set.seed(123)
  negative_df <- data.frame(
    uuid = as.character(1:100),
    number_hh_member_idp = rep(NA, 100),
    responcent_age_non_idp = round(rnorm(100, mean = -1000, sd = 25))
  )
  negative_df <- negative_df %>% dplyr::mutate(responcent_age_non_idp = ifelse(uuid == "100", 150, responcent_age_non_idp))
  testthat::expect_equal(nrow(detect.outliers(df=negative_df, id="uuid", n.sd=2,
                                              method="o1", is.loop=F, colnames=colnames_)), 0)
  testthat::expect_equal(nrow(detect.outliers(df=negative_df, id="uuid", n.sd=2,
                                              method="o2", is.loop=F, colnames=colnames_)), 0)
  testthat::expect_equal(nrow(detect.outliers(df=negative_df, id="uuid", n.sd=2,
                                              method="o3", is.loop=F, colnames=colnames_)), 0)
  testthat::expect_equal(nrow(detect.outliers(df=negative_df, id="uuid", n.sd=2,
                                              method="o4", is.loop=F, colnames=colnames_)), 0)
})

testthat::test_that("generate.boxplot works", {

  raw_data_frame1 <- data.frame(
    uuid = as.character(1:100),
    loop_index = rep("some_unique_value", 100),
    test_variable = round(rnorm(100, 50, 25))
  )
  values <- sample(raw_data_frame1$uuid, 5)
  outliers_data_frame1 <- data.frame(
    uuid = subset(raw_data_frame1, uuid %in% values)$uuid,
    loop_index = rep(NA, 5),
    variable = "test_variable",
    old.value = subset(raw_data_frame1, uuid %in% values)$test_variable,
    issue = rep("Outlier", 5)
  )

  testthat::expect_error(generate.boxplot(
    outliers.list = list(outliers_data_frame1),
    raw.data_frames.list = list(raw_data_frame1),
    columns.list = list("test_variable"),
    n.sd = 2
  ))

  raw_data_frame1 <- data.frame(
    uuid = as.character(1:100),
    test_variable = round(rnorm(100, 50, 25))
  )
  values <- sample(raw_data_frame1$uuid, 5)
  outliers_data_frame1 <- data.frame(
    uuid = subset(raw_data_frame1, uuid %in% values)$uuid,
    loop_index = rep("some_loop_value", 5),
    variable = "test_variable",
    old.value = subset(raw_data_frame1, uuid %in% values)$test_variable,
    issue = rep("Outlier", 5)
  )

  testthat::expect_error(generate.boxplot(
    outliers.list = list(outliers_data_frame1),
    raw.data_frames.list = list(raw_data_frame1),
    columns.list = list("test_variable"),
    n.sd = 2
  ))

  raw_data_frame1 <- data.frame(
    uuid = as.character(1:100),
    test_variable = round(rnorm(100, 50, 25))
  )
  values <- sample(raw_data_frame1$uuid, 5)
  outliers_data_frame1 <- data.frame(
    uuid = subset(raw_data_frame1, uuid %in% values)$uuid,
    loop_index = rep(NA, 5),
    variable = "test_variable",
    old.value = subset(raw_data_frame1, uuid %in% values)$test_variable,
    issue = rep("Outlier", 5)
  )
  # testthat::expect_no_error(generate.boxplot(raw.data_frames.list=list(raw_data_frame1),
  #                           outliers.list=list(outliers_data_frame1),
  #                           columns.list=list("test_variable"),
  #                           n.sd=2, boxplot.path=""))
  # if (!file.exists("2sd.pdf")) {
  #   testthat::fail("Boxplot wasn't created")
  # } else {
  #   file.remove("2sd.pdf")
  # }

  testthat::expect_error(generate.boxplot(raw.data_frames.list=list(raw_data_frame1),
                                          outliers.list=list(),
                                          columns.list=list("test_variable"),
                                          n.sd=2, boxplot.path=""))
})
