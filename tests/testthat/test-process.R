testthat::test_that('process.uuid works, test 1 - basic functionality',{
  test_audit_path <- testthat::test_path('fixtures/audits_test')
  test_loaded  <- load.audit.files(dir.audits = test_audit_path, track.changes = T)


  actual_output <- test_loaded %>%
    dplyr::group_by(uuid) %>%
    dplyr::group_modify(~process.uuid(.x)) %>%
    dplyr::ungroup()


  round_cols <- c('tot.t','tot.rt','tot.rt.inter','t1','rt1','w1','t2','rt2')
  na_cols <- c('w1','t2','rt2','q2','j2','e2')
  questions <- c("question","group.questions")

  expected_output <- test_loaded %>%
    dplyr::group_by(uuid) %>%
    dplyr::mutate(time_end = ifelse(event == 'form.exit' & start == max(start), start,0),
                  time_start = ifelse(event == 'form.start' & start == min(start), start,0),
                  endings = ifelse(event == 'form.exit', start,NA),
                  wait_end = ifelse(event == 'form.resume', start,NA)
    ) %>%
    dplyr::summarise(
      n.iteration = sum(event =='form.exit'),
      tot.t = (max(time_end)-max(time_start))/1000/60,
      tot.rt = sum(ifelse(event %in% questions,duration,0))/60,
      tot.rt.inter = sum(ifelse(event %in% questions,inter_q_duration,0),na.rm = T)/60,
      t1 = (min(endings,na.rm = T)-max(time_start))/1000/60,
      rt1 =  sum(ifelse(event %in% questions & start < min(endings,na.rm = T),duration,0))/60,
      q1 = ifelse(event %in% questions & start < min(endings,na.rm = T) & start>time_start
                  , node, NA) %>% na.omit() %>% unique() %>% length(),
      j1 =  ifelse(event %in% "jump" & start < min(endings,na.rm = T) & start>time_start
                   , 1, 0) %>% sum(),
      e1 =  ifelse(!is.na(`old.value`) & `old.value`!="" & start < min(endings,na.rm = T) & start>time_start
                   , 1, 0) %>% sum(),
      w1 = ifelse(!all(is.na(wait_end)),(min(wait_end, na.rm = T)-min(endings, na.rm = T))/1000/60,0),
      t2 = ifelse(!all(is.na(wait_end)),(max(endings,na.rm = T)-min(wait_end,na.rm = T))/1000/60,0),
      rt2 =  sum(ifelse(event %in% questions & start > min(endings,na.rm = T),duration,0))/60,
      q2 = ifelse(event %in% questions & start > min(endings,na.rm = T) & start>time_start
                  , node, NA) %>% na.omit() %>% unique() %>% length(),
      j2 =  ifelse(event %in% "jump" & start > min(endings,na.rm = T) & start>time_start
                   , 1, 0) %>% sum(),
      e2 =  ifelse(!is.na(`old.value`) & `old.value`!="" & start > min(endings,na.rm = T) & start>time_start
                   , 1, 0) %>% sum()
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(round_cols),~ round(.x,1)),
                  dplyr::across(dplyr::all_of(na_cols),~ ifelse(.x==0,NA,.x)))

  testthat::expect_equal(actual_output, expected_output)
})

testthat::test_that('process.uuid works, test 2 - breaks if the first event is not form.start',{

  test_audit_path <- testthat::test_path('fixtures/audits_test')
  test_loaded  <- load.audit.files(dir.audits = test_audit_path, track.changes = T)

  # test 2 - breaks if the first event is not form.start

  test_loaded2 <- test_loaded
  test_loaded2[1,]$event = 'fake_event'

  testthat::expect_error(
    test_loaded2 %>%
      dplyr::group_by(uuid) %>%
      dplyr::group_modify(~process.uuid(.x)) %>%
      dplyr::ungroup()

  )
})

testthat::test_that(
  'process.uuid works, test 3 breaks if the first event is not form.start, if one form.exit
  is followed by another form.exit without form.resume - should give warning',{

    test_audit_path <- testthat::test_path('fixtures/audits_test')
    test_loaded  <- load.audit.files(dir.audits = test_audit_path, track.changes = T)


    test_loaded2 <- test_loaded %>% dplyr::select(-uuid2)
    test_loaded2[93,]$event = 'form.exit'
    test_loaded2[93,]$start = test_loaded2[93,]$start+10

    testthat::expect_warning(
      test_loaded2 %>%
        dplyr::group_by(uuid) %>%
        dplyr::group_modify(~process.uuid(.x)) %>%
        dplyr::ungroup(),"status=waiting while form.exit!"
    )
  })


testthat::test_that('process.uuid works, Test 4. use uuid2 in the load process to get better error messages on process.uuid',{

  test_audit_path <- testthat::test_path('fixtures/audits_test')

  test_loaded  <- load.audit.files(dir.audits = test_audit_path, track.changes = T,add.uuid2 = T)
  test_loaded2 <- test_loaded
  test_loaded2[93,]$event = 'form.exit'
  test_loaded2[93,]$start = test_loaded2[93,]$start+10

  testthat::expect_warning(
    test_loaded2 %>%
      dplyr::group_by(uuid) %>%
      dplyr::group_modify(~process.uuid(.x)) %>%
      dplyr::ungroup(),"status=waiting while form.exit! uuid: 00f695c4-9a42-4b8f-9fe1-5c9f3e13a2d2"
  )
})

testthat::test_that('process.uuid works, Test 5. throws error when one of the audits doesnt have a form.exit event',{

  test_audit_path <- testthat::test_path('fixtures/audits_test')

  test_loaded  <- load.audit.files(dir.audits = test_audit_path, track.changes = T,add.uuid2 = T)


  test_loaded2 <- test_loaded
  test_loaded2[92,]$event = 'fake'

  testthat::expect_error(
    test_loaded2 %>%
      dplyr::group_by(uuid) %>%
      dplyr::group_modify(~process.uuid(.x)) %>%
      dplyr::ungroup(), "Your form doesn't have the form.exit event. Please double-check: 00f695c4-9a42-4b8f-9fe1-5c9f3e13a2d2"
  )
})


testthat::test_that('process.uuid works, test 6 - pre-processed files',{

  test_audit_path <- testthat::test_path('fixtures/audits_test')

  round_cols <- c('tot.t','tot.rt','tot.rt.inter','t1','rt1','w1','t2','rt2')
  na_cols <- c('w1','t2','rt2','q2','j2','e2')
  questions <- c("question","group.questions")


  test_loaded  <- load.audit.files(dir.audits = test_audit_path, track.changes = T)
  test_loaded <- pre.process.audits(test_loaded,10)

  actual_output <- test_loaded %>%
    dplyr::group_by(uuid) %>%
    dplyr::group_modify(~process.uuid(.x)) %>%
    dplyr::ungroup()


  expected_output <- test_loaded %>%
    dplyr::group_by(uuid) %>%
    dplyr::mutate(time_end = ifelse(event == 'form.exit' & start == max(start), start,0),
                  time_start = ifelse(event == 'form.start' & start == min(start), start,0),
                  endings = ifelse(event == 'form.exit', start,NA),
                  wait_end = ifelse(event == 'form.resume', start,NA)
    ) %>%
    dplyr::summarise(
      n.iteration = sum(event =='form.exit'),
      tot.t = (max(time_end)-max(time_start))/1000/60,
      tot.rt = sum(ifelse(event %in% questions,duration,0))/60,
      tot.rt.inter = sum(ifelse(event %in% questions,inter_q_duration,0),na.rm = T)/60,
      t1 = (min(endings,na.rm = T)-max(time_start))/1000/60,
      rt1 =  sum(ifelse(event %in% questions & start < min(endings,na.rm = T),duration,0))/60,
      q1 = ifelse(event %in% questions & start < min(endings,na.rm = T) & start>time_start
                  , node, NA) %>% na.omit() %>% unique() %>% length(),
      j1 =  ifelse(event %in% "jump" & start < min(endings,na.rm = T) & start>time_start
                   , 1, 0) %>% sum(),
      e1 =  ifelse(!is.na(`old.value`) & `old.value`!="" & start < min(endings,na.rm = T) & start>time_start
                   , 1, 0) %>% sum(),
      tag = paste0(tag[tag!=''],collapse = ','),
      w1 = ifelse(!all(is.na(wait_end)),(min(wait_end, na.rm = T)-min(endings, na.rm = T))/1000/60,0),
      t2 = ifelse(!all(is.na(wait_end)),(max(endings,na.rm = T)-min(wait_end,na.rm = T))/1000/60,0),
      rt2 =  sum(ifelse(event %in% questions & start > min(endings,na.rm = T),duration,0))/60,
      q2 = ifelse(event %in% questions & start > min(endings,na.rm = T) & start>time_start
                  , node, NA) %>% na.omit() %>% unique() %>% length(),
      j2 =  ifelse(event %in% "jump" & start > min(endings,na.rm = T) & start>time_start
                   , 1, 0) %>% sum(),
      e2 =  ifelse(!is.na(`old.value`) & `old.value`!="" & start > min(endings,na.rm = T) & start>time_start
                   , 1, 0) %>% sum()
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(round_cols),~ round(.x,1)),
                  dplyr::across(dplyr::all_of(na_cols),~ ifelse(.x==0,NA,.x)))


  testthat::expect_equal(actual_output, expected_output)


})


testthat::test_that('pre.process.audits works',{
  test_audit_path <- testthat::test_path('fixtures/audits_test')
  test_loaded  <- load.audit.files(dir.audits = test_audit_path, track.changes = T)

  test_loaded2 <- test_loaded[1:10,]

  actual_output <- pre.process.audits(test_loaded2,0.5)

  expected_output <- test_loaded2
  expected_output$tag <- ifelse(expected_output$duration %_>_% 30,paste0(
    expected_output[expected_output$duration %_>_% 30,]$uuid,'-',
    expected_output[expected_output$duration %_>_% 30,]$question),''
  )
  expected_output[expected_output$duration %_>_% 30,'duration'] <- 0

  testthat::expect_equal(actual_output,expected_output)

})


testthat::test_that('process.audit.geospatial works, test 1 - general functionality',{

  test_audit_path <- testthat::test_path('fixtures/audits_geospatial')
  test_loaded  <- load.audit.files(dir.audits = test_audit_path, track.changes = T)


  actual_result <- suppressWarnings(test_loaded %>%
                                      dplyr::group_by(uuid) %>%
                                      dplyr::group_modify(~process.audit.geospatial(.x, start_q ='informed_consent', end_q = 'j3_prevented_access_education')) %>%
                                      dplyr::ungroup() %>%
                                      dplyr::select(-c(longitude,latitude,start,end,accuracy)))

  expected_result <- data.frame(
    uuid = c(unique(test_loaded$uuid)[1], rep(unique(test_loaded$uuid)[2],6),unique(test_loaded$uuid)[3],
             rep(unique(test_loaded$uuid)[4],6),rep(unique(test_loaded$uuid)[5],1),rep(unique(test_loaded$uuid)[6],3),
             rep(unique(test_loaded$uuid)[7],1),rep(unique(test_loaded$uuid)[8],1),rep(unique(test_loaded$uuid)[9],1),
             rep(unique(test_loaded$uuid)[10],1),rep(unique(test_loaded$uuid)[11],4),rep(unique(test_loaded$uuid)[12],1),
             rep(unique(test_loaded$uuid)[13],6)
    ),
    question = c("all", "informed_consent", "informed_consent", "site_name_uid_list",
                 "a8_1_1_administration_training", "b6_allocation_plan", "j3_prevented_access_education",
                 "all", "informed_consent", "a1_2_people_can_hosted_number", "a8_3_services_of_gbv",
                 "c4_bomb_shelter", "e2_2_top_3_wash_needs", "j3_prevented_access_education",
                 "all", "informed_consent", "a9_hum_assist", "j3_prevented_access_education",
                 "all", "all", "all", "all", "informed_consent", "children_0_17",
                 "c1_1_3_smooth_functioning_cold_water_supply", "j3_prevented_access_education",
                 "all", "informed_consent", "a8_2_data_availability", "children_0_17",
                 "b5_individuals_evicted", "c6_receiving_shelter_support", "j3_prevented_access_education"),
    variable_explanation = c('issue', paste0('coordinate ',c(1:6)),'issue', paste0('coordinate ',c(1:6)),'issue',
                             paste0('coordinate ',c(1:3)),rep('issue',4),paste0('coordinate ',c(1:4)),'issue',
                             paste0('coordinate ',c(1:6))
    ),
    issue = c(
      'j3_prevented_access_education is not present for this uuid', rep('To be cheked',6),
      'j3_prevented_access_education is not present for this uuid',rep('To be cheked',6),
      'Enum disabled location tracking',rep('To be cheked',3),rep('j3_prevented_access_education is not present for this uuid',4),
      rep('To be cheked',4),'j3_prevented_access_education is not present for this uuid',rep('To be cheked',6)
    )
  ) %>%
    dplyr::tibble()

  testthat::expect_equal(actual_result,expected_result)


})


testthat::test_that('process.audit.geospatial works, test 2 - missing variable warning but the result is still there',{

  test_audit_path <- testthat::test_path('fixtures/audits_geospatial')
  test_loaded  <- load.audit.files(dir.audits = test_audit_path, track.changes = T)



  expected_result <- data.frame(
    uuid = unique(test_loaded$uuid)[1],
    question = "all",
    variable_explanation = 'issue',
    issue = 'j3_prevented_access_education is not present for this uuid'
  ) %>%
    dplyr::tibble()


  testthat::expect_warning(
    testthat::expect_equal(test_loaded[test_loaded$uuid==unique(test_loaded$uuid)[1],] %>%
                             dplyr::group_by(uuid) %>%
                             dplyr::group_modify(~process.audit.geospatial(.x, start_q ='informed_consent', end_q = 'j3_prevented_access_education')) %>%
                             dplyr::ungroup() %>%
                             dplyr::select(-c(longitude,latitude,start,end,accuracy)),expected_result),
    paste0("The questions you've entered are not present in the data for uuid: ",unique(test_loaded$uuid)[1]))

})

testthat::test_that('process.audit.geospatial works, test 3 - error when there is no geo data',{

  test_audit_path <- testthat::test_path('fixtures/audits_geospatial')
  test_loaded  <- load.audit.files(dir.audits = test_audit_path, track.changes = T)


  testthat::expect_error(
    test_loaded%>%
      dplyr::select(-longitude) %>%
      dplyr::group_by(uuid) %>%
      dplyr::group_modify(~process.audit.geospatial(.x, start_q ='informed_consent', end_q = 'j3_prevented_access_education')) %>%
      dplyr::ungroup() %>%
      dplyr::select(-c(longitude,latitude,start,end)),
    'Error, no geospatial data in your log file. Please double check')

})


testthat::test_that('process.audit.geospatial works, test 4 - no geotracking warning but the result is still there',{

  test_audit_path <- testthat::test_path('fixtures/audits_geospatial')
  test_loaded  <- load.audit.files(dir.audits = test_audit_path, track.changes = T)



  expected_result <- data.frame(
    uuid = unique(test_loaded$uuid)[5],
    question = "all",
    variable_explanation = 'issue',
    issue = 'Enum disabled location tracking'
  ) %>%
    dplyr::tibble()


  testthat::expect_warning(
    testthat::expect_equal(test_loaded[test_loaded$uuid==unique(test_loaded$uuid)[5],] %>%
                             dplyr::group_by(uuid) %>%
                             dplyr::group_modify(~process.audit.geospatial(.x, start_q ='informed_consent', end_q = 'j3_prevented_access_education')) %>%
                             dplyr::ungroup() %>%
                             dplyr::select(-c(longitude,latitude,start,end,accuracy)),expected_result),
    'The enumerator has disabled the geolocation tracking')

})


testthat::test_that('process.audit.geospatial works, test 5 - empty coordinates',{

  test_audit_path <- testthat::test_path('fixtures/audits_geospatial')
  test_loaded  <- load.audit.files(dir.audits = test_audit_path, track.changes = T)

  expected_result <- data.frame(
    uuid = unique(test_loaded$uuid)[2],
    question = "all",
    variable_explanation = 'issue',
    issue = 'No coordinates for the chosen question range'
  ) %>%
    dplyr::tibble()


  testthat::expect_warning(
    testthat::expect_equal(test_loaded[test_loaded$uuid==unique(test_loaded$uuid)[2],] %>%
                             dplyr::mutate(longitude = NA,latitude = NA) %>%
                             dplyr::group_by(uuid) %>%
                             dplyr::group_modify(~process.audit.geospatial(.x, start_q ='informed_consent', end_q = 'j3_prevented_access_education')) %>%
                             dplyr::ungroup() %>%
                             dplyr::select(-c(longitude,latitude,start,end,accuracy)),expected_result),
    'All values between the chosen questions are NA. Returning an empty df')

})


testthat::test_that('process.audit.geospatial works, test 6 - missing variable warning but the result is still there + no uuid2',{

  test_audit_path <- testthat::test_path('fixtures/audits_geospatial')
  test_loaded  <- load.audit.files(dir.audits = test_audit_path, track.changes = T)



  expected_result <- data.frame(
    uuid = unique(test_loaded$uuid)[1],
    question = "all",
    variable_explanation = 'issue',
    issue = 'j3_prevented_access_education is not present for this uuid'
  ) %>%
    dplyr::tibble()


  testthat::expect_warning(
    testthat::expect_equal(test_loaded[test_loaded$uuid==unique(test_loaded$uuid)[1],] %>%
                             dplyr::select(-uuid2) %>%
                             dplyr::group_by(uuid) %>%
                             dplyr::group_modify(~process.audit.geospatial(.x, start_q ='informed_consent', end_q = 'j3_prevented_access_education')) %>%
                             dplyr::ungroup() %>%
                             dplyr::select(-c(longitude,latitude,start,end,accuracy)),expected_result),
    "The questions you've entered are not present in the data for this uuid")
})


