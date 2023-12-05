testthat::test_that('process.uuid works',{
  test_audit_path <- testthat::test_path('fixtures/audits_test')
  test_loaded  <- load.audit.files(dir.audits = test_audit_path, track.changes = T)


  actual_output <- test_loaded %>%
    dplyr::group_by(uuid) %>%
    dplyr::group_modify(~process.uuid(.x)) %>%
    dplyr::ungroup()

  # test 1 - basic functionality

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

  # test 2 - breaks if the first event is not form.start

  test_loaded2 <- test_loaded
  test_loaded2[1,]$event = 'fake_event'

  testthat::expect_error(
    test_loaded2 %>%
      dplyr::group_by(uuid) %>%
      dplyr::group_modify(~process.uuid(.x)) %>%
      dplyr::ungroup()
  )

  # test 3 - if one form.exit is followed by another form.exit without form.resume - should give warning
  test_loaded2 <- test_loaded %>% dplyr::select(-uuid2)
  test_loaded2[93,]$event = 'form.exit'
  test_loaded2[93,]$start = test_loaded2[93,]$start+10

  testthat::expect_warning(
  test_loaded2 %>%
    dplyr::group_by(uuid) %>%
    dplyr::group_modify(~process.uuid(.x)) %>%
    dplyr::ungroup(),"status=waiting while form.exit!"
  )

  # Test 4. use uuid2 in the load process to get better error messages on process.uuid

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


  # Test 5. throws error when one of the audits doesn't have a form.exit event

  test_loaded2 <- test_loaded
  test_loaded2[92,]$event = 'fake'

  testthat::expect_error(
  test_loaded2 %>%
    dplyr::group_by(uuid) %>%
    dplyr::group_modify(~process.uuid(.x)) %>%
    dplyr::ungroup(), "Your form doesn't have the form.exit event. Please double-check: 00f695c4-9a42-4b8f-9fe1-5c9f3e13a2d2"
  )

})


testthat::test_that('pre.process.audits works',{
  test_audit_path <- testthat::test_path('fixtures/audits_test')
  test_loaded  <- load.audit.files(dir.audits = test_audit_path, track.changes = T)

  test_loaded2 <- test_loaded[1:10,]

  actual_output <- pre.process.audits(test_loaded2,1)

  expected_output <- test_loaded2
  expected_output[expected_output$duration %_>_% 60,'duration'] <- 0

  testthat::expect_equal(actual_output,expected_output)

})





