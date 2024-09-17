

#' Test for timeline anomalies within enumerator audit files
#'
#' The function tests for duration outliers within the distribution of duration answers of enumerators based on
#' the geographic variable selected.
#'
#' @param ls - The list of variables to be tested (it is recommended to group questions)
#' @param audit - The audit file
#' @param data - The main dataframe
#' @param geo_column - The column in the main dataframe indicating the geographic allocation of the variable
#' @param enum_name - The name of the column containing enumerator IDs
#' @param ipq_n - The percentage of problematic questions that mark an interview as problematic. 0.3 by default
#' @param n_limit - The minimum number of interviews the enumerator has to have to be considered
#'
#' @return - a dataframe of problematic enumerators
#' @export
#'
#' @examples
#' \dontrun{
#' anomaly_test_tmln_int(ls = ls_test,audit = test_audits,data = test_data,
#' geo_column = 'q2_4_1_oblast',enum_name = 'q0_2_enum_id')
#' }

anomaly_test_tmln_int <- function(ls, audit,data,geo_column, enum_name, ipq_n =0.3, n_limit = 10){

  if(!all(c(ls) %in% audit$question)){
    nm <- setdiff(c(ls), audit$question)
    stop(paste0("Error: some of the questions you've entered are not present in your data. please double check: ",
                paste0(nm,collapse=', ') ))
  }

  if(!all(c(enum_name,geo_column) %in% names(data))){
    nm <- setdiff(c(enum_name,geo_column),names(data))
    stop('The following variables are not present in your dataframe:', paste0(nm, collapse = ', '))

  }

  if(!is.numeric(ipq_n)|!is.numeric(n_limit)){
    stop("The ipq_n or n_limit you've chosen is non-numeric. Please fix this")
  }


  if(!dplyr::between(ipq_n,0,1)){
    stop("The ipq_n is not between 0 and 1. This is wrong.")
  }

  if(!('uuid' %in% names(data)) | !('uuid' %in% names(audit)) ){
    stop('uuid is not present in your data or audit objects please double check')
  }


  if(!any(unique(audit$uuid) %in% data$uuid )){
    stop('None of the uuids present in the audit files are present in the dataframe, please double check')
  }

  general <- audit %>%
    dplyr::filter(question %in% ls,
                  duration >0.5) %>%
    dplyr::left_join(data %>%
                       dplyr::select(`uuid`, dplyr::all_of(c(enum_name,geo_column))) %>%
                       dplyr::distinct()) %>%
    dplyr::group_by(!!rlang::sym(enum_name),!!rlang::sym(geo_column)) %>%
    dplyr::mutate(n_resp = length(unique(uuid))) %>%
    dplyr::filter(
      n_resp>n_limit,
      !is.na(!!rlang::sym(enum_name)),
      !is.na(question),
      !question %in% '') %>%
    dplyr::select(uuid,!!rlang::sym(enum_name),!!rlang::sym(geo_column),question, duration) %>%
    dplyr::mutate(duration = duration/60,
                  duration = ifelse(duration == 0, 00000000.1,duration)) %>%
    dplyr::mutate(duration = log(duration))

  if(nrow(general)>0){

    collected <- general %>%
      dplyr::left_join(general %>%
                         dplyr::group_by(question,!!rlang::sym(geo_column)) %>%
                         dplyr::summarise(mean_q = mean(duration, na.rm = T),
                                          sd_q = sd(duration, na.rm = T))) %>%
      dplyr::mutate(issue = abs(duration)<=(abs(mean_q)-2*abs(sd_q)))


    base_issues <- collected %>%
      dplyr::group_by(!!rlang::sym(enum_name),uuid,!!rlang::sym(geo_column)) %>%
      dplyr::summarise(
        questions = length(question),
        respondents = length(unique(uuid)),
        issues = sum(issue, na.rm =T)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        ipq = issues/questions,
        problematic_interview = ipq>ipq_n)

    enum_issues <- base_issues %>%
      dplyr::group_by(!!rlang::sym(enum_name),!!rlang::sym(geo_column)) %>%
      dplyr::summarise(respondents = sum(respondents),
                       questions = sum(questions),
                       issues = sum(issues),
                       ipq = mean(ipq),
                       problematic_interview = sum(problematic_interview)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(ppr = problematic_interview/respondents) %>%
      dplyr::rename(
        total_respondents = respondents,
        total_questions = questions,
        issues_per_questions =ipq,
        problematic_interviews_perc = ppr
      )


    enum_issues$list_used <- substr(ls[1],start = 1,stop = 4)

    return(enum_issues)
  }else{
    return(dplyr::tibble())
  }

}


#' Test individual enumerator differences in the audit files
#'
#' The function tests for inter-enumerator differences within the distribution of duration answers of enumerators based on
#' the geographic variable selected.
#'
#' @param variable_list - The list of variables to be tested (it is recommended to group questions)
#' @param audit - The audit file
#' @param data_frame - The main dataframe
#' @param enum_name - The name of the column containing enumerator IDs
#' @param geo_column - The column in the main dataframe indicating the geographic allocation of the variable
#' @param n_limit - The minimum number of interviews the enumerator has to have to be considered
#' @param sig_thr - The threshold for the significance filter of the differences test. 0.05 by default
#'
#' @return - a dataframe of problematic enumerators
#' @export
#'
#' @examples
#' \dontrun{
#' anomaly_test_enum_diffr(variable_list = ls_test,
#' audit = test_audits,data_frame = test_data,
#' geo_column = 'oblast',enum_name = 'q0_2_enum_id')
#' }

anomaly_test_enum_diffr <- function(variable_list, audit, data_frame, enum_name, geo_column,
                                    n_limit =10, sig_thr = 0.05){


  if(!all(c(variable_list) %in% audit$question)){
    nm <- setdiff(c(variable_list), audit$question)
    stop(paste0("Error: some of the questions you've entered are not present in your audit data. please double check: ",
                paste0(nm,collapse=', ') ))
  }

  if(!all(c(enum_name,geo_column) %in% names(data_frame))){
    nm <- setdiff(c(enum_name,geo_column),names(data_frame))
    stop('The following variables are not present in your dataframe:', paste0(nm, collapse = ', '))

  }

  if(!is.numeric(n_limit) | !is.numeric(sig_thr)){
    stop("The n_limit or sig_thr you've chosen is non-numeric. Please fix this")
  }

  if(!dplyr::between(sig_thr,0,1)){
    stop("The sig_thr is not between 0 and 1. This is wrong.")
  }

  if(!('uuid' %in% names(data_frame)) | !('uuid' %in% names(audit)) ){
    stop('uuid is not present in your data or audit objects please double check')
  }


  if(!any(unique(audit$uuid) %in% data_frame$uuid )){
    stop('None of the uuids present in the audit files are present in the dataframe, please double check')
  }


  general <- audit %>%
    dplyr::filter(question %in% variable_list) %>%
    dplyr::left_join(data_frame %>%
                       dplyr::select(`uuid`, dplyr::all_of(enum_name)) %>%
                       dplyr::distinct()) %>%
    dplyr::group_by(!!rlang::sym(enum_name)) %>%
    dplyr::mutate(n_resp = length(unique(uuid))) %>%
    dplyr::filter(
      n_resp>n_limit,
      !is.na(!!rlang::sym(enum_name)),
      !is.na(question),
      !question %in% '') %>%
    dplyr::select(uuid,!!rlang::sym(enum_name),question, duration) %>%
    dplyr::mutate(duration = duration/60,
                  duration = ifelse(duration == 0, 00000000.1,duration)) %>%
    dplyr::mutate(duration = log(duration))

  if(nrow(general)>0){

    general <- general %>%
      dplyr::left_join(data_frame %>%
                         dplyr::select(uuid,dplyr::all_of(geo_column)) %>%
                         dplyr::distinct())

    txt <- paste0('duration ~ ', enum_name)

    post_hoc_test_result <-  general %>%
      dplyr::group_by(!!rlang::sym(enum_name), uuid,!!rlang::sym(geo_column)) %>%
      dplyr::summarise(duration = mean(duration)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(!!rlang::sym(geo_column)) %>%
      rstatix::tukey_hsd(formula =  eval(parse(text = txt)))

    result <- post_hoc_test_result %>%
      dplyr::filter(p.adj<=sig_thr)


    if (nrow(result)>0){

      result_fin <- rbind(result %>%
                            dplyr::group_by(!!rlang::sym(geo_column), group1) %>%
                            dplyr::summarise(cnt = dplyr::n()),
                          result %>%
                            dplyr::group_by(!!rlang::sym(geo_column), group2) %>%
                            dplyr::summarise(cnt = dplyr::n()) %>%
                            dplyr::rename(group1 = group2)) %>%
        dplyr::group_by(!!rlang::sym(geo_column), group1) %>%
        dplyr::summarise(cnt = sum(cnt)) %>%
        dplyr::left_join(data_frame %>%
                           dplyr::select(!!rlang::sym(enum_name), !!rlang::sym(geo_column)) %>%
                           dplyr::distinct() %>%
                           dplyr::group_by(!!rlang::sym(geo_column)) %>%
                           dplyr::summarise(cnt_en = dplyr::n())) %>%
        dplyr::ungroup()

      names(result_fin)[names(result_fin) %in% 'group1'] <- enum_name

      result_fin <- result_fin %>%
        dplyr::mutate(perc_issues = cnt/cnt_en) %>%
        dplyr::left_join(
          general %>%
            dplyr::group_by(!!rlang::sym(enum_name), !!rlang::sym(geo_column)) %>%
            dplyr::summarise(duration = mean(duration)) %>%
            dplyr::left_join(general %>%
                               dplyr::group_by(!!rlang::sym(geo_column)) %>%
                               dplyr::summarise(duration_ob = mean(duration)))
        ) %>%
        dplyr::filter(duration< duration_ob,
                      perc_issues > 0.5) %>%
        dplyr::rename(differences_from_enums = cnt,
                      total_enums = cnt_en) %>%
        dplyr::select(-duration, -duration_ob)

      result_fin$list_used <- substr(variable_list[1],start = 1,stop = 4)


      return(result_fin)
    }else{
      return(dplyr::tibble())
    }

  }else{
    return(dplyr::tibble())
  }
}













