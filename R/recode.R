
#' Recode a question by setting variables to NA if they are equal to a given value (code).
#'
#' @note DO NOT use this function for select_multiple questions. Instead use `recode.multiple.set.NA`
#'
#' @param data Dataframe containing records which will be affected.
#' @param variables Vector of strings (or a single string) containing the names of the variables.
#' @param code Vector of strings (or a single string) which will be changed to NA.
#' @param issue String with explanation used for the cleaning log entry.
#' @param ignore_case Whether `code` should be matched case-insensitively. Defaults to True.
#' @param CL_COLS string of columns of the cleaning log defauls to c("uuid", "loop_index", "variable", "old.value", "new.value", "issue")
#'
#' @returns Dataframe containing cleaning log entries constructed from `data`.
#'
#' @examples
#' \dontrun{
#'  recode.set.NA.if(data = filter(raw.main, condition),
#'  variables = c("question1", "question2"),
#'  code = "999", issue = "explanation")
#'  }

recode.set.NA.if <- function(data, variables, code, issue, ignore_case = T,
                             CL_COLS = c("uuid", "loop_index", "variable",
                                         "old.value", "new.value", "issue")){

  if(!all(variables %in% names(data))){
    stop(paste0('Selected variable names are not present in the data',": ",
                paste0(dplyr::setdiff(variables,names(data)),collapse=', ')))
  }else{

    clog <- dplyr::tibble()
    for(variable in variables){
      if(ignore_case) data1 <- data %>% dplyr::filter(stringr::str_to_lower(!!rlang::sym(variable)) %in% stringr::str_to_lower(code))
      else data1 <- data %>% dplyr::filter(!!rlang::sym(variable) %in% code)
      cl <- data1 %>% dplyr::mutate(variable = variable, old.value = !!rlang::sym(variable), new.value = NA,
                                    issue = issue) %>% dplyr::select(dplyr::any_of(CL_COLS))
      clog <- rbind(clog, cl)
    }
    return(clog)
  }
}

#' Recode a question by setting variables to NA if they are matching a regex pattern.
#'
#' This function is also useful also if you need to simply set some variables to NA - you can put ".*" as the `pattern`.
#'
#' @note DO NOT use this function for select_multiple questions. Instead use `recode.multiple.set.NA`
#'
#' @param data Dataframe containing records which will be affected.
#' @param variables Vector of strings (or a single string) containing the names of the variables.
#' @param pattern Regex pattern which will be used to find entries that will be turned to NA.
#' @param issue String with explanation used for the cleaning log entry.
#' @param CL_COLS string of columns of the cleaning log defauls to c("uuid", "loop_index", "variable", "old.value", "new.value", "issue")
#'
#' @returns Dataframe containing cleaning log entries constructed from `data`.
#'
#' @examples
#' \dontrun{
#'  recode.set.NA.regex(data = dplyr::filter(raw.main, condition),
#'  variables = c("question1", "question2"),
#'  pattern = "birth_certificates?", issue = "explanation")
#'  }

recode.set.NA.regex <- function(data, variables, pattern, issue,
                                CL_COLS = c("uuid", "loop_index", "variable",
                                            "old.value", "new.value", "issue")){
  if(!all(variables %in% names(data))){
    stop(paste0('Selected variable names are not present in the data',": ",
                paste0(dplyr::setdiff(variables,names(data)),collapse=', ')))
  }else{

    clog <- dplyr::tibble()
    for(variable in variables){
      data1 <- data %>% dplyr::filter(stringr::str_detect(!!rlang::sym(variable), pattern = pattern))
      cl <- data1 %>% dplyr::mutate(variable = variable, old.value = !!rlang::sym(variable), new.value = NA,
                                    issue = issue) %>% dplyr::select(dplyr::any_of(CL_COLS))
      clog <- rbind(clog, cl)
    }
    return(clog)
  }}


#' Set the given variables for the given entries to NA.
#'
#' @param data Dataframe containing records which will be affected.
#' @param variables Vector of strings (or a single string) containing the names of the variables.
#' @param issue String with explanation used for the cleaning log entry.
#'
#' @returns Dataframe containing cleaning log entries constructed from `data`.
#'
#' @examples
#' \dontrun{
#'  recode.set.NA(data = dplyr::filter(raw.main, condition),
#'  variables = c("question1", "question2"), issue = "explanation")
#'  }

recode.set.NA <- function(data, variables, issue){
  if(!all(variables %in% names(data))){
    stop(paste0('Selected variable names are not present in the data',": ",
                paste0(dplyr::setdiff(variables,names(data)),collapse=', ')))
  }else{

    recode.set.NA.regex(data, variables, ".*", issue)
  }}



#' Recode a question by setting variables to some new value if they are matching a regex pattern.
#'
#' @param data Dataframe containing records which will be affected.
#' @param variables Vector of strings (or a single string) containing the names of the variables.
#' @param new.value New value for the recoded rows
#' @param pattern Regex pattern which will be used to find entries that will be turned to NA.
#' @param issue String with explanation used for the cleaning log entry.
#' @param affect_na Whether NA values in the column should be recoded as well.
#' @param CL_COLS string of columns of the cleaning log defauls to c("uuid", "loop_index", "variable", "old.value", "new.value", "issue")
#'
#' @return Dataframe containing cleaning log entries constructed from `data`.
#'
#' @examples
#' \dontrun{
#' recode.set.value.regex(data = filter(raw.main, condition),
#'  variables = c("question1", "question2"),
#'   pattern = "birth_certificates?", issue = "explanation")
#'   }
recode.set.value.regex <- function(data, variables, pattern, new.value, issue, affect_na = FALSE,
                                   CL_COLS = c("uuid", "loop_index", "variable",
                                               "old.value", "new.value", "issue")){
  if(!all(variables %in% names(data))){
    stop(paste0('Selected variable names are not present in the data',": ",
                paste0(dplyr::setdiff(variables,names(data)),collapse=', ')))
  }else{

    clog <- dplyr::tibble()
    for(variable in variables){
      if(affect_na) data1 <- data %>% dplyr::filter(stringr::str_detect(!!rlang::sym(variable), pattern = pattern) | is.na(!!rlang::sym(variable)))
      else data1 <- data %>% dplyr::filter(stringr::str_detect(!!rlang::sym(variable), pattern = pattern))
      data1 <- data1 %>% dplyr::filter(!!rlang::sym(variable) %not=na% new.value)
      cl <- data1 %>% dplyr::mutate(variable = variable, old.value = !!rlang::sym(variable), new.value = new.value,
                                    issue = issue) %>% dplyr::select(dplyr::any_of(CL_COLS))
      clog <- rbind(clog, cl)
    }
    return(clog)
  }
}



