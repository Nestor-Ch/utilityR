
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




#' Recode select_multiple responses: set to NA.
#'
#' Changes all 1s and 0s to NA in choice columns, sets cumulative variable and _other text answers to NA.
#'
#' @param data Dataframe containing records which will be affected.
#' @param variable String containing the name of the select_multiple variable.
#' @param issue  String with explanation used for the cleaning log entry.
#' @param other_var_name the name of the variable containing the "other" responses
#' @param CL_COLS string of columns of the cleaning log defauls to c("uuid", "loop_index", "variable", "old.value", "new.value", "issue")
#'
#' @return Dataframe containing cleaning log entries constructed from `data`.
#'
#' @examples
#' \dontrun{
#' recode.multiple.set.NA(data = filter(raw.main, condition),
#'  variable = "question2")
#'   }
recode.multiple.set.NA <- function(data, variable, issue, other_var_name = NULL,
                                   CL_COLS = c("uuid", "loop_index", "variable",
                                               "old.value", "new.value", "issue")){

  if(!all(variable %in% names(data))){
    stop(paste0('Selected variable names are not present in the data',": ",
                paste0(dplyr::setdiff(variable,names(data)),collapse=', ')))
  }else{

  ccols <- colnames(data)[stringr::str_starts(colnames(data), paste0(variable, "/"))]

  # filter out cases that already are NA
  data <- data %>% dplyr::filter(!dplyr::if_all(dplyr::all_of(ccols), ~is.na(.)))
  if(nrow(data)>0){
    cl_cummulative <- data %>% dplyr::select(dplyr::any_of(c("uuid", "loop_index", variable))) %>%
      dplyr::mutate(variable = variable, old.value = !!rlang::sym(variable), new.value = NA, issue = issue) %>%
      dplyr::select(dplyr::any_of(CL_COLS))

    cl_choices <- data.frame()
    for(col in ccols){
      df <- data %>% dplyr::filter(!is.na(!!rlang::sym(col)))
      if(nrow(df)>0){
        cl <- df %>%
          dplyr::mutate(variable = col, old.value = !!rlang::sym(col), new.value = NA, issue = issue) %>%
          dplyr::select(dplyr::any_of(CL_COLS))

        cl_choices <- rbind(cl_choices, cl)
        # remove text from text other response if present
        if(stringr::str_ends(col, "/other")){
          other_var_name <- ifelse(is.null(other_var_name), paste0(variable, "_other"), other_var_name)
          cl_other_text <- df %>% dplyr::filter(!is.na(!!rlang::sym(other_var_name))) %>%
            dplyr::mutate(variable = other_var_name, old.value = !!rlang::sym(other_var_name),
                   new.value = NA, issue = issue) %>%
            dplyr::select(dplyr::any_of(CL_COLS))

          cl_choices <- rbind(cl_choices, cl_other_text)
        }
      }
    }
    return(rbind(cl_cummulative, cl_choices))

  }
  return(data.frame())

}
}


#' Recode select_multiple responses: set answer to one particular choice.
#'
#' This function affects choice columns: setting 1 for `choices` and 0 everywhere else.
#' For the cumulative variable, the value will be more or less equal to `paste(choices, collapse=" ")`
#' (to be precise, the exact order of choices will be the same as the one in `tool.choices`) ...
#' Additionally, the _other variable will be set to NA if it is not one of the `choices`
#'
#' @param data Dataframe containing records which will be affected.
#' @param variable String containing the name of the select_multiple variable.
#' @param choices Vector of strings (or a single string) containing the choices that will be added. They must be valid options for this variable.
#' @param issue String with explanation used for the cleaning log entry.
#' @param other_var_name the name of your _other variable - variable containing string responses, NULL by default
#' @param tool.survey your survey sheet in the tool
#' @param tool.choices your choices sheet in the tool
#'
#' @return Dataframe containing cleaning log entries constructed from `data`.
#'
#' @examples
#' \dontrun{
#' recode.multiple.set.choices(data = filter(raw.main, condition),
#' variable = "question_name", choices = "option", issue = "explanation")
#' }
recode.multiple.set.choices <- function(data, variable, choices, issue, other_var_name = NULL,
                                        tool.survey, tool.choices){

  choice_columns <- paste0(variable,"/",choices)
  if(any(!choice_columns %in% colnames(data))){
    stop(paste("Columns",paste(choice_columns, collapse = ", "),"not present in data!"))}

  ls_name <- get.choice.list.from.name(variable, label_colname = 'label::English', tool.survey)

    # find the new value for the cumulative variable (get the proper order from tool.choices)
  newvalue <- tool.choices %>% dplyr::filter(list_name == ls_name & name %in% choices) %>%
    dplyr::pull(name) %>% paste(collapse = " ")
  anychoice_pattern <- paste0("(",choices,")", collapse = "|")

  # filter out NA and cases that already have only these choices selected
  data <- data %>% dplyr::filter(!!rlang::sym(variable) != newvalue)

  if(nrow(data) > 0){
    cl_cummulative <- data %>% dplyr::select(dplyr::any_of(c("uuid", "loop_index", variable))) %>%
      dplyr::rename(old.value = !!rlang::sym(variable)) %>%
      dplyr::mutate(variable = variable, new.value = newvalue, issue = issue)

    # set all other choices columns to 0
    cols <- colnames(data)[stringr::str_starts(colnames(data), paste0(variable, "/")) &
                             !(stringr::str_ends(colnames(data), anychoice_pattern))]

    cl_choices <- rbind(recode.set.value.regex(data, choice_columns, "0", "1", issue),
                        recode.set.value.regex(data, cols, "1", "0", issue))

    if(!"other" %in% choices & any(grepl('other', tool.choices %>%
                                         dplyr::filter(list_name == ls_name) %>%
                                         dplyr::pull(name)))) {
      # remove text from other responses

      # get the name of the _other column
      basic_name_pattern <-  paste0("(",paste0(unlist(stringr::str_split(variable,'_')),collapse = ".*"),"_other)")

      other_var_name <- ifelse(is.null(other_var_name),
                               names(data)[grepl(basic_name_pattern, names(data))]
                               , other_var_name)
      cl_other <- recode.set.NA(data, other_var_name, issue)
      return(rbind(cl_cummulative, cl_choices, cl_other))
    } else{
      return(rbind(cl_cummulative, cl_choices))
    }
  }
  return(data.frame())
}




#' Recode select_multiple responses: add particular choices.
#'
#' Changes all 0s to 1s in choice columns specified by `choices`. Modifies cumulative variable too.
#'
#' @note This function does not affect entries that have NA in `variable`.
#'
#' @param data Dataframe containing records which will be affected.
#' @param variable String containing the name of the select_multiple variable.
#' @param choices Vector of strings (or a single string) containing the choices that will be added. They must be valid options for this variable.
#' @param issue String with explanation used for the cleaning log entry.
#'
#' @return Dataframe containing cleaning log entries constructed from `data`.
#'
#' @examples
#' \dontrun{
#' recode.multiple.add.choices(data = filter(raw.main, condition), variable = "question_name",
#' choices = c("option1", "option2"), issue = "explanation")
#' }
recode.multiple.add.choices <- function(data, variable, choices, issue){

  choice_columns <- paste0(variable,"/",choices)
  if(any(!choice_columns %in% colnames(data))){
    stop(paste("\nColumn",choice_columns[!choice_columns %in% colnames(data)],"not present in data!"))
  }
  choices_pattern <- paste0("(",paste0(choices, collapse = ")|("), ")")
  choices_len <- stringr::str_length(paste0(choices, collapse = "")) + length(choices)-1
  # filter out cases that already have all choices selected
  data <- data %>%
    dplyr::select(any_of(c("uuid", "loop_index", variable)), dplyr::all_of(choice_columns)) %>% dplyr::filter(!is.na(!!rlang::sym(variable))) %>%
    dplyr::mutate(variable2 = stringr::str_squish(stringr::str_remove_all(!!rlang::sym(variable), choices_pattern))) %>%
    dplyr::mutate(len_diff = stringr::str_length(!!rlang::sym(variable)) - stringr::str_length(variable2)) %>%
    dplyr::filter(stringr::str_length(!!rlang::sym(variable)) - stringr::str_length(variable2) != choices_len)
  if(nrow(data) > 0){
    cl_cummulative <- data %>% dplyr::select(dplyr::any_of(c("uuid", "loop_index", variable, "variable2"))) %>%
      dplyr::rename(old.value = !!rlang::sym(variable)) %>%
      dplyr::mutate(variable = variable, new.value = stringr::str_squish(paste(variable2, paste0(choices, collapse = " "))), issue = issue) %>%
      dplyr::select(-variable2)
    if(all(cl_cummulative$new.value %==na% cl_cummulative$old.value)) {
      cl_cummulative <- data.frame()
    }
    choice_columns <- paste0(variable,"/",choices)
    cl_choices <- recode.set.value.regex(data, choice_columns, "0", "1", issue)
    return(rbind(cl_cummulative, cl_choices))
  }
  return(data.frame())
}


#' Recode select_multiple responses: remove particular choices.
#'
#' Removes the relevant text from the cummulative column. Changes all 1s to 0 in choice columns specified by `choices`.
#' Also, if one of the `choices` is "other", then the text variable (_other) will be changed to NA.
#'
#' @note This function does not affect entries that have NA in `variable`.
#'
#' @param data  Dataframe containing records which will be affected.
#' @param variable variable String containing the name of the select_multiple variable.
#' @param choices choices Vector of strings (or a single string) containing the choices that will be removed. They must be valid options for this variable.
#' @param issue issue String with explanation used for the cleaning log entry.
#' @param other_var_name the name of your _other variable - variable containing string responses, NULL by default
#' @param CL_COLS string of columns of the cleaning log defauls to c("uuid", "loop_index", "variable", "old.value", "new.value", "issue")
#'
#' @return Dataframe containing cleaning log entries constructed from `data`
#'
#' @examples
#' \dontrun{
#' recode.multiple.remove.choices(data = filter(raw.main, condition), variable = "question_name",
#' choices = c("option1", "option2"), issue = "explanation")
#' }

recode.multiple.remove.choices <- function(data, variable, choices, issue, other_var_name = NULL,
                                           CL_COLS = c("uuid", "loop_index", "variable",
                                                       "old.value", "new.value", "issue")){

  choice_columns <- paste0(variable,"/",choices)
  if(any(!choice_columns %in% colnames(data))) stop(paste("Column",choice_columns[!choice_columns %in% colnames(data)],"were not found in data!"))

  # filter to include only rows that are not NA, and that have at least one of the choices selected
  anychoice_pattern <- paste0("(",choices,")", collapse = "|")

    data_1 <- data %>% dplyr::filter(!is.na(!!rlang::sym(variable)) & stringr::str_detect(!!rlang::sym(variable), anychoice_pattern))

  if(nrow(data_1) > 0){
    # remove the choices from cumulative column using a combined regex pattern
    cl_cummulative <- data_1 %>%  dplyr::select( dplyr::any_of(CL_COLS),  dplyr::all_of(variable)) %>%
      dplyr::rename(old.value = !!rlang::sym(variable)) %>%
      dplyr::mutate(variable = variable, new.value = stringr::str_squish(stringr::str_remove_all(old.value, anychoice_pattern)), issue = issue)

    cl_choices <- recode.set.value.regex(data_1, choice_columns, "1", "0", issue)

    if(!"other" %in% choices) { return(rbind(cl_cummulative, cl_choices))
    } else{
      basic_name_pattern <-  paste0("(",paste0(unlist(stringr::str_split(variable,'_')),collapse = ".*"),"_other)")
      other_var_name <- ifelse(is.null(other_var_name),
                               names(data)[grepl(basic_name_pattern, names(data))]
                               , other_var_name)

      cl_other <- recode.set.NA(data, other_var_name, issue)

      other_bin_var_name = paste0(variable,"/",'other')
      cl_choices_other <- recode.set.value.regex(data, other_bin_var_name, "1", "0", issue)

      return(rbind(cl_cummulative, cl_choices, cl_choices_other,cl_other))
    }
  }
  return(data.frame())

}


