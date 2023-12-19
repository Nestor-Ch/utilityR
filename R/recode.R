
#' Recode a question by setting variables to NA if they are equal to a given value (code).
#'
#' @note DO NOT use this function for select_multiple questions. Instead use `recode.multiple.set.NA`
#'
#' @param data Dataframe containing records which will be affected.
#' @param variables Vector of strings (or a single string) containing the names of the variables.
#' @param code Vector of strings (or a single string) which will be changed to NA.
#' @param issue String with explanation used for the cleaning log entry.
#' @param ignore_case Whether `code` should be matched case-insensitively. Defaults to True.
#' @param CL_COLS string of columns of the cleaning log defauls to c("uuid","uniqui", "loop_index", "variable", "old.value", "new.value", "issue")
#'
#' @export
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
                             CL_COLS = c("uuid", 'uniqui',"loop_index", "variable",
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
#' @param CL_COLS string of columns of the cleaning log defauls to c("uuid","uniqui", "loop_index", "variable", "old.value", "new.value", "issue")
#'
#' @returns Dataframe containing cleaning log entries constructed from `data`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  recode.set.NA.regex(data = dplyr::filter(raw.main, condition),
#'  variables = c("question1", "question2"),
#'  pattern = "birth_certificates?", issue = "explanation")
#'  }

recode.set.NA.regex <- function(data, variables, pattern, issue,
                                CL_COLS = c("uuid", 'uniqui',"loop_index", "variable",
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
#' @export
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
#' @param CL_COLS string of columns of the cleaning log defauls to c("uuid","uniqui", "loop_index", "variable", "old.value", "new.value", "issue")
#'
#' @return Dataframe containing cleaning log entries constructed from `data`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' recode.set.value.regex(data = filter(raw.main, condition),
#'  variables = c("question1", "question2"),
#'   pattern = "birth_certificates?", issue = "explanation")
#'   }
recode.set.value.regex <- function(data, variables, pattern, new.value, issue, affect_na = FALSE,
                                   CL_COLS = c("uuid",'uniqui' ,"loop_index", "variable",
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
#' @param CL_COLS string of columns of the cleaning log defauls to c("uuid","uniqui", "loop_index", "variable", "old.value", "new.value", "issue")
#'
#' @return Dataframe containing cleaning log entries constructed from `data`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' recode.multiple.set.NA(data = filter(raw.main, condition),
#'  variable = "question2")
#'   }
recode.multiple.set.NA <- function(data, variable, issue, other_var_name = NULL,
                                   CL_COLS = c("uuid", 'uniqui',"loop_index", "variable",
                                               "old.value", "new.value", "issue")){

  if(!all(variable %in% names(data))){
    stop(paste0('Selected variable names are not present in the data',": ",
                paste0(dplyr::setdiff(variable,names(data)),collapse=', ')))
  }else{

    ccols <- colnames(data)[stringr::str_starts(colnames(data), paste0(variable, "/"))]

    # filter out cases that already are NA
    data <- data %>% dplyr::filter(!dplyr::if_all(dplyr::all_of(ccols), ~is.na(.)))
    if(nrow(data)>0){
      cl_cummulative <- data %>% dplyr::select(dplyr::any_of(c("uuid", 'uniqui',"loop_index", variable))) %>%
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
#' @export
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
    cl_cummulative <- data %>% dplyr::select(dplyr::any_of(c("uuid",'uniqui' ,"loop_index", variable))) %>%
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
#' @export
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
    dplyr::select(any_of(c("uuid", 'uniqui',"loop_index", variable)), dplyr::all_of(choice_columns)) %>%
    dplyr::filter(!is.na(!!rlang::sym(variable))) %>%
    dplyr::mutate(variable2 = stringr::str_squish(stringr::str_remove_all(!!rlang::sym(variable), choices_pattern))) %>%
    dplyr::mutate(len_diff = stringr::str_length(!!rlang::sym(variable)) - stringr::str_length(variable2)) %>%
    dplyr::filter(stringr::str_length(!!rlang::sym(variable)) - stringr::str_length(variable2) != choices_len)
  if(nrow(data) > 0){
    cl_cummulative <- data %>% dplyr::select(dplyr::any_of(c("uuid",'uniqui', "loop_index", variable, "variable2"))) %>%
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
#' @param CL_COLS string of columns of the cleaning log defauls to c("uuid","uniqui", "loop_index", "variable", "old.value", "new.value", "issue")
#'
#' @return Dataframe containing cleaning log entries constructed from `data`
#'
#' @export
#'
#' @examples
#' \dontrun{
#' recode.multiple.remove.choices(data = filter(raw.main, condition), variable = "question_name",
#' choices = c("option1", "option2"), issue = "explanation")
#' }

recode.multiple.remove.choices <- function(data, variable, choices, issue, other_var_name = NULL,
                                           CL_COLS = c("uuid", 'uniqui',"loop_index", "variable",
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



#' Recode others: function for recoding select_one questions
#'
#' @param or.select_one Your filled out recoding form after translation and verification  (with only select ones)
#' @param orig_response_col The name of your original column with the untranslated responses, "response.uk" by default
#' @param print_debug Whether you want the function to print out what it's doing
#' @param tool.survey_others The survey sheet of your kobo tool
#' @param tool.choices_others The choices sheet of your kobo tool
#' @param label_colname The name of your english label column. "label::English" by default
#' @param CL_COLS string of columns of the cleaning log. Defauls to c("uuid","uniqui", "loop_index", "variable", "old.value", "new.value", "issue")
#'
#' @note  _others suffix is added to the kobo tool dataframes to avoid recursion
#'
#' @export
#'
#' @return A dataframe for the changes that need to be applied to the data
#'
#' @examples
#' \dontrun{
#' recode.others_select_one(or.select_one = other_requests_file, tool.survey_others=tool.survey,
#' tool.choices_others = tool.choices)
#' }
recode.others_select_one <- function(or.select_one, orig_response_col = "response.uk", print_debug = T,
                                     tool.survey_others=tool.survey, tool.choices_others =tool.choices,label_colname = "label::English",
                                     CL_COLS = c("uuid", 'uniqui',"loop_index", "variable",
                                                 "old.value", "new.value", "issue")){

  # invalid select_ones:
  or.select_one.remove <- dplyr::filter(or.select_one, !is.na(invalid.v))
  if(print_debug) {
    cat(paste("Number of invalid other select_one responses:", nrow(or.select_one.remove)), "\n")
  }
  cl_s1_remove <- rbind(
    or.select_one.remove %>%
      dplyr::mutate(variable = name, old.value = !!rlang::sym(orig_response_col), new.value = NA, issue = "Invalid other response") %>%
      dplyr::select(dplyr::any_of(CL_COLS)),
    or.select_one.remove %>%
      dplyr::mutate(variable = ref.name, old.value = "other", new.value = NA, issue = "Invalid other response") %>%
      dplyr::select(dplyr::any_of(CL_COLS))
  )

  # recoding select_ones:
  or.select_one.recode <- or.select_one %>%
    dplyr::filter(!is.na(existing.v)) %>%
    dplyr::mutate(list_name = sapply(ref.name,function(x){get.choice.list.from.name(x, tool.survey=tool.survey_others, label_colname=label_colname)}),
                  existing.v = stringr::str_remove_all(existing.v, ";"))
  if(print_debug) {
    cat(paste("Number of select_one responses to be recoded:", nrow(or.select_one.recode)), "\n")
  }
  choices_lookup <- or.select_one.recode %>%
    dplyr::select(existing.v, list_name) %>%
    dplyr::rename(label = existing.v) %>%
    dplyr::left_join(tool.choices_others %>%
                       dplyr::rename(label = !!rlang::sym(label_colname), choice_name = name), by = c("label", "list_name")) %>%
    dplyr::distinct()
  if(any(is.na(choices_lookup$choice_name))) {
    missing_names <- choices_lookup %>%
      dplyr::filter(is.na(choice_name))
    stop("Choices not found in lists:\n\t", paste(missing_names$label, missing_names$list_name, sep = "\t - in list ", collapse = "\n\t"))
  }
  cl_s1_recode <- rbind(
    or.select_one.recode %>%
      dplyr::mutate(variable = name, old.value = !!rlang::sym(orig_response_col), new.value = NA, issue = "Recoding other response") %>%
      dplyr::select(dplyr::any_of(CL_COLS)),
    or.select_one.recode %>%
      dplyr::rename(label = existing.v) %>%
      dplyr::left_join(choices_lookup, by = c("list_name", "label")) %>%
      dplyr::mutate(variable = ref.name, old.value = "other", new.value = choice_name, issue = "Recoding other response") %>%
      dplyr::select(dplyr::any_of(CL_COLS))
  )

  # true select_ones:
  or.select_one.true <- dplyr::filter(or.select_one, !is.na(true.v))
  if(print_debug) {
    cat("Number of true other select_one responses:", nrow(or.select_one.true), "\n")
  }
  cl_s1_true <- or.select_one.true %>%
    dplyr::mutate(variable = name, old.value = !!rlang::sym(orig_response_col), new.value = true.v, issue = "Translating other response") %>%
    dplyr::select(dplyr::any_of(CL_COLS))

  return(rbind(cl_s1_true, cl_s1_remove, cl_s1_recode))
}




#' Recode others: function for recoding select_multiple questions
#'
#' @param data your dataframe
#' @param or.select_multiple Your filled out recoding form after translation and verification (with only select multiple)
#' @param orig_response_col The name of your original column with the untranslated responses, "response.uk" by default
#' @param print_debug Whether you want the function to print out what it's doing
#' @param is.loop Whether you're working with loop data
#' @param tool.survey_others The survey sheet of your kobo tool
#' @param tool.choices_others  The choices sheet of your kobo tool
#' @param label_colname The name of your english label column. "label::English" by default
#' @param CL_COLS CL_COLS string of columns of the cleaning log. Defauls to c("uuid","uniqui" ,"loop_index", "variable", "old.value", "new.value", "issue")
#'
#' @return A dataframe for the changes that need to be applied to the data
#' @export
#' @note When running loop data, make sure to have a uuid column in your dataframe. This ensures that the resulting tables are uniform
#'
#' @examples
#' \dontrun{
#' recode.others_select_multiple(data, or.select_one = other_requests_file, tool.survey_others=tool.survey,
#' tool.choices_others = tool.choices)
#' }

recode.others_select_multiple <- function(data, or.select_multiple, orig_response_col = "response.uk", print_debug = T, is.loop ,
                                          tool.survey_others=tool.survey, tool.choices_others =tool.choices,label_colname = "label::English",
                                          CL_COLS = c("uuid",'uniqui', "loop_index", "variable",
                                                      "old.value", "new.value", "issue")){


  if(!'uuid' %in% names(data) & is.loop){
    stop(print('Your loop data doesnt have the uuid variable, please check the data and rename uuid appropriately'))
  }

  # filter out uuids not in the data

  if(any(! or.select_multiple$uniqui %in% data$uniqui)){
    cat(paste0('The following unique IDs from your requests file were not found in the data provided: ',
               paste0(dplyr::setdiff(or.select_multiple$uniqui,data$uniqui),collapse=', '), '. Please double check '
    ))
    or.select_multiple <- or.select_multiple[or.select_multiple$uniqui %in% data$uniqui,]
  }


  # invalid select_multiples:
  or.select_multiple.remove <- dplyr::filter(or.select_multiple, !is.na(invalid.v))
  cl_sm_remove <- dplyr::tibble()
  if (nrow(or.select_multiple.remove) > 0) {
    if(print_debug) {
      cat(paste("Number of invalid select_multiple responses:", nrow(or.select_multiple.remove)), "\n")
    }
    variables <- or.select_multiple.remove %>%
      dplyr::select(ref.name, name) %>%
      dplyr::distinct(ref.name, .keep_all = T)
    for(variable in variables %>%
        dplyr::pull(ref.name)){
      thisvar_data <- data %>%
        dplyr::filter(uniqui %in% (or.select_multiple.remove %>%
                                     dplyr::filter(ref.name == variable) %>%
                                     dplyr::pull(uniqui)))
      other_variable <- variables$name[which(variables$ref.name == variable)]
      # if the 'other' was the only one selected, change the entire question to NA:
      cl_only_other <- thisvar_data %>%
        dplyr::filter(!!rlang::sym(variable) == "other") %>%
        recode.multiple.set.NA( variable, "Invalid other response", other_var_name = other_variable,
                                CL_COLS = CL_COLS)
      cl_notjust_other <- thisvar_data %>%
        dplyr::filter(!!rlang::sym(variable) != "other") %>%
        recode.multiple.remove.choices(variable, "other", "Invalid other response", other_var_name = other_variable,
                                       CL_COLS = CL_COLS)
      cl_sm_remove <- rbind(cl_sm_remove, cl_only_other, cl_notjust_other)
    }
  }

  # recoding select_multiples:

  or.select_multiple.recode <- or.select_multiple %>%
    dplyr::filter(!is.na(existing.v)) %>%
    dplyr::mutate(list_name = sapply(ref.name,function(x){get.choice.list.from.name(x, tool.survey=tool.survey_others, label_colname=label_colname)}),
                  existing.v = stringr::str_split(stringr::str_squish(existing.v), " *; *", simplify = T))

    cl_sm_recode <- dplyr::tibble()
  if(nrow(or.select_multiple.recode) > 0){
    if(print_debug) {
      cat(paste("Number of select_multiple responses to be recoded:", nrow(or.select_multiple.recode)), "\n")
    }
    choices_lookup <- tool.choices_others %>%
      dplyr::filter(list_name %in% or.select_multiple.recode$list_name) %>%
      dplyr::rename(label = !!rlang::sym(label_colname), choice_name = name)

    for (r in 1:nrow(or.select_multiple.recode)) {
      or.row <- or.select_multiple.recode[r,]
      data.row <- data %>%
        dplyr::filter(uniqui == or.row$uniqui)

      chosen_labels <- or.row$existing.v[or.row$existing.v != "" & !is.na(or.row$existing.v)]

      if(any(!chosen_labels %in% choices_lookup$label)) {
        # add a test for breaking
        stop("Choice '", paste(chosen_labels[!chosen_labels %in% choices_lookup$label], collapse = "' "), "' not found in list ", or.row$list_name)
      }

      choices <- choices_lookup %>%
        dplyr::filter(list_name == or.row$list_name & label %in% chosen_labels) %>%
        dplyr::pull(choice_name)

      # check if true.v is also not na
      if(!is.na(or.row$true.v)){

        # in this case, simply add new choices
        cl_sm_recode <- rbind(cl_sm_recode, or.row %>%
                                dplyr::mutate(variable = name, old.value = !!rlang::sym(orig_response_col), new.value = true.v,
                                              issue = "Translating other response") %>%
                                dplyr::select(any_of(CL_COLS)))

        cl_sm_recode_add_choice <- recode.multiple.add.choices(data.row, or.row$ref.name, choices, "Recoding other response")

        cl_sm_recode <- cl_sm_recode %>% dplyr::mutate_all(as.character)
        cl_sm_recode_add_choice <- cl_sm_recode_add_choice %>% dplyr::mutate_all(as.character)
        cl_sm_recode <- dplyr::bind_rows(cl_sm_recode,cl_sm_recode_add_choice)
      }else{

        # read the previous choices and set selection to previous + new
        old_choices <- data.row[[or.row$ref.name]] %>% stringr::str_split(" ", simplify = T)
        choices <- c(choices, old_choices[old_choices!="other"])

        cl_sm_recode_add_ch <- recode.multiple.set.choices(data.row, or.row$ref.name, choices, "Recoding other response",
                                                           other_var_name = or.row$name, tool.survey = tool.survey_others,
                                                           tool.choices = tool.choices_others)

        cl_sm_recode_add_ch <- cl_sm_recode_add_ch %>% dplyr::mutate_all(as.character)

        cl_sm_recode <- dplyr::bind_rows(cl_sm_recode,cl_sm_recode_add_ch)
      }
    }
  }

  # true select_multiples:

  or.select_multiple.true <- or.select_multiple %>%
    dplyr::filter(!is.na(true.v) & check == 2)
  if(print_debug){
    cat(paste("Number of true select_multiple responses:", nrow(or.select_multiple.true)), "\n")
  }
  cl_sm_true <- or.select_multiple.true %>%
    dplyr::mutate(variable = name, old.value = !!rlang::sym(orig_response_col), new.value = true.v,
                  issue = "Translating other response") %>%
    dplyr::select(any_of(CL_COLS))

  # When working with non-loop data we will have an error as loop_index column doesn't exist in the raw.main df. We add it here
  # loop data will have columns uniqui and uuid. Main will only have uniqui

  cl_sm_true <- cl_sm_true %>% dplyr::mutate_all(as.character)
  cl_sm_remove <- cl_sm_remove %>% dplyr::mutate_all(as.character)
  cl_sm_recode <- cl_sm_recode %>% dplyr::mutate_all(as.character)

  result_fin <- dplyr::bind_rows(cl_sm_true, cl_sm_remove, cl_sm_recode)



  if(!is.loop & !'loop_index' %in% names(result_fin)){
    result_fin$loop_index <- NA_character_
  }
  return(result_fin)

}




#' Create a cleaning log for recoding other responses
#'
#' Use the filled out other_requests file to create cleaning.log.other.
#' Run this function on every datasheet your data has (main, loop1, loop2, etc...)
#' Unexpected behavior may occur if some variables from `or.edited` are not present in the data.
#'
#' @param data Dataframe containing Kobo data
#' @param or.edited Dataframe containing the filled-out other_requests file. Needs to contain the standard TEI (True, Existing, Invalid) columns
#' in one of the following formats: (true.v,existing.v, invalid.v) or (true.other, existing.other, invalid.other),
#' as well as column `check`. Please use the ```load.requests``` function to load these xlsx files.
#' @param orig_response_col The name of the column which stores the original (untranslated!) response for each question.
#' @param is.loop Set this to True if the provided dataframe is a loop (it needs to contain loop_index and uuid columns
#' @param print_debug Whether debugging information will be printed to screen (about how many responses will be recoded/translated etc.).
#' @param tool.survey The survey sheet of your kobo tool
#' @param tool.choices The choices sheet of your kobo tool
#' @param label_colname The name of your english label column. "label::English" by default
#'
#' @return Dataframe containing cleaning log entries covering recoding others, constructed from `data` and `or.edited`
#'
#' @export
#'
#' @examples
#' \dontrun{
#' recode.others(data, or.edited = other_requests_file, orig_response_col = "response.uk",tool.survey=tool.survey,
#' tool.choices = tool.choices)
#' }
recode.others <- function(data, or.edited, orig_response_col = "response.uk", is.loop , print_debug = T,
                          tool.survey, tool.choices, label_colname  = "label::English"){

  # a new thing: UNIQUI - universal unique identifier (either loop_index or uuid)
  # it will be used for matching records to or.edited entries :)
  if(!is.loop) {
    or.edited <- or.edited %>%
#      dplyr::select(-any_of("loop_index")) %>%
      dplyr::mutate(uniqui = uuid)
    data <- data %>%
      dplyr::mutate(uniqui = uuid)
  }else{
    if(!"loop_index" %in% colnames(or.edited)){
      stop("Parameter is.loop = TRUE, but column loop_index was not found in or.edited!")
    }
    else{
      or.edited <- or.edited %>%
        dplyr::mutate(uniqui = loop_index)
      data <- data %>%
        dplyr::mutate(uniqui = loop_index)
    }
  }

  # fix legacy naming
  if(!"existing.v" %in% colnames(or.edited)) {
    if("existing.other" %in% colnames(or.edited)) {
      or.edited <- or.edited %>%
        dplyr::rename_with(~gsub(".other", ".v", .), dplyr::ends_with(".other"))
    }else{
      stop("Column 'existing.v' not found in or.edited!\n\tPlease check your requests file.")
    }
  }
  # the column 'check' must be present in the or.edited (meaning you must use the validate = T option when loading requests)
  if(!"check" %in% colnames(or.edited)){
    stop("Column 'check' was not found in or.edited!\n\tPlease, use the `validate` option in load.requests.")
  }

  # check for missing identifiers:
  if(any(!or.edited$uniqui %in% data$uniqui)){
    ids <- or.edited %>%
      dplyr::distinct(uniqui) %>%
      dplyr::pull()

    missing_ids <- ids[!ids %in% data$uniqui]
    if(length(missing_ids) == length(ids)){
      stop("NONE of the identifiers from or.edited were found in data!")
    }else{
      if(print_debug){
        warning("Identifiers from or.edited not found in data:\n\t", paste(missing_ids, collapse = ",\n\t"), "\n")
      }
    }
  }


  # HANDLE SELECT_ONES:
  or.select_one <- or.edited %>%
    dplyr::filter(ref.type == "select_one")
  if(print_debug){
    cat("Total number of select_one other responses:", nrow(or.select_one), "\n")
  }
  s1_data <- data %>%
    dplyr::filter(uniqui %in% or.select_one$uniqui)
  if(nrow(s1_data) == 0) {
    cl_select_one <- dplyr::tibble()
  }else {
    cl_select_one <- recode.others_select_one(or.select_one,
                                              print_debug = print_debug,
                                              tool.survey_others = tool.survey, tool.choices_others = tool.choices,
                                              orig_response_col = orig_response_col,label_colname =label_colname )
  }
  # HANDLE SELECT_MULTIPLES:
  or.select_multiple <- or.edited %>%
    dplyr::filter(ref.type == "select_multiple")
  if(print_debug){
    cat("Total number of select_multiple other responses:", nrow(or.select_multiple), "\n")
  }
  sm_data <- data %>%
    dplyr::filter(uniqui %in% or.select_multiple$uniqui)
  if(nrow(sm_data) == 0){
    cl_select_multiple <- dplyr::tibble()
  }else {
    cl_select_multiple <- recode.others_select_multiple(data=sm_data,or.select_multiple= or.select_multiple,
                                                        orig_response_col=orig_response_col, print_debug = print_debug,
                                                        tool.survey_others = tool.survey, tool.choices_others = tool.choices,
                                                        is.loop=is.loop,
                                                        label_colname = label_colname )
  }
  # works fine for non-loops

  cl_all_others <- rbind(cl_select_one, cl_select_multiple) %>%
    dplyr::filter(old.value %not=na% new.value)

  return(cl_all_others)
}


#' Recode translated columns
#'
#' Recode translate requests into a cleaning log file
#'
#' @param requests The file with the translated requests loaded with the `load.requests` function
#' @param response_col The original untranslated response column. "response.uk" by default
#'
#' @return Returns a cleaning log file
#' @export
#'
#' @examples
#' \dontrun{
#' recode.trans.requests(requests = requests_file, response_col = "response.uk")
#' }
recode.trans.requests <- function(requests,response_col){

  if(!'invalid.v' %in% names(requests)){
    stop('invalid.v column is not in the present in your translated requests file.
         Please double check and make sure to load it with the load.requests function')
  }

  if(!'true.v' %in% names(requests)){
    stop('true.v column is not in the present in your translated requests file.
         Please double check and make sure to load it with the load.requests function')
  }

  trans.invalid <- requests %>%
    dplyr::filter(!is.na(invalid.v))

  if(nrow(trans.invalid)>0){
    result_invalid <- trans.invalid %>%
      dplyr::rename(variable = name,
                    old.value = rlang::sym(response_col)) %>%
      dplyr::mutate(new.value = NA,
                    issue = 'Invalid response') %>%
      dplyr::select(uuid,loop_index, variable, old.value, new.value, issue)
  }else{
    result_invalid <- data.frame()
  }

  trans.valid <- requests %>%
    dplyr::filter(!is.na(true.v))

  if(nrow(trans.valid)>0){
    result_valid <- trans.valid %>%
      dplyr::rename(variable = name,
                    old.value = rlang::sym(response_col)) %>%
      dplyr::mutate(new.value = true.v,
                    issue = 'Translating other response') %>%
      dplyr::select(uuid,loop_index, variable, old.value, new.value, issue)
  }else{
    result_valid <- data.frame()
  }


  result <- rbind(result_invalid,result_valid)
}


















