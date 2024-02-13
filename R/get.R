#' Find the type of variables.
#'
#' @param variable This is the name of the header from raw data.
#' @param tool.survey This is the tool.survey data.frame
#'
#' @return It will return the question type from the kobo tool
#' @export
#'
#' @examples
#' \dontrun{
#' question_type <- get.type(variable = "a2_partner",tool.survey = tool.survey)
#' }
get.type <- function(variable,
                     tool.survey = NULL){
  if(is.null(tool.survey)) stop("tool.survey is not provided.")
  res <- data.frame(name = variable) %>%
    dplyr::left_join(dplyr::select(tool.survey, name, q.type), by = "name", na_matches = "never") %>%
    dplyr::mutate(q.type = ifelse(is.na(q.type) & stringr::str_detect(name, "/"), "select_multiple", q.type))
  if(any(is.na(res$q.type))){
    warning(paste("Variables not found in tool.survey:", paste0(res[is.na(res$q.type),] %>% dplyr::pull(name),collapse = ", ")))
  }
  return(dplyr::pull(res, q.type))
}


#' Find the label of a variable
#'
#' @param variable This is the name of the header from raw data.
#' @param label_colname This is the label_colname input
#' @param tool.survey This is the tool.survey data.frame
#'
#' @return It will return the label of the question from the kobo tool
#' @export
#'
#' @examples
#' \dontrun{
#' label <- get.label(variable = "a2_partner", label_colname = label_colname, tool.survey = tool.survey)
#' }
get.label <- function(variable,
                      label_colname = NULL,
                      tool.survey= NULL){

  if(is.null(tool.survey)) stop("tool.survey is not provided.")
  if(is.null(label_colname)) stop("label_colname is not provided.")

  not_in_tool <- variable[!variable %in% tool.survey$name]
  if(length(not_in_tool) > 0){
    warning(paste("Variables not found in tool.survey:", paste0(not_in_tool, collapse = ", ")))
  }
  if (any(stringr::str_detect(variable, "/"))) variable <- stringr::str_split(variable, "/", 2, T)[,1]
  res <- data.frame(name = variable) %>%
    dplyr::left_join(dplyr::select(tool.survey, name, !!rlang::sym(label_colname)), by = "name", na_matches = "never")

  return(dplyr::pull(res, label_colname))
}

#' Find the label of the Choice
#'
#' @param choice the name of the choice
#' @param list the name of the list containing choice
#' @param label_colname This is the label_colname input
#' @param tool.choices This is the tool.choices data.frame
#'
#' @return It will return the label of the choice
#' @export
#'
#' @examples
#' \dontrun{
#' choice_label <- get.choice.label(choice = "yes", list = "yn",
#'                                  label_colname = label_colname,
#'                                  tool.choices = tool.choices)
#' }
get.choice.label <- function(choice,
                             list,
                             label_colname = NULL,
                             tool.choices = NULL){

  if(is.null(tool.choices)) stop("tool.choices is not provided.")
  if(is.null(label_colname)) stop("label_colname is not provided.")
  if(!list %in% tool.choices$list_name) stop(paste("list",list, "not found in tool.choices!"))
  tool.choices <- tool.choices %>%
    dplyr::select(name, list_name, all_of(label_colname))

  tool.choices <- tool.choices[tool.choices$list_name == list,]

  res <- data.frame(name = unlist(choice)) %>%
    dplyr::left_join(tool.choices,by = "name", na_matches = "never")
  if(any(is.na(res[[label_colname]]))){
    culprits <- paste0(res[is.na(res[[label_colname]]),] %>%
                         dplyr::pull(name), collapse = ", ")
    warning(paste0("Choices not in the list (", list, "):", culprits))
  }
  # if(nrow(res) == 0) stop("All choices not in the list!")

  res_vec <- dplyr::pull(res, label_colname)
  return(res_vec)
}

#' Find the name of the Choice from label
#'
#' @param choice_label the label of the choice
#' @param list the name of the list containing choice
#' @param label_colname This is the label_colname input
#' @param tool.choices This is the tool.choices data.frame
#'
#' @return It will return the name of the choice label
#' @export
#'
#' @examples
#' \dontrun{
#' choice_name <- get.choice.name.from.label(choice = "Yes", list = "yn",
#'                                  label_colname = label_colname,
#'                                  tool.choices = tool.choices)
#' }
get.choice.name.from.label <- function(choice_label,
                                       list,
                                       label_colname = NULL,
                                       tool.choices = NULL){

  if(is.null(tool.choices)) stop("tool.choices is not provided.")
  if(is.null(label_colname)) stop("label_colname is not provided.")
  if(!list %in% tool.choices$list_name) stop(paste("list",list, "not found in tool.choices!"))
  tool.choices <- tool.choices %>%
    dplyr::select(name, list_name, all_of(label_colname))

  tool.choices <- tool.choices[tool.choices$list_name == list,]

  res <- data.frame(unlist(choice_label))
  names(res) <- label_colname
  res <- res %>%
    dplyr::left_join(tool.choices,by = label_colname, na_matches = "never")

  if(any(is.na(res[['name']]))){
    culprits <- paste0(res[is.na(res[[label_colname]]),] %>%
                         dplyr::pull(name), collapse = ", ")
    warning(paste0("Choices not in the list (", list, "):", culprits))
  }
  # if(nrow(res) == 0) stop("All choices not in the list!")

  res_vec <- dplyr::pull(res, name)
  return(res_vec)
}


#' Find the choices list name using name
#'
#' @param variable This is the name of the header from raw data.
#' @param tool.survey This is the tool.survey data.frame
#'
#' @return It will return the list_name value of the chosen variable
#' @export
#'
#' @examples
#' \dontrun{
#' list_name_from_name <- get.choice.list.from.name(variable = "a2_partner",
#'                                                  tool.survey = tool.survey)
#' }
get.choice.list.from.name <- function(variable,
                                      tool.survey = NULL){

  if(is.null(tool.survey)) stop("tool.survey is not provided.")

  not_in_tool <- variable[!variable %in% tool.survey$name]
  if(length(not_in_tool) > 0){
    warning(paste("Variables not found in tool.survey:", paste0(not_in_tool, collapse = ", ")))
  }

  if (stringr::str_detect(variable, "/")) variable <- stringr::str_split(variable, "/")[[1]][1]
  result <- tool.survey[tool.survey$name == variable & !is.na(tool.survey$name),]
  return(result %>% dplyr::pull(list_name))
}


#' Finds the choice list for a question basing on its type
#'
#' @param q_type The type of the variable from the kobo tool
#'
#' @return the list_name of the choices
#' @export
#'
#' @examples
#' \dontrun{
#'   choice_list_name <- get.choice.list.from.type("select_one a2_partner")
#' }
get.choice.list.from.type <- function(q_type){
  q_type.1 <- stringr::str_split(q_type, " ")[[1]]
  if (length(q_type.1)==1) return(NA)
  else return(q_type.1[2])
}

#' Finds parent question basing on relevancy text
#'
#' @param q_relevancy The relevancy entry from the kobo tool
#'
#' @return the parent questions variable name
#' @export
#'
#' @examples
#' \dontrun{
#' parent_variable <- get.ref.question("selected(${b17_access_stores}, 'other'"))
#' }
#'
get.ref.question <- function(q_relevancy){
  q_relevancy.1 <- stringr::str_split(q_relevancy, "\\{")[[1]][2]
  return(stringr::str_split(q_relevancy.1, "\\}")[[1]][1])
}


#' Finds all 'select' type questions, and their choices
#'
#' @param tool.choices This is the tool.choices data.frame
#' @param tool.survey This is the tool.survey data.frame
#' @param label_colname This is the label_colname input (usually `label::English``)
#'
#' @return a data.frame including all select questions with the type, name, label, q.type, list_name, and choices
#' @export
#'
#' @examples
#' \dontrun{
#' select_db <- get.select.db(tool.choices = tool.choices,
#'                            tool.survey = tool.survey,
#'                            label_colname = label_colname)
#' }
#'
get.select.db <- function(tool.choices = NULL,
                          tool.survey = NULL,
                          label_colname = NULL){
  if(is.null(tool.survey)) stop("tool.survey is not provided.")
  if(is.null(tool.choices)) stop("tool.choices is not provided.")
  if(is.null(label_colname)) stop("label_colname is not provided.")

  # list of choices for each list_name (from TOOL_CHOICES)
  list.choices <- tool.choices[!is.na(tool.choices$list_name),] %>% dplyr::group_by(list_name) %>%
    dplyr::mutate(choices=paste(name, collapse=";\n"),
           choices.label=paste(!!rlang::sym(label_colname), collapse=";\n")) %>%
    dplyr:: summarise(choices=choices[1], choices.label=choices.label[1])
  # list of choices for each question
  select.questions <- tool.survey %>%
    dplyr::rename(q.label=label_colname) %>%
    dplyr::select(type, name, q.label) %>%
    dplyr::mutate(q.type=as.character(lapply(type, function(x) return(stringr::str_split(x, " ")[[1]][1]))),
           list_name=as.character(lapply(type, get.choice.list.from.type)))
  select.questions <- select.questions[select.questions$list_name!="NA" & select.questions$list_name!="group" & select.questions$list_name!="repeat",] %>%
    dplyr::left_join(list.choices, by="list_name")
  select.questions <- select.questions[!is.na(select.questions$choices),]
  return(select.questions)
}


#' Finds all 'other' questions and their ref question and choices
#'
#' @param tool.choices This is the tool.choices data.frame
#' @param tool.survey This is the tool.survey data.frame
#' @param label_colname This is the label_colname input (usually `label::English``)
#'
#' @return Dataframe containing name, parent name, full.label, parent type, choices, choices label
#' @export
#'
#' @examples
#' \dontrun{
#' other_db <- get.other.db(tool.choices = tool.choices,
#'                          tool.survey = tool.survey,
#'                          label_colname = label_colname)
#' }
#'
get.other.db <- function(tool.survey= NULL,
                         tool.choices= NULL,
                         label_colname = NULL){
  if(is.null(tool.survey)) stop("tool.survey is not provided.")
  if(is.null(tool.choices)) stop("tool.choices is not provided.")
  if(is.null(label_colname)) stop("label_colname is not provided.")
  select.questions <- get.select.db(tool.choices = tool.choices,
                                    tool.survey = tool.survey,
                                    label_colname = label_colname)

 # for each "other" question, get ref.question and list of choices
 other.db <- tool.survey[stringr::str_ends(tool.survey$name, "_other") & tool.survey$type=="text",] %>%
    dplyr::rename(label=label_colname) %>%
    dplyr::select("name", "label", "relevant") %>%
    dplyr::mutate(ref.name=as.character(lapply(relevant, get.ref.question))) %>%
    dplyr::left_join(dplyr::select(select.questions, "name", "q.type", "q.label", "list_name", "choices", "choices.label"),
              by=c("ref.name"="name")) %>%
    dplyr::rename(ref.label=q.label, ref.type=q.type) %>%
    dplyr::mutate(full.label=paste0(ref.label, " - ", label)) %>%
    dplyr::select(name, ref.name, full.label, ref.type, choices, choices.label)

  return(other.db)
}

#' Finds all questions which should be translated (meaning all 'text'-type question that are not 'other's)
#'
#' @param tool.choices This is the tool.choices data.frame
#' @param tool.survey This is the tool.survey data.frame
#' @param label_colname This is the label_colname input (usually `label::English``)
#'
#' @return Dataframe containing name,full.label
#' @export
#'
#' @examples
#' \dontrun{
#' trans_db <- get.trans.db(tool.choices = tool.choices,
#'                          tool.survey = tool.survey,
#'                          label_colname = label_colname)
#' }
get.trans.db <- function(tool.survey= NULL,
                         tool.choices= NULL,
                         label_colname = NULL){
  if(is.null(tool.survey)) stop("tool.survey is not provided.")
  if(is.null(tool.choices)) stop("tool.choices is not provided.")
  if(is.null(label_colname)) stop("label_colname is not provided.")
  select.questions <- get.select.db(tool.choices = tool.choices,
                                    tool.survey = tool.survey,
                                    label_colname = label_colname)

  trans.db <- tool.survey[!stringr::str_ends(tool.survey$name, "_other") & tool.survey$type == "text",]
  trans.db <- trans.db %>%
    dplyr::rename(label=label_colname) %>%
    dplyr::select("name", "label")
  return(trans.db)
}



#' Finds all text questions
#'
#' @param tool.choices This is the tool.choices data.frame
#' @param tool.survey This is the tool.survey data.frame
#' @param label_colname This is the label_colname input (usually `label::English``)
#'
#' @return A dataframe of all text questions in your survey
#' @export
#'
#' @note This function exists for cases where the person who coded the Kobo questionnaire didn't follow the conventional rules
#' For writing them and some of the questions were ommited from other `other.db` or `trans.db`. This function will create an
#' object that is compatible with both of those dataframes and relevant rows from it can be appened to either `other.db`
#' or `trans.db`.
#'
#' @examples
#' \dontrun{
#' text.db <- get.text.db(tool.choices = tool.choices,
#'                          tool.survey = tool.survey,
#'                          label_colname = label_colname)
#' }
get.text.db <- function(tool.survey= NULL,
                         tool.choices= NULL,
                         label_colname = NULL){
  if(is.null(tool.survey)) stop("tool.survey is not provided.")
  if(is.null(tool.choices)) stop("tool.choices is not provided.")
  if(is.null(label_colname)) stop("label_colname is not provided.")
  select.questions <- get.select.db(tool.choices = tool.choices,
                                    tool.survey = tool.survey,
                                    label_colname = label_colname)

  # for each "other" question, get ref.question and list of choices
  text.db <- tool.survey[tool.survey$type=="text",] %>%
    dplyr::rename(label=label_colname) %>%
    dplyr::select("name", "label", "relevant") %>%
    dplyr::mutate(ref.name=as.character(lapply(relevant, get.ref.question))) %>%
    dplyr::left_join(dplyr::select(select.questions, "name", "q.type", "q.label", "list_name", "choices", "choices.label"),
                     by=c("ref.name"="name")) %>%
    dplyr::rename(ref.label=q.label, ref.type=q.type) %>%
    dplyr::mutate(full.label=ifelse(!isna(ref.label),paste0(ref.label, " - ", label),label)) %>%
    dplyr::select(name, ref.name, full.label, ref.type, choices, choices.label)
  return(text.db)
}














