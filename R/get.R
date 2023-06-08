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
#' label <- get.label(variable = "a2_partner",tool.survey = tool.survey)
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

#' Title
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



