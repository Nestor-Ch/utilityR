#' Load the label column name from the tool survey
#'
#' @param filename_tool a link to the tool filename
#' @param language define the language of the label to be used
#'
#' @return the name of the label column
#' @export
#'
#' @examples
#' \dontrun{
#' label_colname <- load.label.colname(filename_tool, language = "English")
#' }
load.label.colname <- function(filename_tool, language = "English"){
  tool_colnames <- readxl::read_xlsx(filename_tool, sheet = "survey", col_types = "text") %>% names
  return(tool_colnames[agrep(paste0("label::",language), tool_colnames)])
}


#' Load the 'choices' tab from a Kobo tool.
#'
#' @param filename_tool This is the path to the file that contains the tool
#' @param label_colname This is the variable of the label colname
#'
#' @return A dataframe: tool.choices, it's the same as the 'choices' tab from the tool, filtered to include only distinct rows.
#' @export
#'
#' @examples
#' \dontrun{
#' filename_tool <- "tool.xlsx"
#' label_colname <- load.label.colname(filename_tool)
#' tool_choices <- load.tool.choices(filename_tool, label_colname)
#' }

load.tool.choices <- function(filename_tool, label_colname){
  tool.choices <- readxl::read_xlsx(filename_tool, sheet = "choices", col_types = "text") %>%
    dplyr::select(dplyr::all_of(c("list_name", "name")), !!rlang::sym(label_colname))
  tool.choices <- tool.choices[!is.na(tool.choices$list_name),] %>%
    dplyr::distinct() %>%
    as.data.frame()
  return(tool.choices)
}



#' Load the 'survey' tab from a Kobo tool.
#'
#' @param filename_tool This is the path to the file that contains the tool
#' @param label_colname This is the variable of the label colname
#' @param keep_cols Whether all columns from the original tool should be kept. By default it is False, meaning that only the relevant label, hints, etc are kept
#'
#' @return A dataframe of the survey from the kobo tool, with new columns added: `datasheet`, `q.type`, and `list_name`
#' @export
#'
#' @examples
#' \dontrun{
#' filename_tool <- "tool.xlsx"
#' label_colname <- load.label.colname(filename_tool)
#' tool_survey <- load.tool.survey(filename_tool, label_colname, keep_cols = F)
#' }

load.tool.survey <- function(filename_tool, label_colname,  keep_cols = F){
  tool.survey <- readxl::read_xlsx(filename_tool, sheet = "survey", col_types = "text")
  tool.survey <- tool.survey[!is.na(tool.survey$type),]
  tool.survey <- tool.survey %>%
    dplyr::mutate(q.type=as.character(lapply(type, function(x) stringr::str_split(x, " ")[[1]][1])),
           list_name=as.character(lapply(type, function(x) stringr::str_split(x, " ")[[1]][2])),
           list_name=ifelse(stringr::str_starts(type, "select_"), list_name, NA)) %>%
    as.data.frame()

  if(!keep_cols){
    # select only the relevant (English) labels, hints etc.
    lang_code <- stringr::str_split(label_colname, "::", 2, T)[2]
    lang_code <- stringr::str_replace(stringr::str_replace(lang_code, "\\(", "\\\\("), "\\)", "\\\\)")
    cols <- colnames(tool.survey)
    cols_to_keep <- cols[stringr::str_detect(cols, paste0("((label)|(hint)|(constraint_message)|(required_message))::",lang_code)) |
                           !stringr::str_detect(cols, "((label)|(hint)|(constraint_message)|(required_message))::")]

    tool.survey <- dplyr::select(tool.survey, all_of(cols_to_keep))
  }
  # Find which data sheet question belongs to:
  tool.survey <- tool.survey %>% dplyr::mutate(datasheet = NA)
  sheet_name <- "main"
  for(i in 1:nrow(tool.survey)){
    toolrow <- tool.survey %>% dplyr::slice(i)
    if(stringr::str_detect(toolrow$type, "begin[ _]repeat")) sheet_name <- toolrow$name
    else if(stringr::str_detect(toolrow$type, "end[ _]repeat")) sheet_name <- "main"   # watch out for nested repeats (Why would you even want to do that?)
    else if(stringr::str_detect(toolrow$type, "((end)|(begin))[ _]group", T)) tool.survey[i, "datasheet"] <- sheet_name
  }
  return(tool.survey)
}

