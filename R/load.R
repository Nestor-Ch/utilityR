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
#' label_colname <- load.label_colname(filename_tool, language = "English")
#' }
load.label_colname <- function(filename_tool, language = "English"){
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
#' tool_choices <- load.tool.choices(filename_tool)
#' }

load.tool.choices <- function(filename_tool, label_colname){
  df <- readxl::read_xlsx(filename_tool, sheet = "choices", col_types = "text") %>%
    dplyr::select(dplyr::all_of(c("list_name", "name")), !!rlang::sym(label_colname))
  df <- df[!is.na(df$list_name),]
  return(df)
}
