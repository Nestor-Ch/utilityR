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


