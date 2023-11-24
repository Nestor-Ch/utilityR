#' Save other requests file
#'
#' @param df your other requests dataframe
#' @param wb_name The name you'd like your excel file to have
#' @param use_template Whether the function should use pre-established template. `False` by default
#' @param directory The directory that holds your other requests
#' @param template_dir The directory where you've saved your pre-established template if using.
#' "resources/other_requests_template.xlsx" by default
#'
#' @return Exports a dataframe of other requests into your requests folder
#' @export
#'
#' @examples
#' \dontrun{
#' save.other.requests(df = data, wb_name = 'other_requests', directory = 'output/checking/requests/')
#' }
save.other.requests <- function(df, wb_name, use_template = F, directory,
                                template_dir="resources/other_requests_template.xlsx"){

  if(use_template){
    wb <- openxlsx::loadWorkbook(template_dir)
  }else{
    wb <-  openxlsx::createWorkbook()
  }

  # styles
  style.col.blue <- openxlsx::createStyle(fgFill="#CCE5FF", valign="top",
                                          border="TopBottomLeftRight",
                                          borderColour="#000000", wrapText=T)

  style.col.green <- openxlsx::createStyle(fgFill="#E5FFCC", border="TopBottomLeftRight",
                                           borderColour="#000000",valign="top",
                                           fontSize = 10, fontName = "Arial Narrow", wrapText=T)

  style.col.green.bold <- openxlsx::createStyle(textDecoration="bold", fgFill="#E5FFCC",
                                                valign="top",border="TopBottomLeftRight",
                                                borderColour="#000000",fontSize = 10,
                                                fontName = "Arial Narrow", wrapText=T)


  openxlsx::addWorksheet(wb, "Sheet2", zoom = 90)
  openxlsx::writeData(wb = wb, x = df, sheet = "Sheet2", startRow = 1,
                      headerStyle = openxlsx::createStyle(textDecoration="bold", border = "Bottom", fontName = "Arial"))

  response_cols_ind <- which(stringr::str_starts(colnames(df), "response"))
  for(i in response_cols_ind){
    openxlsx::addStyle(wb, "Sheet2",
                       style = openxlsx::createStyle(fontSize = 10, fontName = "Arial Narrow", wrapText = T),
                       rows = 1:nrow(df)+1, cols=i)
    openxlsx::setColWidths(wb, "Sheet2", cols = i, widths = 30)
  }
  openxlsx::addStyle(wb, "Sheet2",
                     style = openxlsx::createStyle(fontSize = 10, fontName = "Arial Narrow", wrapText = T),
                     rows = 1:nrow(df)+1, cols=which(colnames(df) == "choices.label"))
  openxlsx::addStyle(wb, "Sheet2",
                     style = openxlsx::createStyle(fontSize = 11, wrapText = T),
                     rows = 1:nrow(df)+1, cols=which(colnames(df) == "full.label"))

  openxlsx::setColWidths(wb, "Sheet2", cols = 1, widths = 5)
  openxlsx::setColWidths(wb, "Sheet2", cols = 2:which(colnames(df) == "choices.label")-1, widths = "auto")
  openxlsx::setColWidths(wb, "Sheet2", cols = which(colnames(df) == "choices.label"), widths = 50)
  openxlsx::setColWidths(wb, "Sheet2", cols = which(colnames(df) == "full.label"), widths = 30)
  openxlsx::setColWidths(wb, "Sheet2", cols = (ncol(df)-4):(ncol(df)), widths = 35)

  openxlsx::addStyle(wb, "Sheet2", style = style.col.green, rows = 1:(nrow(df)+1), cols = ncol(df)-2, stack = T)
  openxlsx::addStyle(wb, "Sheet2", style = style.col.green, rows = 1:(nrow(df)+1), cols = ncol(df)-1, stack = T)
  openxlsx::addStyle(wb, "Sheet2", style = style.col.green, rows = 1:(nrow(df)+1), cols = ncol(df), stack = T)
  openxlsx::addStyle(wb, "Sheet2", style.col.green.bold, rows = 1, cols = ncol(df)-2, stack = T)
  openxlsx::addStyle(wb, "Sheet2", style.col.green.bold, rows = 1, cols = ncol(df)-1, stack = T)
  openxlsx::addStyle(wb, "Sheet2", style.col.green.bold, rows = 1, cols = ncol(df), stack = T)

  filename <- paste0(directory, wb_name, ".xlsx")
  openxlsx::saveWorkbook(wb, filename, overwrite=TRUE)

}




#' Save translation requests
#'
#' @param df your other requests dataframe
#' @param wb_name The name you'd like your excel file to have
#' @param blue_cols If you'd like for any columns to be blue - specify which ones here
#' @param use_template Whether the function should use pre-established template. `False` by default
#' @param directory The directory that holds your other requests
#' @param template_dir directory where you've saved your pre-established template if using.
#' "resources/other_requests_template.xlsx" by default
#'
#' @return Exports a dataframe of translation requests into your requests folder
#' @export
#'
#' @examples
#' \dontrun{
#' save.trans.requests(df = data, wb_name = 'other_requests', directory = 'output/checking/requests/')
#' }
save.trans.requests <- function(df, wb_name, blue_cols = NULL, use_template = F,directory,
                                template_dir = "resources/trans_requests_template.xlsx"){


  # styles
  style.col.blue <- openxlsx::createStyle(fgFill="#CCE5FF", valign="top",
                                          border="TopBottomLeftRight",
                                          borderColour="#000000", wrapText=T)

  style.col.green <- openxlsx::createStyle(fgFill="#E5FFCC", border="TopBottomLeftRight",
                                           borderColour="#000000",valign="top",
                                           fontSize = 10, fontName = "Arial Narrow", wrapText=T)

  style.col.green.bold <- openxlsx::createStyle(textDecoration="bold", fgFill="#E5FFCC",
                                                valign="top",border="TopBottomLeftRight",
                                                borderColour="#000000",fontSize = 10,
                                                fontName = "Arial Narrow", wrapText=T)


  if(use_template){
    wb <- openxlsx::loadWorkbook(template_dir)
  }else{
    wb <- openxlsx::createWorkbook()
  }

  # remove the useless "EXISTING" column, and the word 'other':
  if(length(df %>% dplyr::select(dplyr::starts_with("EXISTING")) %>% names) > 0){
    df <- df %>% dplyr::select(-dplyr::starts_with("EXISTING"))
    colnames(df) <- gsub("other ", "", colnames(df))
  }
  df <- df %>% dplyr::select(-starts_with("ref")) %>% dplyr::select(-starts_with("choices"))

  openxlsx::addWorksheet(wb, "Sheet2")
  openxlsx::writeData(wb = wb, x = df, sheet = "Sheet2", startRow = 1)

  openxlsx::setColWidths(wb, "Sheet2", cols = 1, widths = 5)
  openxlsx::setColWidths(wb, "Sheet2", cols = 2:ncol(df), widths = "auto")

  response_cols_ind <- which(stringr::str_starts(colnames(df), "response"))
  for(i in append(response_cols_ind, 1)){
    openxlsx::addStyle(wb, "Sheet2", style = openxlsx::createStyle(fontSize = 10, fontName = "Arial Narrow", wrapText = T),
             rows = 1:nrow(df)+1, cols=i)
    openxlsx::setColWidths(wb, "Sheet2", cols = i, widths = 30)
  }
  for (col in blue_cols) {
    i <- grep(paste0('^',col,'$'), colnames(df))
    if(length(i) == 0){
      stop(paste(col,"not found in df!"))}
    openxlsx::addStyle(wb, "Sheet2", style = style.col.blue, rows = 1:(nrow(df)+1), cols = i, stack = T)
    openxlsx::setColWidths(wb, "Sheet2", cols = which(colnames(df) == col), widths = 20)
  }

  openxlsx::addStyle(wb, "Sheet2", style = openxlsx::createStyle(textDecoration="bold", valign = "bottom"), rows = 1, cols=1:ncol(df), stack = T)

  openxlsx::addStyle(wb, "Sheet2", style = style.col.green, rows = 1:(nrow(df)+1), cols = which(stringr::str_starts(colnames(df), "TRUE")), stack = T)
  openxlsx::addStyle(wb, "Sheet2", style.col.green.bold, rows = 1, cols = which(stringr::str_starts(colnames(df), "TRUE")), stack = T)
  openxlsx::addStyle(wb, "Sheet2", style = style.col.green, rows = 1:(nrow(df)+1), cols = which(stringr::str_starts(colnames(df), "INVALID")), stack = T)
  openxlsx::addStyle(wb, "Sheet2", style.col.green.bold, rows = 1, cols = which(stringr::str_starts(colnames(df), "INVALID")), stack = T)

  filename <- paste0(directory, wb_name, ".xlsx")
  openxlsx::saveWorkbook(wb, filename, overwrite=TRUE)
}

#' Save follow up requests
#'
#' Alias for create.follow.up.requests
#'
#' @param checks.df The checks file for the follow-up requests
#' @param directory The directory for your follow-up request
#' @param wb_name The name of your workbook
#'
#' @return Write a follow-up requests file
#' @export
#'
#' @examples
#' \dontrun{
#' save.follow.up.requests(checks.df = data, wb_name = 'other_requests', directory = 'output/checking/requests/')
#' }
save.follow.up.requests <- function(checks.df, directory,wb_name){
  create.follow.up.requests(checks.df, directory,wb_name)
  }



