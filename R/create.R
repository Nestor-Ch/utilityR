#' Create a follow-up requests file
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
#' create.follow.up.requests(checks.df = data, wb_name = 'other_requests', directory = 'output/checking/requests/')
#' }
create.follow.up.requests <- function(checks.df, directory,wb_name){
  use.color <- function(check.id){
    return(stringr::str_starts(check.id, "0"))
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


  # define styles
  style.col.green <- openxlsx::createStyle(fgFill="#E5FFCC", border="TopBottomLeftRight", borderColour="#000000")
  style.col.green.first <-  openxlsx::createStyle(textDecoration="bold", fgFill="#E5FFCC",
                                                  border="TopBottomLeftRight", borderColour="#000000", wrapText=F)
  col.style <-  openxlsx::createStyle(textDecoration="bold", fgFill="#CECECE",halign="center",
                                      border="TopBottomLeftRight", borderColour="#000000")
  # arrange cleaning.log so that colors are properly assigned later
  cl <- checks.df %>%
    dplyr::arrange(variable) %>%
    dplyr::group_modify(~ rbind(
      dplyr::filter(.x, !use.color(check.id)) %>%
        dplyr::arrange(check.id, uuid),
      dplyr::filter(.x, use.color(check.id)) %>%
        dplyr::arrange(check.id)))
  cl <- cl %>%
    dplyr::arrange(match(check.id, stringr::str_sort(unique(cl$check.id), numeric=T)))
  # save follow-up requests
  wb <-  openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Follow-up", zoom = 90)
  openxlsx::writeData(wb = wb, x = cl, sheet = "Follow-up", startRow = 1)

  openxlsx::addStyle(wb, "Follow-up", style = style.col.green, rows = 1:(nrow(cl)+1), cols=which(colnames(cl)=="explanation"))
  openxlsx::addStyle(wb, "Follow-up", style = style.col.green, rows = 1:(nrow(cl)+1), cols=which(colnames(cl)=="invalid"))
  openxlsx::addStyle(wb, "Follow-up", style = style.col.green, rows = 1:(nrow(cl)+1), cols=which(colnames(cl)=="new.value"))
  openxlsx::addStyle(wb, "Follow-up", style = style.col.green.first, rows = 1, cols=which(colnames(cl)=="explanation"))
  openxlsx::addStyle(wb, "Follow-up", style = style.col.green.first, rows = 1, cols=which(colnames(cl)=="new.value"))
  openxlsx::addStyle(wb, "Follow-up", style = style.col.green.first, rows = 1, cols=which(colnames(cl)=="invalid"))


  openxlsx::setColWidths(wb, "Follow-up", cols=1:ncol(cl), widths="auto")
  # setColWidths(wb, "Follow-up", cols=ncol(cl)-1, widths=50)

  openxlsx::setColWidths(wb, "Follow-up", cols=which(colnames(cl)=="issue"), widths=50)
  openxlsx::addStyle(wb, "Follow-up", style = openxlsx::createStyle(wrapText=T), rows = 1:(nrow(cl)+1), cols=which(colnames(cl)=="issue"))

  openxlsx::addStyle(wb, "Follow-up", style = col.style, rows = 1, cols=1:ncol(cl))

  col.id <- which(colnames(cl)=="old.value")
  if(nrow(cl) > 0){
    random.color <- ""
    for (r in 2:nrow(cl)){
      if((!use.color(as.character(cl[r, "check.id"])) &
          as.character(cl[r, "uuid"])==as.character(cl[r-1, "uuid"]) &
          as.character(cl[r, "check.id"])==as.character(cl[r-1, "check.id"])) |
         (use.color(as.character(cl[r, "check.id"])) &
          as.character(cl[r, "check.id"])==as.character(cl[r-1, "check.id"]))){
        if (random.color == ""){
          random.color <- randomcoloR::randomColor(1, luminosity = "light")
          openxlsx::addStyle(wb, "Follow-up",
                             style = openxlsx::createStyle(fgFill=random.color, wrapText=T),
                             rows = r:(r+1), cols=col.id)
        }
      } else {random.color=""}
    }
  }

  filename <- paste0(directory, wb_name)
  filename <- ifelse(stringr::str_ends(filename, "\\.xlsx", T), paste0(filename, ".xlsx"), filename)
  openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
}
