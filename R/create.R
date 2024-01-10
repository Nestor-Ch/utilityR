
#' Create deletion log
#'
#' Creates a deletion log for the provided data and reason.
#'
#' @param data Dataframe containing columns `uuid` and `col_enum`. A subset of `raw.main`, filtered to have the deleted `uuid` `loop_index`
#' @param col_enum The name of the column which contains the enumerator's id in your main dataframe
#' @param reason This is a string describing the reason for removing a survey from data.
#' @param is.loop Whether the `data` argument is a loop or not
#' @param data.main If the `data` argument is a loop, you'll need to provide the main dataframe too
#'
#' @note The provision of `data.main` in the case of using `is.loop` = T is needed because we're pulling up the `col_enum`
#' from the main dataframe.
#' @export
#' @return A dataframe containing a deletion log with columns `uuid`, `col_enum`, `reason`, OR an empty dataframe if `data` has 0 rows.
#'
#' @examples
#' \dontrun{
#' # for non-loops
#' create.deletion.log(data = data, col_enum = 'enum_col', reason = 'bad entry', is.loop = F)
#' # for loops
#' create.deletion.log(data = data_loop, col_enum = 'enum_col', reason = 'bad entry', is.loop = T, data.main = data)
#' }

create.deletion.log <- function(data, col_enum, reason, is.loop = F, data.main = NULL){

  if(!'uuid' %in% colnames(data)){
    stop("Your data doesn't have a uuid column, please add it and re-run the script")
  }

  if(!is.loop && "loop_index" %in% colnames(data)){
    warning("Parameter is.loop is = False, but data contains column 'loop_index'. It will be assumed that this data is, actually, a loop!\n
              N.B.: for the future, you can use apply.changes without the is.loop parameter.\n")
    is.loop <- T
  }else if(is.loop && (!"loop_index" %in% colnames(data))) {
    stop("Parameter is.loop is = True, but data does not contain column 'loop_index'!\n")
  }

  if(is.loop & is.null(data.main)){
    stop("Your data is a loop but you haven't provided the main dataframe. Please enter the data.main parameter")
  } else if(is.loop & !is.null(data.main)){

    data <- data %>% dplyr::left_join(data.main %>% dplyr::select(uuid, !!rlang::sym(col_enum)),by = dplyr::join_by(uuid))

  }

  if(!col_enum %in% colnames(data)){
    stop(paste0("\nEnumerator column (", col_enum, ") not found in the data!\nPossible matches for enum_col:\n",
                paste0(agrep('enum', colnames(data), max.distance = 1, value = T),collapse = '\n')))
  }

  if(nrow(data) > 0){
    # if it's a loop, then include the loop_index in the deletion log
    if("loop_index" %in% colnames(data)){
      data <- data %>%
        dplyr::select(uuid, loop_index, any_of(col_enum)) %>%
        dplyr::rename('col_enum' = dplyr::all_of(col_enum))
    }else{
      data <- data %>%
        dplyr::select(uuid, any_of(col_enum))%>%
        dplyr::rename('col_enum' = dplyr::all_of(col_enum))
    }
    return(data %>%
             dplyr::mutate(reason=reason))
  }else return(data.frame())
}



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


#' Format a translate responses file
#'
#' Format a dataframe containing responses to prepare for other/translate requests
#'
#' Relocates columns and adds the TEI columns.
#'
#' @param responses.j Dataframe containing responses to any questions from `questions.db`
#' @param response_colname String containing name of the column which has the relevant response (tanslated to English in most cases) unused
#'
#' @return Returns a formatted recode.others dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' create.translate.requests(responses.j = responses.j, response_colname = "response.en")
#' }
create.translate.requests <- function(responses.j, response_colname = "response.en"){

  relevant_colnames <- c("uuid", "loop_index", "name", "ref.name","full.label","ref.type","ref.response", "choices.label", "choice", "today")

  response_cols <- colnames(responses.j)[stringr::str_starts(colnames(responses.j), "response")]
  relevant_colnames <- append(relevant_colnames, response_cols)
  responses.j <- responses.j %>%
    dplyr::select(any_of(relevant_colnames)) %>%
    dplyr::relocate( dplyr::all_of(response_cols), .after = last_col()) %>%
    dplyr::mutate("TRUE other (provide a better translation if necessary)"=NA,
           "EXISTING other (copy the exact wording from the options in column choices.label)"=NA,
           "INVALID other (insert yes or leave blank)"=NA) %>%
    dplyr::arrange(name, !!rlang::sym(response_cols[which(response_cols == response_colname)])) %>%
    dplyr::tibble()

  return(responses.j)
}





