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





#' Load 'request' logs from specified directory.
#'
#' Searches `dir` to find XLSX files with names that start with a match for `filename.pattern`.
#' NB: This pattern-matching is case-insensitive. If files contain the classic "TRUE", "EXISTING" or "INVALID" (TEI) columns,
#' these will be renamed to "true.v", "existing.v", and "invalid.v" respectively and optionally validated for errors.

#' @param dir Directory which should be searched for files.
#' @param filename.pattern String with a regex pattern which will be passed to `list.files` to match files,
#' however: '^' (string start) is added at the start of the pattern, and ".*\\.xlsx" is added at the end,
#' so effectively files that will be loaded must be XLSX and have names that start with the provided pattern.
#'
#' @param sheet Optional parameter passed to `read_xlsx`, defaults to NULL (first sheet of an Excel workbook)
#' @param validate Should the file be validated (make sure that only one of TEI columns is filled.)
#'
#' @return Your request file in the tibble format
#' @export
#'
#' @examples
#' \dontrun{
#' load.requests(dir = 'your_path', filename.pattern = 'your_pattern')
#' }
load.requests <- function(dir, filename.pattern, sheet=NULL, validate=FALSE){
  file.type <-  stringr::str_squish(stringr::str_replace_all(filename.pattern, "[^a-zA-Z]+"," "))
  filenames <- list.files(dir, recursive=FALSE, full.names=TRUE, ignore.case = TRUE,
                          pattern=paste0("^",filename.pattern,".*\\.xlsx"))
  if (length(filenames) == 0){
    warning(paste("Files with",file.type,"requests not found!"))
    return(dplyr::tibble())
  } else {
    cat(paste("Loading",length(filenames),file.type,"files:\n"),paste(filenames, collapse = "\n "),"\n")
    res <- data.frame()
    for (filename in filenames){
      # load file
      other <- readxl::read_xlsx(filename, col_types = "text", trim_ws = T, sheet = sheet)
      if (filename==filenames[1]){
        res <- other
      }else{
        if(ncol(res)!=ncol(other)) warning("Number of columns differs between files! Check them to make sure everything is correct, please!")
        res <- dplyr::bind_rows(res, other)
      }

    }
    # rename: TRUE -> true.v, EXISTING -> existing.v, INVALID -> invalid.v
    c_tei_cols <- c("true", "existing", "invalid")
    for(c in c_tei_cols) {
      colnames(res)[stringr::str_starts(colnames(res), stringr::str_to_upper(c))] <- paste0(c,'.v')
    }
    if(validate){
      c_tei_cols <- paste0(c_tei_cols, ".v")
      if(all(c_tei_cols %in% colnames(res))){
        res <- res %>% dplyr::mutate(check = rowSums(is.na(dplyr::select(res, dplyr::all_of(c_tei_cols)))))
        check.res <- res %>% dplyr::select(c(
          dplyr::any_of(c("uuid","ref.type","check"))))
        check.missing <- check.res %>% dplyr::filter(check == 3)
        if(nrow(check.missing)){
          warning(paste0("Missing entries:\n", paste0(" ", unlist(check.missing[,1]) , collapse = "\n")))
        }
        if("ref.type" %in% colnames(check.res)){
          check.res <- check.res %>%
            dplyr::filter(check == 0 | (check == 1 & ref.type != "select_multiple"))  # select_multiple can have 2 columns selected
        }else{
          check.res <- dplyr::filter(check.res, check != 2)
        }
        if(nrow(check.res)>0) {
          warning(paste0("Multiple columns selected:\n", paste0(" ", unlist(check.res[,1]) , collapse = "\n")))
        }
      }else{
        stop("One or more of 'true', 'existing', 'invalid' columns not found in requests files.")
      }
    }
    return(res)
  }
}



#' Load Audit files
#'
#' Loads all audit files from a selected folder into 1 large dataframe
#'
#' @param dir.audits  The directory in which to look for audit files (path resembling .../data/audits/...)
#' @param uuids The uuids of surveys that are to be loaded. If NULL is provided here (and by default) all uuids from dir.audits will be loaded.
#' @param track.changes Whether the survey has the parameter track-changes set to `true`
#' @param add.uuid2 Whether the function should add uuid2. If TRUE, this'll allow the user a better control over error messages in proccess uuid
#'
#' @return Returns a dataframe with contents of all `audit.csv` files from `dir.audits` or its subdirectories.
#' @export
#'
#' @examples
#' \dontrun{
#' load.audit.files(dir.audits = 'your_path', uuids = data$uuid)
#' }
load.audit.files <- function(dir.audits, uuids=NULL, track.changes=F, add.uuid2=T){

  audit.filenames <- list.files(dir.audits, pattern="audit.csv", recursive=TRUE, full.names=TRUE)
  cat("Loading audit logs from",dir.audits,"...\n")

  counter <- 0
  res <- data.frame()
  for (filename in audit.filenames){
    # get uuid from filename
    sp <- stringr::str_split(filename, "\\/")[[1]]  # could throw an error on Unix?
    uuid <- sp[length(sp)-1]
    if(is.null(uuids) | uuid %in% uuids){
      # load file
      audit <- readr::read_csv(filename, show_col_types = FALSE, locale = readr::locale(encoding = "UTF-8")) %>%
        dplyr::mutate(uuid=uuid, .before=1)

      # TODO: make sure that the below is correctly done (probably not)
      if(track.changes & "old-value" %in% colnames(audit)) {
        audit <- audit %>%
          dplyr::rename("old.value" = `old-value`, "new.value" = `new-value`)
      } else {
        audit <- audit %>%
          dplyr::mutate(old.value = NA, new.value = NA)
      }
      if(add.uuid2==T){
        audit$uuid2 = uuid
      }
      audit <- audit %>%
        dplyr::mutate(across(dplyr::ends_with('value'),~ as.character(.x))) %>%
        dplyr::mutate(across(dplyr::ends_with('value'),~ ifelse(.x == '' |.x == ' '|.x == '\n',NA_character_, .x )))
      counter <- counter + 1
      res <- dplyr::bind_rows(res, audit)
      cat("...")
    }
  }
  if(nrow(res) > 0){
    res <- res  %>%
      dplyr::group_by(uuid) %>%
      dplyr::mutate(inter_q_duration = (start-dplyr::lag(end))/1000) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(duration=(end-start)/1000,
             group=sapply(stringr::str_split(node, '\\/'), function(x){
               id.group <- ifelse("G_survey" %in% x, 4, 3)
               return(x[id.group])}),
             question=sapply(stringr::str_split(node, '\\/'), function(x){return(x[length(x)])})) %>%
      dplyr::mutate(event=stringr::str_replace_all(event, " ", ".")) %>%
      dplyr::tibble()

    # if there are large differences


    cat("\n...Done\n")
    cat(paste("Loaded", counter, "audit logs.\n"))
  }else{
    warning("No relevant audit logs found!")
  }
  return(res)
}



