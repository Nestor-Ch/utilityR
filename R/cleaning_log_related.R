# ------------------------------------ Recode - related functions -------------------------------------------



#' Apply changes to main data basing on a cleaning log.
#'
#' Outputs warnings if uuids, loop indeces, or variables from `clog` are not found in `data`. Or if old.value doesn't match what is present in the data
#' Be aware: all values will be written to data as character.
#'
#' @param data Data (raw.main or raw.loop)
#' @param clog Cleaning log - dataframe containing columns uuid, loop_index, variable, new.value, old.value
#' @param is.loop Obsolete. This function automatically guesses whether the data contains column 'loop_index'. But there will be warnings produced just in case.
#' @param print_debug If True (any by default), warnings related to value mismatches and informational messages about the number of changes will be printed.
#'
#' @return Dataframe containing data with applied changes
#' @export
#' @examples
#' \dontrun{
#' apply.changes(data, clog = c_log, is.loop = F,print_debug = T)
#' }
apply.changes <- function(data, clog, is.loop, print_debug = T){
  if(!is.loop && "loop_index" %in% colnames(data)){
    warning("Parameter is.loop is = False, but data contains column 'loop_index'. It will be assumed that this data is, actually, a loop!\n
              N.B.: for the future, you can use apply.changes without the is.loop parameter.\n")
    is.loop <- T
  }else if(is.loop && (!"loop_index" %in% colnames(data))) {
    stop("Parameter is.loop is = True, but data does not contain column 'loop_index'!\n
              N.B.: for the future, you can use apply.changes without the is.loop parameter.\n")
  }

  if(!is.loop && ("loop_index" %in% colnames(clog))){
    clog <- dplyr::filter(clog, is.na(loop_index))
  }else if(is.loop)
    clog <- dplyr::filter(clog, !is.na(loop_index))
  if(nrow(clog) == 0){
    stop("No changes to be applied (cleaning log empty)")
  }
  else{
    missinguuids <- c()
    missingloop_indexs <- c()
    missingvars <- c()
    changes_counter <- 0
    for (r in 1:nrow(clog)){
      variable <- as.character(clog$variable[r])
      if(!variable %in% colnames(data)) {
        missingvars <- append(missingvars, variable)
        next
      }
      if(is.loop){
        loop_index <- as.character(clog$loop_index[r])
        if(!loop_index %in% data$loop_index){
          missingloop_indexs <- append(missingloop_indexs, loop_index)
          next
        }
        if(print_debug && data[data$loop_index == loop_index, variable] %not=na% clog$old.value[r]){
          warning(paste0("Value in data is different than old.value in Cleaning log!\nloop_index: ", loop_index,
                         "\tVariable: ", variable,
                         "\tExpected: ", clog$old.value[r], "\t found: ", data[data$loop_index == loop_index, variable],
                         "\tReplacing with: ", clog$new.value[r]))
        }
        data[data$loop_index == loop_index, variable] <- as.character(clog$new.value[r])
        changes_counter <- changes_counter + 1
      }else {
        uuid <- as.character(clog$uuid[r])
        if(!uuid %in% data$uuid) {
          missinguuids <- append(missinguuids, uuid)
          next
        }
        if(print_debug && data[data$uuid == uuid, variable] %not=na% clog$old.value[r]){
          warning(paste0("Value in data is different than old.value in Cleaning log!\nUUID: ", uuid,
                         "\tVariable: ", variable,
                         "\tExpected: ", clog$old.value[r], "\t found: ", data[data$uuid == uuid, variable],
                         "\tReplacing with: ", clog$new.value[r]))
        }
        data[data$uuid == uuid, variable] <- as.character(clog$new.value[r])
        changes_counter <- changes_counter + 1
      }
    }
    if(print_debug && length(missinguuids > 0)) warning(paste0("uuids from cleaning log not found in data:\n\t", paste0(missinguuids, collapse = "\n\t")))
    if(print_debug && length(missingloop_indexs) > 0) warning(paste0("loop_indexes from cleaning log not found in data:\n\t", paste0(missingloop_indexs, collapse = "\n\t")))
    if(print_debug && length(missingvars > 0))  warning(paste0("variables from cleaning log not found in data:\n\t", paste0(missingvars, collapse = "\n\t")))
    if(print_debug) cat("\tMade", changes_counter, "changes to the data.\n")
  }
  return(data)
}



#' Undo the things that you've changed in your data
#'
#' Provide the same cleaning log as the one that you used to apply the changes and voila...
#'
#' this function will undo the changes and rewind your data to the previous state! as long as you didn't modify the cleaning log since you applied the changes!
#' This function basically flips old.value and new.value around in the clog, and applies changes again.
#'
#' @param data your dataframe with applied changes
#' @param clog your cleaning log
#' @param is.loop whether the changes were applied to the loop
#' @export
#' @return an old version of your dataframe
#'
#' @examples
#' \dontrun{
#' undo.changes(data, clog = c_log)
#' }
undo.changes <- function(data, clog, is.loop){
  clog <- clog %>%
    dplyr::mutate(temp = new.value) %>%
    dplyr::mutate(new.value = old.value,
                  old.value = temp)
  return(data %>% apply.changes(clog, print_debug = T,is.loop=is.loop))
}


#' Create a logical check DF
#'
#' @param check Dataframe with data, filtered according to some flag (variable + uuid/loop_index filter).
#' Must contain columns `uuid` and all columns in `question.names`
#' @param id The identifier of this logical check.
#' @param question.names List of relevant queston names for this logical check.
#' @param issue description of the issue encountered
#' @param cols_to_keep  List of columns from raw.main to be included in result.
#' @param is.loop Obsolete. This function automatically guesses whether the data contains column 'loop_index'.
#' But there will be warnings produced just in case.
#' @param date If you want to include the `date` column, you can specify it's name in here
#' @export
#' @return Dataframe containing at the least columns: `uuid`, `check.id`, `variable`, `issue`, `old.value`, `new.value`, `explanation`.
#' This object can be later added to cleaning log.
#'
#' @examples
#' \dontrun{
#' make.logical.check.entry(check = data, id = 1, question_names = c('b11_gas_heating_price','b10_gas_vehicle_price'), issue = 'test', is.loop = F)
#' }


make.logical.check.entry <- function(check, id, question.names, issue, cols_to_keep = c(), is.loop , date = NA){
  res <- data.frame()

  if(!is.loop && "loop_index" %in% colnames(check)){
    warning("Parameter is.loop is = False, but check contains column 'loop_index'. It will be assumed that this data is, actually, a loop!\n
              N.B.: for the future, you can use apply.changes without the is.loop parameter.\n")
    is.loop <- T
  }else if(is.loop && (!"loop_index" %in% colnames(check))) {
    stop("Parameter is.loop is = True, but check does not contain column 'loop_index'!\n
              N.B.: for the future, you can use apply.changes without the is.loop parameter.\n")
  }

  if(is.loop){
    cols_to_keep <- append("loop_index", cols_to_keep)
  }


  for(q.n in question.names){
    new.entries <- check %>%
      dplyr::mutate(variable = q.n,
                    issue=issue,
                    old.value =!!rlang::sym(q.n),
                    new.value = NA,
                    invalid = NA,
                    explanation = NA)

    new.entries[["check.id"]] <- id

    if(!is.na(date)){
      cols_to_keep <- c(cols_to_keep, 'survey.date')
      new.entries['survey.date'] = check[,date]
    }

    new.entries <- new.entries %>%
      dplyr::select(dplyr::all_of(c(cols_to_keep, "uuid","check.id", "variable", "issue",
                                    "old.value", "new.value", "invalid", "explanation"))) %>%
      dplyr::relocate(uuid) %>%
      dplyr::mutate_all(as.character)

    res <- rbind(res, new.entries)
  }

  return(res %>%
           dplyr::arrange(uuid))
}



#' add.to.cleaning.log.other.remove
#'
#' The function that takes the filled out other.requests and creates a cleaning log the changes needed for the `invalid` entries
#'
#' @param data your dataframe
#' @param other_requests  filled out other.requests file (only invalid == T rows) #add a check for this
#' @param is.loop whether the data is a loop
#' @param issue the reason that the entries were deemed invalid for the cleaning log entry, "Invalid other response" by default
#' @export
#' @return a filled out cleaning log file for the given entry
#' @note this function is not vectorized - hence it takes on only 1 row of `other_requests` at a time.
#' If you want to run it over a column, please use `vectorized.add.to.cleaning.log.other.remove`. Just make sure to provide the correct inputs
#'
#'
#' @examples
#' \dontrun{
#' add.to.cleaning.log.other.remove(data = data, other_requests = other.requests, is.loop = F)
#' }

add.to.cleaning.log.other.remove <- function(data, other_requests, is.loop, issue = "Invalid other response"){


  if(!is.loop && "loop_index" %in% colnames(data)){
    warning("Parameter is.loop is = False, but data contains column 'loop_index'. It will be assumed that this data is, actually, a loop!")
    is.loop <- T
  }else if(is.loop && (!"loop_index" %in% colnames(data))) {
    stop("Parameter is.loop is = True, but data does not contain column 'loop_index'!\n")
  }


  if(length(setdiff(other_requests$name,colnames(data)))>0){
    stop(paste0(
      'One of the entries in your name column in the other_requests file is not present in the columns of the dataframe:\n',
      paste0(setdiff(other_requests$name,colnames(data)), collapse = '\n')
    ))

  }

  # uniqui will allow us to filter with 1 smooth function

  if(is.loop){
    data$uniqui <- data$loop_index
    other_requests$uniqui <- other_requests$loop_index
  }else{
    data$uniqui <- data$uuid
    other_requests$uniqui <- other_requests$uuid
  }

  old.response <- data %>%
    dplyr::filter(uniqui %in% other_requests$uniqui) %>%
    dplyr::pull(!!rlang::sym(other_requests$name))

  clog <- data.frame()

  # remove text of the response
  df <- data.frame(uuid=other_requests$uuid, variable=other_requests$name, issue=issue,
                   old.value=old.response, new.value=NA)


  clog <- rbind(clog, df)

  # remove relative entries
  if (other_requests$ref.type[1]=="select_one"){
    old.value <- "other"
    df <- data.frame(uuid=other_requests$uuid, variable=other_requests$ref.name, issue=issue, old.value=old.value, new.value=NA)
    clog <- rbind(clog, df)

  }
  if (other_requests$ref.type[1]=="select_multiple"){
    old.value <- as.character(data[data$uniqui==other_requests$uniqui[1], other_requests$ref.name])
    l <- stringr::str_split(old.value, " ")[[1]]
    new.value <- paste(l[l!="other"], collapse=" ")
    new.value <- ifelse(new.value=="", NA, new.value)
    df <- data.frame(uuid=other_requests$uuid, variable=other_requests$ref.name, issue=issue, old.value=old.value, new.value=new.value)

    clog <- rbind(clog, df)
    if (is.na(new.value)){
      # set all choices columns to NA
      cols <- colnames(data)[stringr::str_starts(colnames(data), paste0(other_requests$ref.name, "/"))]
      oldvalues <- data %>%
        dplyr::filter(uniqui == other_requests$uniqui) %>%
        dplyr::select(dplyr::all_of(cols)) %>%
        unlist() %>%
        unname()
      df <- data.frame(uuid=other_requests$uuid, variable=cols, issue=issue, old.value=oldvalues, new.value=NA)
      clog <- rbind(clog, df)
    } else{
      df <- data.frame(uuid=other_requests$uuid, variable=paste0(other_requests$ref.name, "/other"), issue=issue,
                       old.value="1", new.value="0")
      clog <- rbind(clog, df)
    }
  }

  if(is.loop==T){
    clog$loop_index = other_requests$loop_index
  }else{
    clog$loop_index = NA
  }

  clog <- dplyr::relocate(clog, loop_index, .after = uuid)
  return(clog)
}


#' Vectorized add.to.cleaning.log.other.remove
#'
#' Allows the user to iterate over their `other_requests` file and produce the cleaning log file
#'
#' @param data your dataframe
#' @param other_requests your other_requests - make sure that you're inputing the requests that are available in the `data`
#' If one of the variables in the `name` column of `other_requests` isn't present in the `data` object, it will be removed from the resuls
#' @param issue the reason that the entries were deemed invalid for the cleaning log entry, "Invalid other response" by default
#' @param is.loop Whether your data is a loop
#' @param invalid the name of your invalid column - 'invalid.v' by default
#'
#' @export
#' @return a cleaning log file
#'
#' @examples
#' \dontrun{
#' vectorized.add.to.cleaning.log.other.remove(data = data, other_requests = other.requests)
#' }

vectorized.add.to.cleaning.log.other.remove <-  function(data, other_requests, is.loop, issue = "Invalid other response",
                                                         invalid = 'invalid.v'){

  if(!all(other_requests[[invalid]] == 'yes')){
    stop("Not all of the rows in the invalid column are equal to 'yes', are you sure you're uploading only invalid entries?")
  }

  if(!is.loop && "loop_index" %in% colnames(data)){
    warning("Parameter is.loop is = False, but data contains column 'loop_index'. It will be assumed that this data is, actually, a loop!")
    is.loop <- T
  }else if(is.loop && (!"loop_index" %in% colnames(data))) {
    stop("Parameter is.loop is = True, but data does not contain column 'loop_index'!\n")
  }


  if(length(setdiff(other_requests$name,colnames(data)))>0){
    warning(paste0(
      'Some of the entries in your name column in the other_requests file are not present in the columns of the dataframe:\n',
      paste0(setdiff(other_requests$name,colnames(data)), collapse = '\n'), 'Make sure to run them for the correct dataframe'
    ))

    other_requests <- other_requests[other_requests$name %in% colnames(data),]

  }


  clog <- lapply(1:nrow(other_requests),function(row){
    add.to.cleaning.log.other.remove(data = data,
                                     other_requests=other_requests[row,],
                                     is.loop = is.loop,
                                     issue = issue
    )
  })

  clog <- do.call(rbind, clog)

  return(clog)

}


#' Remove invalid responses to open question
#'
#' @param other_requests - a file with invalid answers to open questions
#' @param issue - why they were deleted
#' @export
#' @return a cleaning log for deletion of responses
#'
#' You can provide it with vectors of invalid answers, it'll work just fine
#'
#' @examples
#' \dontrun{
#' add.to.cleaning.log.trans.remove(other_requests = other.requests)
#' }

add.to.cleaning.log.trans.remove <- function(other_requests,issue = "Invalid response"){

  # remove text of the response
  df <- data.frame(uuid=other_requests$uuid, loop_index= other_requests$loop_index, variable=other_requests$name, issue=issue,
                   old.value=other_requests$response.uk, new.value=NA)

  return(df)
}




#' Add existing select_one choices
#'
#' This function creates a cleaning log for cases where the 'other' response is already present in the
#'select_one choices list
#'
#' @param data The dataframe
#' @param other_requests The other_requests file, filtered to only contain existing response options
#' @param is.loop Whether the data is a loop
#' @param tool.survey The survey sheet of the tool
#' @param tool.choices The choices sheet of the tool
#' @param issue The reason you're recoding this entry, "Recoding other response" by default
#' @param label_colname the column with the choice label in your tool.choices. 'label::English' by default
#' @param existing your 'existing' column - 'existing.v' by default
#'
#' @note This function is not vectorized. It only works 1 row of `other_requests` at a time
#' @export
#' @return a filled out cleaning log file
#'
#' @examples
#' \dontrun{
#' add.to.cleaning.log.other.recode.one(data = data, other_requests = other_requests, is.loop = F,
#' tool.survey = tool.survey, tool.choices=tool.choices)
#' }

add.to.cleaning.log.other.recode.one <- function(data, other_requests, is.loop,
                                                 tool.survey, tool.choices,
                                                 issue = "Recoding other response",
                                                 label_colname = 'label::English',
                                                 existing = 'existing.v'){

  if(!existing %in% colnames(other_requests)){
    stop(paste0('Your other_requests file does not containt the ',existing,' column.
                If your existing column has another name please make sure to write it in the existing argument of this function'))
  }

  if(!is.loop && "loop_index" %in% colnames(data)){
    warning("Parameter is.loop is = False, but data contains column 'loop_index'. It will be assumed that this data is, actually, a loop!")
    is.loop <- T
  }else if(is.loop && (!"loop_index" %in% colnames(data))) {
    stop("Parameter is.loop is = True, but data does not contain column 'loop_index'!\n")
  }

  if(length(setdiff(other_requests$name,colnames(data)))>0){
    stop(paste0(
      'One of the entries in your name column in the other_requests file is not present in the columns of the dataframe:\n',
      paste0(setdiff(other_requests$name,colnames(data)), collapse = '\n')
    ))

  }

  # uniqui will allow us to filter with 1 smooth function

  if(is.loop){
    data$uniqui <- data$loop_index
    other_requests$uniqui <- other_requests$loop_index
  }else{
    data$uniqui <- data$uuid
    other_requests$uniqui <- other_requests$uuid
  }

  clog <- data.frame()

  old.response <- data %>%
    dplyr::filter(uniqui == other_requests$uniqui) %>%
    dplyr::pull(!!rlang::sym(other_requests$name))
  # remove text of the response
  df <- data.frame(uuid=other_requests$uuid, loop_index = other_requests$loop_index,
                   variable=other_requests$name, issue=issue,
                   old.value=old.response, new.value=NA)

  clog <- rbind(clog, df)
  # get list of choices from other response
  if (stringr::str_detect(other_requests[[existing]], ";")) {
    choices <- stringr::str_trim(stringr::str_split(other_requests[[existing]], ";")[[1]])
  } else {
    choices <- stringr::str_trim(stringr::str_split(other_requests[[existing]], "\r\n")[[1]])
  }
  choices <- choices[choices!=""]
  if (length(choices)>1) {
    print(dplyr::select(other_requests, uuid,loop_index, name))
    stop("More than one existing.option for a select_one question")
  }
  # recode choice
  choice <- choices[1]
  list.name <- dplyr::filter(tool.survey, name==other_requests$ref.name[1])$list_name
  new.code <- dplyr::filter(tool.choices, list_name==list.name & !!rlang::sym(label_colname)==choice)
  if (nrow(new.code)!=1) {
    stop(paste0("Choice is not in the list. UUID: ", other_requests$uuid,"; recode.into: ", choice))
  }
  else{
    df <- data.frame(uuid=other_requests$uuid, loop_index = other_requests$loop_index,
                     variable=other_requests$ref.name, issue=issue,
                     old.value="other", new.value=new.code$name)
    clog <- rbind(clog, df)
    return(clog)
  }
}



#' Add existing select_one choices
#' This function creates a cleaning log for cases where the 'other' response is already present in the
#' select_multiple choices list
#'
#' @param data The dataframe
#' @param other_requests The other_requests file, filtered to only contain existing response options
#' @param is.loop Whether the data is a loop
#' @param tool.survey The survey sheet of the tool
#' @param tool.choices The choices sheet of the tool
#' @param issue The reason you're recoding this entry, "Recoding other response" by default
#' @param label_colname the column with the choice label in your tool.choices. 'label::English' by default
#' @param existing your 'existing' column - 'existing.v' by default
#'
#' @note This function is not vectorized. It only works 1 row of `other_requests` at a time
#' @export
#' @return a filled out cleaning log file
#'
#' @examples
#' \dontrun{
#' add.to.cleaning.log.other.recode.multiple(data = data, other_requests = other_requests, is.loop = F,
#' tool.survey = tool.survey, tool.choices=tool.choices)
#' }
add.to.cleaning.log.other.recode.multiple <- function(data, other_requests, is.loop,
                                                      tool.survey, tool.choices,
                                                      issue = "Recoding other response",
                                                      label_colname = 'label::English',
                                                      existing = 'existing.v'){

  if(!existing %in% colnames(other_requests)){
    stop(paste0('Your other_requests file does not containt the ',existing,' column.
                If your existing column has another name please make sure to write it in the existing argument of this function'))
  }

  if(!is.loop && "loop_index" %in% colnames(data)){
    warning("Parameter is.loop is = False, but data contains column 'loop_index'. It will be assumed that this data is, actually, a loop!")
    is.loop <- T
  }else if(is.loop && (!"loop_index" %in% colnames(data))) {
    stop("Parameter is.loop is = True, but data does not contain column 'loop_index'!\n")
  }

  if(length(setdiff(other_requests$name,colnames(data)))>0){
    stop(paste0(
      'One of the entries in your name column in the other_requests file is not present in the columns of the dataframe:\n',
      paste0(setdiff(other_requests$name,colnames(data)), collapse = '\n')
    ))

  }

  if(is.loop){
    data$uniqui <- data$loop_index
    other_requests$uniqui <- other_requests$loop_index
  }else{
    data$uniqui <- data$uuid
    other_requests$uniqui <- other_requests$uuid
  }

  clog <- data.frame()

  old.response <- data %>%
    dplyr::filter(uniqui == other_requests$uniqui) %>%
    dplyr::pull(!!rlang::sym(other_requests$name))
  # remove text of the response
  df <- data.frame(uuid=other_requests$uuid, loop_index = other_requests$loop_index,
                   variable=other_requests$name, issue=issue,
                   old.value=old.response, new.value=NA)
  clog <- rbind(clog, df)
  # get list of choices from other response
  if (stringr::str_detect(other_requests[[existing]], ";")) {
    choices <- stringr::str_trim(stringr::str_split(other_requests[[existing]], ";")[[1]])
  } else {
    choices <- stringr::str_trim(stringr::str_split(other_requests[[existing]], "\r\n")[[1]])
  }
  choices <- choices[choices!=""]
  # set variable/other to "0"
  df <- data.frame(uuid=other_requests$uuid, loop_index = other_requests$loop_index,
                   variable=paste0(other_requests$ref.name, "/other"), issue=issue,
                   old.value="1", new.value="0")
  clog <- rbind(clog, df)
  # get list of choices already selected
  old.value <- as.character(data[data$uniqui==other_requests$uniqui[1], other_requests$ref.name[1]])
  l <- stringr::str_split(old.value, " ")[[1]]
  l.cumulative <- l[l!="other"]
  # add to the cleaning log each choice in the other response
  for (choice in choices){
    # set corresponding variable to "1" if not already "1"
    list.name <- dplyr::filter(tool.survey, name==other_requests$ref.name[1])$list_name
    new.code <- dplyr::filter(tool.choices, list_name==list.name & !!rlang::sym(label_colname)==choice)
    if (nrow(new.code)!=1){
      stop(paste0("Choice is not in the list. UUID: ", other_requests$uuid,"; recode.into: ", choice))
    }
    variable.name <- paste0(other_requests$ref.name, "/", new.code$name)
    if (variable.name %in% colnames(data)){
      old.boolean <- data[[variable.name]][data$uniqui==other_requests$uniqui[1]]
    } else{
      stop(paste("Column", variable.name,"not found in data"))}
    if (old.boolean=="0"){
      df <- data.frame(uuid=other_requests$uuid, loop_index = other_requests$loop_index,
                       variable=variable.name, issue=issue,
                       old.value=old.boolean, new.value="1")
      clog <- rbind(clog, df)
    }
    l.cumulative <- unique(c(l.cumulative, new.code$name))
  }
  # update cumulative variable
  new.value <- stringr::str_squish(paste(sort(l.cumulative), collapse=" "))
  df <- data.frame(uuid=other_requests$uuid, loop_index = other_requests$loop_index,
                   variable=other_requests$ref.name, issue=issue,
                   old.value=old.value, new.value=new.value)
  clog <- rbind(clog, df)
  return(clog)
}




#' Add recoded other requests to the cleaning log
#'
#' @param data The dataframe
#' @param other_requests Other_requests file
#' @param is.loop Whether the dataframe and the request line is a loop
#' @param tool.survey The 'survey' sheet of the KOBO tool
#' @param tool.choices The 'choices' sheet of the KOBO tool
#' @param issue The reason you're recoding this entry, "Recoding other response" by default
#' @param label_colname the column with the choice label in your tool.choices. 'label::English' by default
#' @param existing your 'existing' column - 'existing.v' by default
#' @param ref.type your 'ref.type' column - 'ref.type' by default
#'
#' @return a filled out cleaning log file
#'
#' @note This function is not vectorized. It only works 1 row of `other_requests` at a time.
#' For the vectorization functionality, please use `vectorized.add.to.cleaning.log.other.recode`
#'
#' @export
#'
#' @examples
#' \dontrun{
#' add.to.cleaning.log.other.recode(data = data, other_requests = other_requests, is.loop = F,
#' tool.survey = tool.survey, tool.choices=tool.choices)
#' }
add.to.cleaning.log.other.recode <- function(data, other_requests,is.loop,
                                             tool.survey,tool.choices,
                                             issue = "Recoding other response",
                                             label_colname = 'label::English',
                                             existing = 'existing.v',
                                             ref.type = 'ref.type'){
  if (other_requests[[ref.type]][1]=="select_one"){
    res <- add.to.cleaning.log.other.recode.one(data = data, other_requests= other_requests,
                                                is.loop = is.loop,
                                                tool.survey=tool.survey,tool.choices=tool.choices)
  }else if(other_requests[[ref.type]][1]=="select_multiple"){
    res <- add.to.cleaning.log.other.recode.multiple(data = data, other_requests= other_requests,
                                                     is.loop = is.loop,
                                                     tool.survey=tool.survey,tool.choices=tool.choices)
  }
  return(res)
}





#' Vectorized add recoded other requests to the cleaning log
#'
#' @param data The dataframe
#' @param other_requests  Other_requests file
#' @param tool.survey The 'survey' sheet of the KOBO tool
#' @param tool.choices The 'choices' sheet of the KOBO tool
#' @param is.loop Whether the dataframe and the request line is a loop
#' @param issue The reason you're recoding this entry, "Recoding other response" by default
#' @param label_colname the column with the choice label in your tool.choices. 'label::English' by default
#' @param existing your 'existing' column - 'existing.v' by default
#' @param ref.type your 'ref.type' column - 'ref.type' by default
#'
#' @return a filled out cleaning log file
#' @export
#'
#' @examples
#' \dontrun{
#' vectorized.add.to.cleaning.log.other.recode(data = data, other_requests = other_requests, is.loop = F,
#' tool.survey = tool.survey, tool.choices=tool.choices)
#' }
vectorized.add.to.cleaning.log.other.recode <-  function(data, other_requests,
                                                         tool.survey,tool.choices,
                                                         is.loop,
                                                         issue = "Recoding other response",
                                                         label_colname = 'label::English',
                                                         existing = 'existing.v',
                                                         ref.type = 'ref.type'){

  if(any(is.na(other_requests[[existing]]))){
    stop("Some of the rows in the existing column are NA, are you sure you're uploading only existing entries?")
  }

  if(length(setdiff(other_requests$name,colnames(data)))>0){
    warning(paste0(
      'Some of the entries in your name column in the other_requests file are not present in the columns of the dataframe:\n',
      paste0(setdiff(other_requests$name,colnames(data)), collapse = '\n'), 'Make sure to run them for the correct dataframe'
    ))
    other_requests <- other_requests[other_requests$name %in% colnames(data),]
  }

  if(!is.loop && "loop_index" %in% colnames(data)){
    warning("Parameter is.loop is = False, but data contains column 'loop_index'. It will be assumed that this data is, actually, a loop!")
    is.loop <- T
  }else if(is.loop && (!"loop_index" %in% colnames(data))) {
    stop("Parameter is.loop is = True, but data does not contain column 'loop_index'!\n")
  }




  clog <- lapply(1:nrow(other_requests),function(row){
    add.to.cleaning.log.other.recode(data = data,
                                     other_requests=other_requests[row,],
                                     is.loop = is.loop,
                                     tool.survey = tool.survey,
                                     tool.choices = tool.choices,
                                     label_colname= label_colname,
                                     existing = existing,
                                     ref.type = ref.type,
                                     issue = issue
    )
  })

  clog <- do.call(rbind, clog)
  return(clog)

}









