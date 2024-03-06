#' Find ids that are missing from the dataframe
#'
#' @param data Your dataframe
#' @param uniquis uuids or loop indeces that you're trying to double-check with the data
#' @param print_warnings whether the function should print any warnings
#' @param is.loop Whether the data and the uniquis you're feeding it belong to a loop
#'
#' @return Returns a list of uniquis that are missing from your data
#' @export
#'
#' @examples
#' \dontrun{
#' find.missing.ids(data, uniquis = uuid_list)
#' }
find.missing.ids <- function(data, uniquis, print_warnings = T,is.loop=F){
  if(length(uniquis) == 0) {
    return(character(0))}
  if(is.loop == T){
    if(!"loop_index" %in% names(data)) {
      stop("uniquis are loop indexes, but data does not contain column loop_index!")}
    if(any(grepl('^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$',uniquis)) & print_warnings) {
      warning("not all provided identifiers are loop_indexes!")}
    data$uniqui <- data$loop_index
  } else if ("uuid" %in% names(data)) {
    data$uniqui <- data$uuid
  } else{
    stop("data does not contain column uuid!")}
  if(any(!data$uniqui %in% uniquis)){
    missing_ids <- uniquis[!uniquis %in% data$uniqui]
    if(length(missing_ids) == length(uniquis) & print_warnings){
      warning("NONE of the identifiers were found in data!")}
    else if(length(missing_ids) > 0 & print_warnings){
      warning("Identifiers not found in data:\n\t", paste(missing_ids, collapse = ",\n\t"))}
    return(missing_ids)
  }
}

#' Similarity analysis function
#'
#' @param outdata Result from the find.similar.surveys function
#' @param enum.column Name of column that represent unique identifier for the enumerators
#' @param visualise Whether to render visualisation plot or not
#' @param boxplot.path path for storing visualisation file
#'
#' @return Returns analysis and outliers data frames.
#' Analysis data frame contains grouped information for every enumerators,
#' while outliers data frame about enumerators identified as outliers in the analysis.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result.analysis <- analyse.similarity(
#'  outdata=outdata,
#'  enum.column='a2_1_enum_id',
#'  visualise=T,
#'  boxplot.path = "path/to/store/visualisation")
#'  analysis <- result.analysis$analysis
#'  outliers <- result.analysis$outliers
#' }
analyse.similarity <- function(outdata, enum.column, visualise=F, boxplot.path="") {
  if (!(enum.column %in% colnames(outdata))) {
    stop(paste("Column", enum.column, "does not exist in the data."))
  }

  if (nrow(outdata) <= 1) {
    stop("Input outdata data frame is so small (0 or 1 columns)")
  }

  analysis <- outdata %>%
    dplyr::group_by(!!rlang::sym(enum.column)) %>%
    dplyr::summarize(
      num_rows = dplyr::n(),
      num_unique_group_ids = dplyr::n_distinct(`group_id`),
      sum_number_different_columns = sum(number_different_columns, na.rm = TRUE),
      different_columns_coefficient = round(sum_number_different_columns / num_rows, 1)
    )

  analysis$uuid <- analysis %>% dplyr::pull(!!rlang::sym(enum.column))
  colnames <- c("different_columns_coefficient")

  outliers <- detect.outliers(analysis, id="uuid", n.sd=2,
                              method="o1", is.loop=F, colnames)
  if (visualise) {
    generate.boxplot(outliers.list=list(outliers), raw.data_frames.list=list(analysis),
                     columns.list=list(colnames), n.sd=2, boxplot.path=boxplot.path)
  }

  outliers <- outliers %>% dplyr::rename(!!rlang::sym(enum.column) := uuid)
  analysis <- analysis %>% dplyr::select(-uuid)
  return(list(analysis=analysis, outliers=outliers))
}

#' Helper function for the find.similar.surveys, can be used separately
#'
#' Receive data frame and list of columns names that should be processed
#' make text to lower case -> split by words -> sort by words in ascending order -> combine with " "
#' @param df Data frame that should be processed
#' @param labels list of columns names that should be processed
#'
#' @export
#'
#' @examples
#' \dontrun{
#' clean_data <- column.cleaner(data, colnames(data))
#' }
column.cleaner <- function(df, labels) {
  if (!all(labels %in% colnames(df))) {
    stop("Not all neaded columns exist")
  }
  output_data <- df

  process_tokens <- function(tokens) {
    tokens <- tolower(tokens)
    tokens <- sort(tokens)
    stringi::stri_paste(tokens, collapse = " ")
  }

  for (label in labels) {
    if (!all(sapply(output_data[[label]], is.character))) {
      output_data[[label]] <- as.character(output_data[[label]])
      warning(paste("Column '", label, "'was not of character format"))
    }

    output_data[[label]] <- sapply(stringi::stri_split_regex(output_data[[label]], "\\s+"), process_tokens)
  }

  return(output_data)
}

# Helper function for the find.similar.surveys, do not use separately
calculate.gower <- function(data, data.main, uuid, uuids, idnk_value) {
  gower_dist <- cluster::daisy(data, metric = "gower", warnBin = FALSE, warnAsym = FALSE, warnConst = FALSE)
  gower_mat <- as.matrix(gower_dist)

  # Convert distance to number of differences and determine closest matching survey
  r <- unlist(lapply(1:nrow(data), function(i){
    srv1 <- sort(gower_mat[i,] * ncol(data))[1]
    srv2 <- sort(gower_mat[i,] * ncol(data))[2]
    if (names(srv1) == as.character(i)) return(srv2)
    else return(srv1)
  }))

  outdata <- data.main
  outdata[["num_cols_not_NA"]] <- rowSums(data != "NA")
  outdata[[paste0("num_cols_", idnk_value)]] <- rowSums(data == idnk_value)
  outdata[[uuid]] <- uuids
  outdata[["_id_most_similar_survey"]] <- uuids[as.numeric(names(r))]
  outdata[["number_different_columns"]] <- as.numeric(r)
  outdata <- outdata %>% dplyr::arrange(number_different_columns, !!rlang::sym(uuid))

  return(outdata)
}

#' For each survey, it finds the closest matching survey with the same enumerator
#' and with the minimum number of different column
#'
#' @param data.main DataFrame with raw data
#' @param tool.survey tool.survey data frame
#' @param uuid Name of the column in which uuids are stored.
#' @param idnk_value Value (from tool.choices) that represents the answer "I don't know"
#' @param enum.column Name of column that represent unique identifier for the enumerators
#'
#' @export
#'
#' @details
#' - Uses gower distance when finding the most similar surveys
#' - Searches for similar survays in the section of the same enumerator, namely the column specified in enum.column
#'
#' @examples
#' \dontrun{
#' outdata <- find.similar.surveys(
#'  data.main=raw.main,
#'  tool.survey=tool.survey,
#'  enum.column='a2_1_enum_id')
#' }
find.similar.surveys <- function(data.main, tool.survey, uuid="_uuid",
                                 idnk_value="idnk", enum.column=NA, is.loop=FALSE){
  if (is.na(enum.column)) {
    stop("The column name for the enumerator ids was not provided")
  }

  if (nrow(data.main) <= 1) {
    stop("Input raw data frame is so small (0 or 1 columns)")
  }

  if (nrow(tool.survey) == 0) {
    stop("Input tool.survey data frame has 0 rows")
  }

  if (!(uuid %in% colnames(data.main))) {
    stop(paste("Column", uuid, "does not exist in the data."))
  }

  if (!(enum.column %in% colnames(data.main))) {
    stop(paste("Column", enum.column, "does not exist in the data."))
  }

  data <- data.main

  # 1) store UUIDs
  uuids <- data[[uuid]]

  # 2) convert all columns to character and tolower
  data <- dplyr::mutate_all(data, as.character)
  data <- dplyr::mutate_all(data, tolower)

  # 3) remove columns that are naturally different in each survey:
  # - columns of type = "start", "end", etc.
  # - columns starting with "_"
  # - option columns for the select multiple -> keeping only the concatenation column
  types_to_remove <- c("start", "end", "today", "deviceid", "date", "geopoint", "audit", "note")
  if (is.loop == FALSE) {
    types_to_remove <- c(types_to_remove, "calculate")
  }
  cols_to_keep <- data.frame(column=colnames(data)) %>%
    dplyr::left_join(dplyr::select(tool.survey, name, type), by=c("column"="name")) %>%
    dplyr::filter((!(type %in% types_to_remove) &
                     !stringr::str_starts(column, "_") & !stringr::str_detect(column, "/") &
                     !stringr::str_detect(column, "URL") & !stringr::str_detect(column, "url") &
                     !stringr::str_ends(column, "_other") & column != uuid))
  if (nrow(cols_to_keep) <= 1) {
    stop("There is not enought columns for search similarity")
  }
  data <- data[, cols_to_keep$column]
  # 4) remove columns with all NA; convert remaining NA to "NA"; convert all columns to factor
  data <- data[, colSums(is.na(data))<nrow(data)]
  data[is.na(data)] <- "NA"

  data <- column.cleaner(data, colnames(data))

  data <- data %>% dplyr::mutate_if(is.character, factor)
  error.message <- "NAs detected, remove them before proceeding (it can happen when converting to factor)"
  if (sum(is.na(data)) > 0) stop(error.message)

  enum_list <- unique(data[[enum.column]])

  result.outdata <- list()
  for (enumerator in enum_list) {
    enum_ids <- which(data[[enum.column]] == enumerator)
    enum_data <- data %>% dplyr::filter(!!rlang::sym(enum.column) == enumerator)
    # enum_data.main <- data.main[, cols_to_keep$column]
    enum_data.main <- data.main
    enum_data.main <- enum_data.main[enum_ids, ]
    enum_uuids <- uuids[enum_ids]
    if (nrow(enum_data) > 1) {
      result.outdata <- dplyr::bind_rows(result.outdata, calculate.gower(enum_data, enum_data.main,
                                                                         uuid, enum_uuids, idnk_value))
    }
  }
  if (nrow(result.outdata) == 0) {
    stop("There is not any enumerators with more than 1 survey")
  }
  edge_list <- data.frame(from = result.outdata[[uuid]], to = result.outdata[["_id_most_similar_survey"]])
  graph <- igraph::graph_from_data_frame(d = edge_list, directed = FALSE)

  # 9) find connected components in the graph
  components <- igraph::clusters(graph)
  if (nrow(result.outdata) != length(components$membership)) {
    stop("Probably data contains duplicates")
  }
  result.outdata[["group_id"]] <- components$membership

  return(result.outdata)
}



#' Find the questions that have the questions from `var_list` as parents in their relevances
#'
#' @param tool.survey your tool.survey object
#' @param var_list the list of variables you want to check
#'
#' @export
#'
#' @note this function won't work if you have multiple relevance conditions in your rows.
#' They will be filtered out
#' @examples
#' \dontrun{
#' relevancy_dictionary <- find.relevances(
#'  tool.survey=tool.survey,
#'  var_list=c('variable1','variable2'))
#' }
find.relevances <- function(tool.survey, var_list){
  relevancy_dictionary <- tool.survey %>%
    dplyr::select(name,relevant) %>%
    dplyr::mutate(relevant = gsub(' ','',relevant)) %>%
    dplyr::filter(!grepl('\\)or|\\)and',relevant)) %>%
    dplyr::filter(grepl(
      ifelse(length(var_list)>1,paste0(var_list,collapse='|'),var_list),
      relevant))%>%
    tidyr::separate(relevant,into = c('variable','binary'),sep='}') %>%
    dplyr::mutate(
      original =paste0(gsub("selected\\(\\$\\{(\\w+)", "\\1 \\2",variable),'_other'),
      original =gsub(' ','',original),
      variable = gsub("selected\\(\\$\\{(\\w+)", "\\1 \\2", variable),
      binary = gsub('[^_[:alnum:] ]+','',binary)) %>%
    tidyr::unite('relevant',variable:binary,sep='/') %>%
    dplyr::mutate(relevant = gsub(' ','',relevant),
                  similar = stringdist::stringsim(name, original,method='jaccard',q=2)) %>%
    dplyr::filter(!similar>0.85) %>%
    dplyr::select(-similar, -original) %>%
    dplyr::tibble()
  return(relevancy_dictionary)
}
