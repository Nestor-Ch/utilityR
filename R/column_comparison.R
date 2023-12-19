


#' Compare columns of clean and old dataframes and build a cleaning log
#'
#' @param clean_data Clean data
#' @param raw_data Raw dataframe
#' @param id_col The name of your uuid column
#' @param columns_to_check Which columns should be checked
#' @param is.loop Whether your data is a loop
#' @param issue Why did you recode them
#'
#' @return A cleaning log
#' @export
#'
#' @examples
#' \dontrun{
#' compare_columns(clean_data = clean.data, raw_data = raw.main, uuid_col ='uuid', columns_to_check = col_list)
#' }
compare_columns <- function(clean_data, raw_data, id_col, is.loop=F, columns_to_check, issue){

  if(is.loop & !id_col =='loop_index'){
    stop("You've indicated that the data is a loop but didn't choose loop_index as the id_col")
  }

  if(is.loop==F & id_col =='loop_index'){
    stop("You've indicated that the data is not a loop but choose loop_index as the id_col")
  }

  if('loop_index'%in% names(clean_data) & is.loop==F){
    stop("You've indicated that the data is not a loop but your dataframe contains loop_index column")
  }

  if(!id_col %in% names(raw_data) | !id_col %in% names(clean_data)){
    stop('Your id_col is not present in one of the dataframes')
  }


  diff_ids <- setdiff(clean_data[[id_col]] , raw_data[[id_col]])

  if(length(diff_ids)>0){
    warning(paste0('Some of the ids in your clean data are not present in your raw data and will be excluded: ',
                   paste0(diff_ids,collapse = ',\n')))
    clean_data <- clean_data[!clean_data[[id_col]] %in% diff_ids,]
  }

  diff_cols <- setdiff(columns_to_check,names(clean_data) )
  if(length(diff_cols)>0){
    warning(paste0('Some of the columns in your columns_to_check list are not present in your clean data and will be excluded: ',
                   paste0(diff_cols,collapse = ',\n')))
    columns_to_check <- intersect(columns_to_check,names(clean_data))
  }

    clean_data$uniqui <- clean_data[[id_col]]
    raw_data$uniqui <- raw_data[[id_col]]


    comparison_results <- clean_data %>%
      dplyr::select(uniqui, uuid, dplyr::all_of(columns_to_check)) %>%
      dplyr::mutate_all(as.character) %>%
      tidyr::pivot_longer(dplyr::all_of(columns_to_check), names_to = 'variable', values_to = 'new.value' )

    old_results <- raw_data %>%
      dplyr::select(uniqui, uuid, dplyr::all_of(columns_to_check)) %>%
      dplyr::mutate_all(as.character) %>%
      tidyr::pivot_longer(dplyr::all_of(columns_to_check), names_to = 'variable', values_to = 'old.value' )


    comparison_results <- comparison_results %>% dplyr::left_join(old_results)

  # Filter out rows where values are the same
  comparison_results <- comparison_results[!comparison_results$old.value %==na% comparison_results$new.value, ]
  rownames(comparison_results) <- NULL

  if(is.loop == F){
    comparison_results <- comparison_results %>%
      dplyr::select(-uniqui) %>%
      dplyr::mutate(loop_index = NA,
                    issue = issue)
  }else{
    comparison_results <- comparison_results %>%
      dplyr::rename(loop_index = uniqui) %>%
      dplyr::mutate(issue = issue)
  }

  comparison_results <- comparison_results %>% dplyr::select(uuid, loop_index, variable, old.value, new.value, issue)

  return(comparison_results)
}

