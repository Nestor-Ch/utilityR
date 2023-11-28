#' Look up the values of specified columns in 2 different datasets
#'
#' @param id The ids you want to look up
#' @param id_column The name of the ID column
#' @param column The column you want to look up
#' @param clean.data Your clean dataset
#' @param raw.data Your raw dataset
#'
#' @return A lookup table comparing the old and new values of a column for the given uuid
#'
#' @examples
#' \dontrun{
#' lookup_columns(id = 'uuid_example', id_column = 'uuid', column = 'q_1_a',
#' clean.data = clean.main, raw.data = raw.main)
#' }
lookup_columns <- function(id, id_column, column,clean.data,raw.data) {

  old_value <- raw.data[raw.data[[id_column]] == id, ][[column]]
  new_value <- clean.data[clean.data[[id_column]] == id, ][[column]]

  data.frame(id = id, column = column, old.value = old_value, new.value = new_value)
}


#' Compare columns of clean and old dataframes and build a cleaning log
#'
#' @param clean_data Clean data
#' @param raw_data Raw dataframe
#' @param uuid_col The name of your uuid column
#' @param columns_to_check Which columns should be checked
#'
#' @return A cleaning log
#' @export
#'
#' @examples
#' \dontrun{
#' compare_columns(clean_data = clean.data, raw_data = raw.main, uuid_col ='uuid', columns_to_check = col_list)
#' }
compare_columns <- function(clean_data, raw_data, uuid_col, columns_to_check){

  diff_ids <- setdiff(clean_data[[uuid_col]] , raw_data[[uuid_col]])

  if(length(diff_ids)>0){
    warning(paste0('Some of the uuids in your clean data are not present in your raw data and will be excluded: ',
                   paste0(diff_ids,collapse = ',\n')))
    clean_data <- clean_data[!clean_data[[uuid_col]] %in% diff_ids,]
  }

  diff_cols <- setdiff(columns_to_check,names(clean_data) )
  if(length(diff_cols)>0){
    warning(paste0('Some of the columns in your columns_to_check list are not present in your clean data and will be excluded: ',
                   paste0(diff_cols,collapse = ',\n')))
    columns_to_check <- intersect(columns_to_check,names(clean_data))
  }



  comparison_results <- expand.grid(id = unique(clean_data[[uuid_col]]), column = columns_to_check)

  comparison_results <- apply(comparison_results,1,function(x){
    lookup_columns(x[['id']], x[['column']], id_column = uuid_col,clean.data=clean_data, raw.data=raw_data)
  })
  comparison_results <- do.call(rbind,comparison_results)

  # Filter out rows where values are the same
  comparison_results <- comparison_results[!comparison_results$old.value %==na% comparison_results$new.value, ]
  rownames(comparison_results) <- NULL
  return(comparison_results)
}

