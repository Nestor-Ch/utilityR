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
