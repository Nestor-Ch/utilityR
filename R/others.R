#' Checks mismatching between cumulative and binary columns for select multiple answers
#'
#' @param data data frame
#' @param survey tool.survey data frame
#' @param id_col The name of your uuid column
#'
#' @return Data frame with mismatching entities
#' @export
#'
#' @examples
#' \dontrun{
#' select.multiple.check(data=raw.main, survey=tool.survey, id_col="uuid")
#' }
select.multiple.check <- function(data, survey, id_col="uuid") {

  if (!("type" %in% colnames(survey))) {
    stop("Survey data frame doesn't contain column - type")
  }

  sm_names.survey <- survey %>%
    tibble::tibble() %>%
    dplyr::filter(grepl('select_multiple',type)) %>%
    dplyr::pull(name)
  sm_names.data <- dplyr::intersect(sm_names.survey, names(data))
  if (length(sm_names.data) == 0) {
    stop("Tool.survey mismatches data or data doesn't have any select_multiple questions")
  }
  res <- data.frame()
  for(sm_name in sm_names.data){
    if(sum(is.na(data[ , sm_name])) != nrow(data)){

      # get all variants of the answer
      choices <- colnames(data) %>%
        purrr::keep(grepl(paste0("^", sm_name, "/"), .)) %>%
        unname()

      test <- data %>%
        dplyr::select(!!rlang::sym(id_col), !!rlang::sym(sm_name)) %>%
        tidyr::separate_rows(!!rlang::sym(sm_name), sep=" ") %>%
        fastDummies::dummy_cols(sm_name, remove_selected_columns=T) %>%
        dplyr::group_by(!!rlang::sym(id_col)) %>%
        dplyr::summarise_all(sum) %>%
        dplyr::mutate_all(as.character)

      # dummies produce a column with a dummy NA value. We need to account that in the actual df it's an NA
      if(sum(is.na(data[ , sm_name])) > 0){
        test <- test %>%
          dplyr::mutate(across(-!!rlang::sym(id_col), ~dplyr::if_else(!!rlang::sym(paste0(sm_name, '_NA')) == '1', NA, .))) %>%
          dplyr::ungroup() %>%
          dplyr::select(-dplyr::any_of(paste0(sm_name,'_NA')))
      }

      # Add missing columns with all zeros to 'test'
      colnames(test) <- gsub(paste0(sm_name, '_'), paste0(sm_name, '/'), colnames(test))
      missing_choices <- dplyr::setdiff(choices, colnames(test))

      if (length(missing_choices) > 0) {
        test[, missing_choices] <- dplyr::if_else(rowSums(!is.na(test[, -1])) > 0, 0, NA)
      }

      raw.test <- data[ , names(test)]
      columns_to_check <- dplyr::setdiff(names(test), id_col)
      test_res <- utilityR::compare_columns(test, raw.test, id_col=id_col,
                                            columns_to_check=columns_to_check,
                                            issue="wrong matching")
      if(nrow(test_res) > 0){
        res <- rbind(res, test_res)
      }
    }
  }
  if (nrow(res) > 0) {
    res <- res %>%
      dplyr::rename("binary.value"="old.value", "cumulative.value"="new.value")
  }
  return (res)
}
