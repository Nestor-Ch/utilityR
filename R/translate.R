#' Look up a raw dataframe to find all responses to a given set of questions.
#'
#' @param .dataframe raw data
#' @param questions.db dataframe that contain a column `name` such as a subset of `tool.survey`
#' @param values_to Name of th vector containing the found responses
#' @param is.loop A boolean defining if the raw data you are using is the main or one of the loops
#'
#' @return A dataframe containing the uuid, name and the column specified by values_to.
#'         In case is.loop is T, then a column called loop_index will be added with the unique values.
#' @export
#'
#' @examples
#' q.db <- data.frame(name = c("age","occupation"))
#' raw.data <- data.frame(age = c(21,32), occupation = c("cook","train conductor"),uuid = c("abc","def"))
#' find.responses(raw.data, q.db, "responses")

find.responses <- function(.dataframe, questions.db, values_to="responses", is.loop = F){
  if(nrow(questions.db) == 0){
    warning("questions.db is empty - returning an empty dataframe.")
    return(data.frame())
  }

  if(nrow(.dataframe) == 0){
    warning("data is empty - returning an empty dataframe.")
    return(data.frame())
  }

  if(!is.loop){
    .dataframe[["loop_index"]] <- NA
  }

  responses <- .dataframe %>%
    dplyr::select(c("uuid", "loop_index", any_of(questions.db$name))) %>%
    tidyr::pivot_longer(cols = any_of(questions.db$name),
                 names_to="question.name", values_to=values_to,
                 values_transform = as.character) %>%
    dplyr::filter(!is.na(!!rlang::sym(values_to))) %>%
    dplyr::select(uuid, loop_index, question.name, !!rlang::sym(values_to))

  if(is.loop){
    responses.j <- responses %>%
      dplyr::left_join(questions.db, by=c("question.name"="name")) %>% dplyr::rename(name="question.name") %>%
      dplyr::left_join(dplyr::select(.dataframe, loop_index), by="loop_index") %>%
      as.data.frame()
  } else {
    responses.j <- responses %>%
      dplyr::left_join(questions.db, by=c("question.name"="name")) %>% dplyr::rename(name="question.name") %>%
      dplyr::left_join(dplyr::select(.dataframe, uuid), by="uuid")%>%
      as.data.frame()
  }
  return(responses.j)
}
