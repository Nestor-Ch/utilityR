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


#' Translate a vector from a given dataframe.
#'
#' The provided dataframe `responses` must contain the column `values_from` which will be used as input vector for the translation.
#' Also outputs informative logs to file named "translate_info.csv". You may specify the target language using `target_lang` parameter
#' (or keep it NULL - by default it will be autodected)
#'
#' @param responses Dataframe containing a column which shall be translated.
#' @param values_from Name of the column from `responses` which shall be translated.
#' @param directory The directory where the 'translate_info.csv' will be written
#' @param source_lang String containing a two-letter language code. The input vector will be translated from this language. Defaults to NULL - autodetect language.
#' @param target_lang A two-letter language code. Input vector will be translated into this language. Defaults to 'en' - translation to English
#' @param api.key The path to the file with your API key. "resources/microsoft.api.key_regional.R" by default
#'
#' @return The same dataframe as `responses`, but with a new column, containing the translation.
#' The column will be named according to the target language. By default, the output will be stored in column named 'response.en'
#' @export
#'
#' @examples
#' \dontrun{
#' translate.responses(responses = data_responses, values_from = "response.uk",directory = 'outputs/requests')
#' }
translate.responses <- function(responses, values_from = "response.uk",directory, source_lang = NULL, target_lang = "en",
                                api.key = "resources/microsoft.api.key_regional.R"){

  info_df <- data.frame()
  responses_batch <- data.frame()
  temp_resp_whole <- data.frame()
  start_time <- Sys.time()
  # relevant_colnames <- c("uuid","loop_index","name", "ref.name","full.label","ref.type",
  # "choices.label", values_from)

  # extract unique responses from the source dataframe
  responses <- responses %>%
    dplyr::mutate(resp_lower = stringr::str_to_lower(!!rlang::sym(values_from)))

  input_vec <- responses %>%
    dplyr::distinct(resp_lower) %>%
    dplyr::pull(resp_lower)
  # cleaning up html leftovers:
  input_vec <- gsub("&#39;", "'", input_vec) %>% na.omit()
  # counts characters which will be translated
  char_counter <- sum(stringr::str_length(input_vec))
  # TODO: pause here, print the char_counter, and ask the user if the translation should go ahead
  if (char_counter > 200000){
    yes_no <- svDialogs::dlgInput("The number of characters exceeds 200,000. Please enter [YES] if you would like to proceed or [NO] to kill:", "YES or NO")$res
  } else{
    yes_no <- "YES"
  }
  if (char_counter>1000){
    batching <- svDialogs::dlgInput(paste0("How many batches would you like to split your translation (",char_counter," characters)? (please only integer)"), 0)$res
    batching <- as.numeric(batching)
  }else{
    batching <- 1
  }
  if(yes_no == "YES"){
    if(length(input_vec) > 0){
      col_name <- paste0("response.",target_lang)

      temp_resp <- dplyr::tibble(input_vec)
      temp_resp[[col_name]] <- NA
      temp_resp <-  temp_resp[sample(1:nrow(temp_resp)),]
      ## create batches
      temp_resp_batches <- split(temp_resp, factor(sort(rank(row.names(temp_resp))%%batching)))
      progress.bar.title <- as.character(Sys.time())
      pb <- tcltk::tkProgressBar(progress.bar.title, "Number of batches executed", 0, batching, 0, width = 600)
      prog <- 1
      for (temp_resp_batch in temp_resp_batches){
        tcltk::setTkProgressBar(pb, prog, progress.bar.title, paste0("Number of batches executed: ", prog, " of ", batching,"\n",length(temp_resp_batch$input_vec)," responses will be translated to ",target_lang, "\nThis means ",sum(stringr::str_length(temp_resp_batch$input_vec))," utf-8 characters."))
        prog <- prog + 1
        # actual translation:
        result_vec <- NULL
        result_vec <- try(translateR::translate(content.vec = temp_resp_batch$input_vec,
                                                microsoft.api.key = source(api.key)$value,
                                                microsoft.api.region = "switzerlandnorth",
                                                source.lang = source_lang, target.lang = target_lang))
        if(inherits(result_vec,"try-error")) break
        # checking the results
        info_df <- rbind(info_df, data.frame(## DEBUGG IT HERE
          "input_responses_num" = length(temp_resp_batch$input_vec),
          "translated_characters_num" = sum(stringr::str_length(temp_resp_batch$input_vec)),
          "language_from" = ifelse(is.null(source_lang), "NULL", source_lang),
          "result_num" = length(result_vec),
          "time_elapsed" = as.numeric(Sys.time() - start_time),
          "date"=Sys.Date(),
          "status"=NA))
        if(is.null(result_vec)){
          warning("Error while translating responses: result_vec is NULL\n")
          info_df$status <- "error"
        }else{
          temp_resp_batch[[col_name]] <- gsub("&#39;", "'", result_vec)
          if(length(result_vec) == length(temp_resp_batch$input_vec)){
            info_df$status <- "success"
            # bind the translated and source dfs
            temp_resp_whole <- rbind(temp_resp_whole,temp_resp_batch)
          }else{
            info_df$status <- "partial success"
          }
        }
      }
      close(pb)
      if("partial success" %in% info_df$status){
        svDialogs::msgBox("translate.responses: finished - PARTIAL SUCCESS?")
      } else{
        svDialogs::msgBox("translate.responses: finished - SUCCESS")
      }
      responses <- responses %>% dplyr::left_join(temp_resp_whole, by = c("resp_lower" = "input_vec"))
    }else{
      warning("Nothing to be translated")
    }
  }
  # dump info about the results of translation
  log_filename <- paste0(directory,"translate_info.csv")
  if(file.exists(log_filename)){
    write.table(info_df, file = log_filename, append = T, row.names = F, col.names = F, sep = ',')}
  else {write.table(info_df, file = log_filename, row.names = F, col.names = T, sep = ',')}

  responses <- responses %>% dplyr::select(-resp_lower)
  return(responses)
}






















