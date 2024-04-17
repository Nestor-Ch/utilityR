
#' Process audit files
#'
#' @param df Audit files loaded with `load.audit.files` function
#'
#' @return A dataframe describing time-related parameters of the interviews\cr
#' - \code{n.iteration} - The number of iterations per interviews \cr
#' - \code{tot.t} - Total time of the interview. Calculated as `start` of the last `form.exit` event minus
#' the `start` of `form.start` event \cr
#' - \code{tot.rt} - The total response time of the interview. The sum of the `duration`
#' column in the loaded audits dataframe \cr
#' - \code{tot.rt.inter} - The total time between questions in the interview.
#' The sum of the `inter_q_duration` column in the loaded audits dataframe\cr
#' - \code{t} - Time of each iteration. Calculated as `start` of the iteration's `form.exit` event
#' minus the `start` of the iterations`form.start` event\cr
#' - \code{rt} - Response time of each iteration. The sum of the `duration` column in the loaded audits dataframe for the iteration\cr
#' - \code{q} - Number of questions per iteration\cr
#' - \code{j} - Number of jump events per iteration\cr
#' - \code{e} - Number of edits per iteration Calculated as the number of non NA entries in the `old.value` column\cr
#' - \code{w} - Waiting time - the `start` column of iteration's `form.resume`event - the `start`\cr
#' - \code{tag} - If you've pre-processed files, this column will tag the uuid-question pairs that were outside of the set threshold
#' for the column of the pervious iterations `form.exit` event
#' @export
#'
#' @note This function is non vectorized. Mostly used in pipes after a `group_by(uuid)` command
#'
#' @examples
#' \dontrun{
#' loaded.audits %>%
#'   dplyr::group_by(uuid) %>%
#'   dplyr::group_modify(~process.uuid(.x)) %>%
#'   dplyr::ungroup()
#' }
process.uuid <- function(df){
  max.num.iterations <- 15
  t <- list() # Time of each iteration
  rt <- list() # response time of each iteration
  rt_inter <- list() #time between the start of a question and the end of a previous one
  j <- list() # number of jumps
  q <- list() # number of questions
  w <- list() # waiting time
  e <- list() # number of edits
  t1 <- df$start[1]
  if (df$event[1]!="form.start"){
    stop("First event is not form.start?!")}
  status <- "filling"
  for (r in 2:nrow(df)){
    if (status=="filling" & df$event[r]=="form.exit"){
      t2 <- df$start[r]
      t <- append(t, (t2-t1)/1000/60)
      sub.df <- dplyr::filter(df, start>=t1 & start<=t2)
      questions <- dplyr::filter(sub.df,  event == "question"| event == "group.questions")
      rt <- append(rt, sum(questions$duration)/60)
      rt_inter <- append(rt_inter, sum(questions$inter_q_duration)/60)
      q <- append(q, length(unique(questions$node)))
      j <- append(j, sum(sub.df$event=="jump"))
      e <- append(e, nrow(dplyr::filter(questions, !is.na(`old.value`) & `old.value`!="")))
      status <- "waiting"
    } else if(status=="waiting" & df$event[r]=="form.resume"){
      t1 <- df$start[r]
      w <- append(w, (t1-t2)/1000/60)
      status <- "filling"
    } else if (status=="waiting" & df$event[r]=="form.exit"){
      if("uuid2" %in% colnames(df)){
        warning(paste("status=waiting while form.exit! uuid:",df$uuid2[r]))
      }
      else warning("status=waiting while form.exit!")
    }
  }


  res <- data.frame()
  res[1, "n.iteration"] <- length(t)
  res[1, "tot.t"] <- round((df[nrow(df), "start"] - df[1, "start"])/60000, 1)
  res[1, "tot.rt"] <- round(sum(dplyr::filter(df, event == "question"| event == "group.questions")$duration)/60, 1)
  res[1, "tot.rt.inter"] <- round(sum(dplyr::filter(df, event == "question"| event == "group.questions")$inter_q_duration,na.rm = T)/60, 1)
  for (i in 1:max.num.iterations){
    res[1, c(paste0("t", i), paste0("rt", i),
             paste0("q", i), paste0("j", i),
             paste0("e", i),
             paste0("w", i))] <- NA
  }
  if (length(t)==0){
    stop(paste0("Your form doesn't have the form.exit event. Please double-check: ", unique(df$uuid2)))}
  else{
    for (i in 1:min(length(t), max.num.iterations)){
      res[1, paste0("t", i)] <- round(t[[i]], 1)
      res[1, paste0("rt", i)] <- round(rt[[i]], 1)
      res[1, paste0("q", i)] <- q[[i]]
      res[1, paste0("j", i)] <- j[[i]]
      res[1, paste0("e", i)] <- e[[i]]
    }
  }
  if (length(w)>0){
    for (i in 1:min(length(w), max.num.iterations)){
      res[1, paste0("w", i)] <- round(w[[i]], 1)
    }
  }

  if('tag' %in% names(df)){
    res$tag <- paste0(df[!df$tag=='',]$tag,collapse = ',')
  }


  # if("uuid2" %in% colnames(res)){
  #   res <- res %>% select(-uuid2)
  # }
  # new functionality :)
  empty_colls <- names(res[,sapply(res, function(x)all(is.na(x)))])

  res <- res %>%  dplyr::select(-dplyr::all_of(empty_colls))  # dropping empty columns (all NA)
  return(res)
}


#' Pre-processing audit files
#'
#' In cases when you believe that the enumerator was stalling on some of the questions to increase the time of the
#' interview, you can fix it with this function. Every value of the `duration` column of your audits above your
#' `threshold` value will be set to the question group average calculated by omitting the values above this threshold.
#' If there are no such values, the value for such rows will be set to 0.
#'
#' @param df Audit files loaded with `load.audit.files` function
#' @param threshold Your numeric threshold for
#'
#' @return Audit files without the outlier duration values
#' @export
#'
#' @examples
#' \dontrun{
#' pre.process.audits(df, 10)
#' }
pre.process.audits <- function(df,threshold){
  threshold <- as.numeric(threshold)*60
  df$duration <- as.numeric(df$duration)
  group_means <- df %>%
    dplyr::filter(duration  %_<_% threshold) %>%
    dplyr::group_by(question) %>%
    dplyr::summarise(group_mean = mean(duration, na.rm = TRUE))

  result <- df %>%
    dplyr:: left_join(group_means, by = "question") %>%
    dplyr::mutate(group_mean = ifelse(is.na(group_mean),0,group_mean),
                  tag = ifelse(duration  %_>_% threshold & !is.na(question), paste0(uuid,'-',question), ''),
                  duration = ifelse(duration  %_>_% threshold, group_mean, duration)) %>%
    dplyr::select(-group_mean)%>%
    dplyr::ungroup()
  return(result)
}



#' Geospatial processing of audit data
#'
#' @param df the dataframe of audits
#' @param start_q the question you consider to be the start of the interview
#' @param end_q the question you consider to be the end of the interview
#'
#' @return a dataframe containing the id, time and location data for the first, second and last set of coodrinates per interview
#' @export
#' @note This function is non vectorized. Mostly used in pipes after a `group_by(uuid)` command
#' @examples
#' \dontrun{
#' loaded.audits %>%
#'   dplyr::group_by(uuid) %>%
#'   dplyr::group_modify(~process.audit.geospatial(.x,start_q='consent',end_q='final_q_of_interview')) %>%
#'   dplyr::ungroup()
#' }
process.audit.geospatial <- function (df, start_q, end_q)
{
  if (any(!c("latitude", "longitude") %in% names(df))) {
    stop("Error, no geospatial data in your log file. Please double check")
  }
  if (any(!c(start_q, end_q) %in% df$question)) {
    if ("uuid2" %in% names(df)) {
      warning(paste0("The questions you've entered are not present in the data for uuid: ",
                     unique(df$uuid2)))
    }
    else {
      warning("The questions you've entered are not present in the data for this uuid")
    }
    res_df <- data.frame(start = NA_integer_, end = NA_integer_,
                         latitude = NA_integer_, longitude = NA_integer_,
                         accuracy = NA_integer_,
                         question = "all", variable_explanation = "issue",
                         issue = paste0(paste(setdiff(c(start_q, end_q),
                                                      df$question), collapse = ","), " is not present for this uuid"))
    return(res_df)
  }
  if (any(df$event %in% c("location.tracking.disabled", "location providers disabled",
                          "location permissions not granted"))) {
    warning("The enumerator has disabled the geolocation tracking")
    res_df <- data.frame(start = NA_integer_, end = NA_integer_,
                         latitude = NA_integer_, longitude = NA_integer_,
                         accuracy = NA_integer_,
                         question = "all", variable_explanation = "issue",
                         issue = "Enum disabled location tracking")
    return(res_df)
  }
  else {
    time_start <- df %>% dplyr::filter(question %in% start_q) %>%
      dplyr::filter(start %in% min(start)) %>% dplyr::pull(start)
    time_end <- df %>% dplyr::filter(question %in% end_q,
                                     !event %in% 'jump') %>%
      dplyr::filter(start %in% max(start)) %>% dplyr::pull(start)
    df_filt <- df %>% dplyr::filter(!is.na(latitude) & start <=
                                      time_end & start >= time_start) %>% dplyr::mutate(coord = paste(latitude,
                                                                                                      longitude, accuracy))
    if (all(is.na(df_filt$latitude))) {
      warning("All values between the chosen questions are NA. Returning an empty df")
      res_df <- data.frame(start = NA_integer_, end = NA_integer_,
                           latitude = NA_integer_, longitude = NA_integer_,
                           accuracy = NA_integer_,
                           question = "all", variable_explanation = "issue",
                           issue = "No coordinates for the chosen question range")
      return(res_df)
    }
    df_filt <- transform(df_filt, id = match(coord, unique(coord)))
    max_id <- max(df_filt$id)
    df_filt <- df_filt %>% dplyr::group_by(id) %>% dplyr::filter(dplyr::if_else(id ==
                                                                                  max_id, start == max(start), start == min(start))) %>%
      dplyr::slice_head(n = 1) %>% dplyr::ungroup()
    res_df <- data.frame(start = df_filt$start, end = df_filt$end,
                         latitude = df_filt$latitude, longitude = df_filt$longitude,
                         accuracy = df_filt$accuracy,
                         question = df_filt$question, variable_explanation = paste0("coordinate ",
                                                                                    df_filt$id), issue = "To be cheked")
    return(res_df)
  }
}









