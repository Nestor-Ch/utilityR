
#' Process audit files
#'
#' @param df Audit files loaded with `load.audit.files` function
#'
#' @return A dataframe describing time-related parameters of the interviews
#' `n.iteration` - The number of iterations per interviews
#' `tot.t` - Total time of the interview. Calculated as `start` of the last `form.exit` event minus
#' the `start` of `form.start` event
#' `tot.rt` - The total response time of the interview. The sum of the `duration` column in the loaded audits dataframe
#' `tot.rt.inter` - The total time between questions in the interview.
#' The sum of the `inter_q_duration` column in the loaded audits dataframe
#' `t` - Time of each iteration. Calculated as `start` of the iteration's `form.exit` event
#' minus the `start` of the iterations`form.start` event
#' `rt` - Response time of each iteration. The sum of the `duration` column in the loaded audits dataframe for the iteration
#' `q` - Number of questions per iteration
#' `j` - Number of jump events per iteration
#' `e` - Number of edits per iteration Calculated as the number of non NA entries in the `old.value` column
#' `w` - Waiting time - the `start` column of iteration's `form.resume`
#' event - the `start` for the column of the pervious iterations `form.exit` event
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
                  duration = ifelse(duration  %_>_% threshold, group_mean, duration)) %>%
    dplyr::select(-group_mean)
  return(result)
}


