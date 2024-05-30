
# load your audit files
audits <- utilityR::load.audit.files(directory_dictionary$dir.audits, uuids = raw.main$uuid, track.changes = F)

# add 2 more columns to make readable start and end columns
audits$start_readable <- as.POSIXct(audits$start / 1000, origin = "1970-01-01")
audits$end_readable <- as.POSIXct(audits$end / 1000, origin = "1970-01-01")

# if you want to pre-process audits, do it here
if(pre_process_audit_files){
  audits <- utilityR::pre.process.audits(audits, threshold = max_length_answer_1_question)
}


# process your audit files to get the duration of each interview. To understand what each column means, run help(process.uuid)
if(nrow(audits) == 0) {
  audits.summary <- tibble(uuid = raw.main$uuid, tot.rt = NA)
}else{
  audits.summary <- audits %>%
    distinct(inter_q_duration,duration, uuid,start_readable,end_readable,event, .keep_all = T) %>%
    filter(!(inter_q_duration %_<_% 0 & !event %in% c('form.exit','form.start'))) %>%
    group_by(uuid) %>%
    group_modify(~utilityR::process.uuid(.x)) %>%
    ungroup() %>%
    mutate(tot.rt = tot.rt+tot.rt.inter)
}

# get the additional data from the main df
data.audit <- raw.main %>%
  dplyr::mutate(duration_mins = difftime(end, start, units = 'mins'),
         num_NA_cols = rowSums(is.na(raw.main)),
         num_dk_cols = rowSums(select(., matches("dk_undec")), na.rm = T),
         num_other_cols = rowSums(!is.na(raw.main[str_ends(colnames(raw.main), "_other")]), na.rm = T)) %>%
  select(uuid, !!sym(directory_dictionary$enum_colname), start, end, duration_mins, num_NA_cols, num_dk_cols, num_other_cols)

# Generate the audits_summary file - General info about each interview
audits.summary <- data.audit %>%
  left_join(audits.summary, by="uuid") %>% select(-contains("/")) %>%
  relocate(uuid, duration_mins, num_NA_cols, num_dk_cols, num_other_cols, tot.rt) %>%
  arrange(duration_mins)

write.xlsx(audits.summary, make.filename.xlsx(directory_dictionary$dir.audits.check, "audits_summary"))


# follow up with FPs if there are surveys under 10 minutes or above 1 hour
survey_durations_check <- audits.summary %>% filter(tot.rt < min_duration_interview | tot.rt > max_duration_interview)
if(nrow(survey_durations_check) > 0){
  write.xlsx(survey_durations_check, make.filename.xlsx(directory_dictionary$dir.audits.check, "survey_durations"),
             zoom = 90, firstRow = T)
}else cat("\nThere are no survey durations to check :)")


## Soft duplicates (less than 5 different columns?)

print("Checking for soft duplicates in data grouped by enumerators...")
# if you don't really need to have boxplot with the statistics of enumerators, you can set visualise=F
res.soft_duplicates <- utilityR::find.similar.surveys(raw.main, tool.survey, uuid = "uuid", enum.column=directory_dictionary$enum_colname)

analysis.result <- utilityR::analyse.similarity(res.soft_duplicates, enum.column=directory_dictionary$enum_colname, visualise=T,
                                                boxplot.path="output/checking/audit/enumerators_surveys_")
analysis <- analysis.result$analysis
outliers <- analysis.result$outliers

analysis$data_id <- 'main'
outliers$data_id <- 'main'

soft.duplicates <- res.soft_duplicates %>%
  filter(number_different_columns <= min_num_diff_questions) %>%
  relocate(uuid, num_cols_not_NA,num_cols_idnk,`_id_most_similar_survey`,
           number_different_columns) %>%
  arrange(number_different_columns)

soft_dupl_list <- vector('list',length(ls))

soft_dupl_list[[1]] <- soft.duplicates

# same thing but for loops
if(length(sheet_names_new)>0){
  for(i in 1:length(sheet_names_new)){

  txt <- paste0(sheet_names_new[i], "_tmp <-" ,
                sheet_names_new[i]," %>% left_join(raw.main %>% select(uuid, !!sym(directory_dictionary$enum_colname)))")

  eval(parse(text = txt))

  txt <- paste0('res.soft_duplicates_l <- utilityR::find.similar.surveys(
                ',sheet_names_new[i],'_tmp, tool.survey, uuid = "loop_index", enum.column=directory_dictionary$enum_colname)')

  eval(parse(text = txt))

  analysis.result_l <- utilityR::analyse.similarity(res.soft_duplicates_l, enum.column=directory_dictionary$enum_colname, visualise=T,
                                                  boxplot.path=paste0("output/checking/audit/enumerators_surveys_",sheet_names_new[i]))

  analysis_l <- analysis.result_l$analysis
  outliers_l <- analysis.result_l$outliers

  analysis_l$data_id <- sheet_names_new[i]
  outliers_l$data_id <- sheet_names_new[i]

  analysis <- rbind(analysis,analysis_l)
  outliers <- rbind(outliers,outliers_l)

  min_num_diff_questions_loop <- ceiling(min_num_diff_questions*(ncol(res.soft_duplicates_l)/ncol(res.soft_duplicates)))

  soft.duplicates_l <- res.soft_duplicates_l %>%
    filter(number_different_columns <= min_num_diff_questions_loop) %>%
    relocate(uuid, loop_index, num_cols_not_NA,num_cols_idnk,`_id_most_similar_survey`,
             number_different_columns) %>%
    arrange(number_different_columns)

  soft_dupl_list[[i+1]] <- soft.duplicates_l

  txt <- paste0(sheet_names_new[i],'_tmp')
  rm(txt)
  }

}

names(soft_dupl_list) <- ls

write.xlsx(soft_dupl_list, make.filename.xlsx(directory_dictionary$dir.audits.check, "soft_duplicates"))
write.xlsx(analysis, make.filename.xlsx(directory_dictionary$dir.audits.check, "soft_duplicates_analysis"))
write.xlsx(outliers, make.filename.xlsx(directory_dictionary$dir.audits.check, "soft_duplicates_outliers"))

cat("Check soft duplicates in soft.duplicates data frame or soft_duplicates xlsx file")

cat("Check outliers of enumerators surveys group in output/checking/outliers/enumerators_surveys_2sd.pdf")
cat("Also, you can find analysis of the enumerators in analysis data frame, and outliers in outliers data frame.
      If you want to check data without analysis, res.soft_duplicates for you. You can do different manipulations by yourself")

rm(data.audit, analysis.result)
