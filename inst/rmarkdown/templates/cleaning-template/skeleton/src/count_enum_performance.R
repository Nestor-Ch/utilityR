source("src/load_Data.R")


deleted_colums <- data.frame(variable=setdiff(names(kobo.raw.main),names(raw.main)),
                             action = 'removed',
                             rationale = NA
)

data_extract <- raw.main[,c('uuid', directory_dictionary$enum_colname)]

logbook <- cleaning.log %>% 
  left_join(kobo.raw.main %>% select(uuid,deviceid )) %>% 
  mutate(type_of_issue = NA,
         changed = 'Yes',
         feedback=NA) %>% 
  select(uuid, enumerator_id,deviceid,variable,issue, type_of_issue,feedback,changed,old.value, new.value) %>% 
  tibble()


del_log <- deletion.log.new %>% 
  left_join(kobo.raw.main %>% 
              select(uuid,deviceid,all_of(directory_dictionary$enum_colname)) %>% 
              distinct()) %>% 
  select(uuid,all_of(directory_dictionary$enum_colname),deviceid,reason) %>% 
  mutate(type_of_issue = NA,
         feedback = 'deleted')
  

submission_file <- list(
  'variable_tracker' =deleted_colums,
  'data_extract'=data_extract,
  'logbook' = logbook,
  'del_log'=del_log
)

write.xlsx(submission_file, make.filename.xlsx("output/enum_performance", "Enumerator_performance_temp"), overwrite = T,
           zoom = 90, firstRow = T)


cat("\n> Done. Created 1 file in output/enum_performance.")
