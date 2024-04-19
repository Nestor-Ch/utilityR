source("src/load_Data.R")

deleted_colums_ls <- c()
if(length(ls_loops)>0){
  for(i in 1:length(ls_loops)){
    txt <- paste0('del_list <- setdiff(names(kobo.raw.loop',i,'),names(raw.loop',i,'))')
    eval(parse(text = txt))
    deleted_colums_ls <- c(deleted_colums_ls,del_list)
  }
}

deleted_colums <- data.frame(variable=c(setdiff(names(kobo.raw.main),names(raw.main)),deleted_colums_ls),
                             action = 'removed',
                             rationale = NA
)

data_extract <- kobo.raw.main[,c('uuid', directory_dictionary$enum_colname)]

logbook <- cleaning.log %>% 
  left_join(kobo.raw.main %>% select(uuid,deviceid )) %>% 
  mutate(type_of_issue = NA,
         changed = 'Yes',
         feedback=NA) %>% 
  select(uuid, !!sym(directory_dictionary$enum_colname),deviceid,variable,issue, type_of_issue,feedback,changed,old.value, new.value) %>% 
  tibble() %>% 
  distinct()


del_log <- deletion.log.new %>% 
  left_join(kobo.raw.main %>% 
              select(uuid,deviceid,all_of(directory_dictionary$enum_colname)) %>% 
              distinct()) %>% 
  select(uuid,all_of(directory_dictionary$enum_colname),deviceid,reason) %>% 
  mutate(type_of_issue = NA,
         feedback = 'deleted') %>% 
  distinct()


submission_file <- list(
  'variable_tracker' =deleted_colums,
  'data_extract'=data_extract,
  'logbook' = logbook,
  'del_log'=del_log
)

write.xlsx(submission_file, make.filename.xlsx("output/Cleaning_logbook", "Cleaning_logbook"), overwrite = T,
           zoom = 90, firstRow = T)


cat("\n> Done. Created 1 file in output/Cleaning_logbook.")
