
# DECISIONs:
# Enter uuids of the interviews that didn't pass the audit check

duration_check <- readxl::read_excel(make.filename.xlsx(directory_dictionary$dir.audits.check, "survey_durations"))

ids <- duration_check %>% 
  filter(tot.rt<=min_duration_interview) %>% 
  pull(uuid)

deletion.log.too.fast <- utilityR::create.deletion.log(raw.main %>% filter(uuid %in% ids),
                                                       directory_dictionary$enum_colname, "Survey duration deemed too fast.")

ids <- duration_check %>% 
  filter(tot.rt>=max_duration_interview) %>% 
  pull(uuid)

deletion.log.too.slow <- utilityR::create.deletion.log(raw.main %>% filter(uuid %in% ids),
                                                       directory_dictionary$enum_colname, "Survey duration deemed too slow")

# Enter uuids of the interviews that are soft duplicates to remove:
deletion.log.softduplicates <- data.frame()
for(i in 1:length(ls)){
  if(i==1){
    main_dupl <- readxl::read_xlsx(make.filename.xlsx(directory_dictionary$dir.audits.check, "soft_duplicates"),
                                   col_types = "text", sheet = ls[i])
    ids <- c(main_dupl$uuid)
    
    deletion.log.softduplicates <- utilityR::create.deletion.log(raw.main %>% filter(uuid %in% ids),
                                                                 directory_dictionary$enum_colname, "Soft duplicate")
  }else{
    
    loop_dupl <- readxl::read_xlsx(make.filename.xlsx(directory_dictionary$dir.audits.check, "soft_duplicates"),
                                   col_types = "text", sheet = ls[i])
    
    ids <- c(loop_dupl$loop_index)
    
    txt <- paste0('loop_frame <-', sheet_names_new[i-1],' %>% filter(loop_index %in% ids)')
    eval(parse(text = txt))
    
    deletion.log.softduplicates_loop <- utilityR::create.deletion.log(loop_frame,
                                                                 directory_dictionary$enum_colname, "Soft duplicate",
                                                                 is.loop = T, data.main = raw.main)
    
    rm('loop_frame')
    
    soft_duplicates <- bind_rows(soft_duplicates,deletion.log.softduplicates_loop)
    
  }
  }
    

# Enter uuids of the interviews that are incomplete submissions to remove:
ids_incompl <- c(
  
)
deletion.log.incomplete <- utilityR::create.deletion.log(raw.main %>% filter(uuid %in% ids_incompl), directory_dictionary$enum_colname, "Incomplete submission")

deletion.log.audits <- bind_rows(deletion.log.too.fast,deletion.log.too.slow, deletion.log.softduplicates, deletion.log.incomplete)
deletion.log.new <- bind_rows(deletion.log.new, deletion.log.audits)

#################################################
##   removing fast submissions and duplicates  ##
## run this to remove duplicates and no-consents  ##
raw.main  <- raw.main[!(raw.main$uuid %in% deletion.log.audits$uuid),]
if(length(sheet_names_new)>0){
  for(loop in sheet_names_new){
    txt <- paste0(loop,'<-',loop,'[!(',loop,'$uuid %in% deletion.log.audits$uuid),]')
    eval(parse(text=txt))
  }
}#################################################

rm(ids_incompl, ids,deletion.log.too.fast, deletion.log.softduplicates)
