
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
soft_duplicates <- readxl::read_excel(make.filename.xlsx(directory_dictionary$dir.audits.check, "soft_duplicates"))

ids <- c(soft_duplicates$uuid)

deletion.log.softduplicates <- utilityR::create.deletion.log(raw.main %>% filter(uuid %in% ids),
                                                             directory_dictionary$enum_colname, "Soft duplicate")

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
