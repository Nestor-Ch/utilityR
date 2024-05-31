
# DECISIONs:
# Enter uuids of the interviews that didn't pass the audit check
if (file.exists(make.filename.xlsx(directory_dictionary$dir.audits.check, "survey_durations"))){
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
}else{
  deletion.log.too.fast <- data.frame()
  deletion.log.too.slow <- data.frame()
}

# Enter uuids of the interviews that are soft duplicates to remove:
deletion.log.softduplicates <- data.frame()
deletion.log.softduplicates_loop <- data.frame()
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
    
    if( nrow(loop_dupl)>0){
      unique_uuids <- length(unique(loop_dupl$uuid))
      
      txt <- paste0('The uploaded soft duplicate check file from datasheet',ls[i],
                    ' has ',unique_uuids,' unique uuids that are going to be removed. \nAre you sure this is correct? Enter Y to continue:')
      
      res <- menu(c("Y", "n"), title=txt)
      # res <- readline(txt)
      
      if(res == 1){
        
        ids <- c(loop_dupl$loop_index)
        
        txt <- paste0('loop_frame <-', sheet_names_new[i-1],' %>% filter(loop_index %in% ids)')
        eval(parse(text = txt))
        
        deletion.log.softduplicates_loop <- utilityR::create.deletion.log(loop_frame,
                                                                          directory_dictionary$enum_colname, "Soft duplicate",
                                                                          is.loop = T, data.main = raw.main)
        
        rm('loop_frame')
        rm('res')
        
        deletion.log.softduplicates <- bind_rows(deletion.log.softduplicates, deletion.log.softduplicates_loop)
      }
    }
    
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
