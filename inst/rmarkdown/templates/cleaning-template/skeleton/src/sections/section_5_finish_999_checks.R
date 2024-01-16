if(nrow(cl_log_999>0) & apply_999_changes == 'Yes'){
  # Separate the cleaning log files so that we only apply changes to those data that need it.
  raw.main_999 <- cl_log_999 %>% 
    filter(variable %in% names(raw.main))
  
  # apply changes to the frame
  raw.main <- utilityR::apply.changes(raw.main,clog = raw.main_999,is.loop = F)
  
  if(length(sheet_names_new)>0){
    for(i in sheet_names_new){
      # get the weird values per loop
      txt <- paste0(i,'_999 <- cl_log_999 %>% 
      filter(variable %in% names(',i,'))')
      eval(parse(text=txt))
      
      # if any in a given loop, apply changes and drop them
      txt <- paste0(
        "if(nrow(",i,"_999)>0){
          ",i," <- utilityR::apply.changes(",i,",clog = ",i,"_999,is.loop = T)
        }"
      )
      eval(parse(text=txt))
    }
  }
  
  cleaning.log <- bind_rows(cleaning.log, cl_log_999)
}
