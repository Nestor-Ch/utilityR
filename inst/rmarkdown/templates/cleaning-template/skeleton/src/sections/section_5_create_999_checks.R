
cl_log_999 <- tibble()

# get the integers from main
int_cols_main  <- tool.survey %>% 
  filter(type %in% c("integer","decimal") & datasheet == "main") %>% 
  pull(name)
# recode if any
cl_log_999_main <- utilityR::recode.set.NA.if(raw.main,int_cols_main, code = code_for_check,issue = 'Wrong entry') %>% 
  left_join(raw.main %>% select(uuid,!!sym(directory_dictionary$enum_colname)))
#bind the logs
cl_log_999 <- bind_rows(cl_log_999,cl_log_999_main)


if(length(sheet_names_new)>0){
  for(i in 1:length(sheet_names_new)){
    txt <- paste0('int_cols_loop',i,' <- tool.survey %>% 
    filter(type %in% c("integer","decimal") & datasheet != "main" & name %in% names(',sheet_names_new[[i]],')) %>% 
    pull(name)')
    eval(parse(text=txt))
    txt <- paste0('if(length(int_cols_loop',i,')>0){
    cl_log_999_loop',i,' <- utilityR::recode.set.NA.if(raw.loop',i,',int_cols_loop',i,', code = code_for_check,issue = "Wrong entry") %>% 
  left_join(raw.main %>% select(uuid,!!sym(directory_dictionary$enum_colname)))
                  bind_rows(cl_log_999,cl_log_999_loop',i,')}')
    eval(parse(text=txt))

  }
}

if(nrow(cl_log_999)>0){
  warning(paste0('detected ',nrow(cl_log_999),' c("',paste0(code_for_check,collapse = '","'),'") entries in your data check cl_log_999 or "output/checking/999_diferences.xlsx" for details'))
  write.xlsx(cl_log_999, "output/checking/999_diferences.xlsx", overwrite=T)
}




