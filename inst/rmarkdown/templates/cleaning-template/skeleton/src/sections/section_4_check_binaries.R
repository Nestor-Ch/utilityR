

# check that binaries only contain 0,1 or NA
binaries_main <- names(raw.main %>% select(contains('/')))
wrong_values <- binaries_main[!apply(raw.main[,binaries_main],2,function(x){all(x %in% c('0','1',NA))})]
if (length(wrong_values)>0){
  wrong_values_frame <- data.frame(columns = wrong_values, sheet = 'main')
}else{wrong_values_frame <- data.frame()}


for(sheet in sheet_names_new){
  txt <- paste0('binaries_loop <- names(',sheet,' %>% select(contains("/")))')
  eval(parse(text=txt))
  txt <- paste0("wrong_values <- binaries_loop[!apply(",sheet,"[,binaries_loop],2,function(x){all(x %in% c('0','1',NA))})]")
  eval(parse(text=txt))
  
  if (length(wrong_values)>0){
    wrong_values_frame_loop <- data.frame(columns = wrong_values, sheet = sheet)
  }else{wrong_values_frame_loop <- data.frame()}
  wrong_values_frame <- rbind(wrong_values_frame,wrong_values_frame_loop)
}

if(nrow(wrong_values_frame)>0){
  stop("Error, some of your binary columns contain values outside of the expected rande of 0,1,NA. Please check the wrong_values_frame object for details")
}
