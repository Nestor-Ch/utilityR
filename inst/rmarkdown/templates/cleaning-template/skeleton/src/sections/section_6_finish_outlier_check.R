if(nrow(cleaning.log.outliers>0)){
  
  # apply changes to the frame
  raw.main.outliers <- cleaning.log.outliers[cleaning.log.outliers$variable%in% names(raw.main),]
  if(nrow(raw.main.outliers)){
    raw.main <- utilityR::apply.changes(raw.main,clog = raw.main.outliers,is.loop = F)
  }
  
  if(length(sheet_names_new)>0){
    for(i in 1:length(sheet_names_new)){
      txt <- paste0('raw.loop',i,'.outliers <- cleaning.log.outliers[cleaning.log.outliers$variable%in% names(raw.loop',i,'),]')
      eval(parse(text=txt))
      txt <- paste0('if(nrow(raw.loop',i,'.outliers)>0){
      raw.loop',i,' <- utilityR::apply.changes(raw.loop',i,',clog = raw.loop',i,'.outliers,is.loop = T)
      }')
      eval(parse(text=txt))
    }}
  
}
