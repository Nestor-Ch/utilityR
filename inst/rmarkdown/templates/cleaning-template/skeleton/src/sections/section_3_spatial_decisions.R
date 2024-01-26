
deletion.log.coord <- readxl::read_excel(make.filename.xlsx(directory_dictionary$dir.audits.check, "geospatial_check"))

if(nrow(deletion.log.coord)>0){
  
  deletion.log.new <- bind_rows(deletion.log.new, deletion.log.coord)

  #################################################
  ##   removing weird coordinate submissions  ##
  raw.main  <- raw.main[!(raw.main$uuid %in% deletion.log.coord$uuid),]
  if(length(sheet_names_new)>0){
    for(loop in sheet_names_new){
      txt <- paste0(loop,'<-',loop,'[!(',loop,'$uuid %in% deletion.log.coord$uuid),]')
      eval(parse(text=txt))
    }
  }
  #################################################
  
}