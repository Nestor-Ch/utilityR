if(file.exists(make.filename.xlsx(directory_dictionary$dir.audits.check, "geospatial_check"))){
  deletion.log.coord <- readxl::read_excel(make.filename.xlsx(directory_dictionary$dir.audits.check, "geospatial_check"))
  
  if(file.exists(make.filename.xlsx("output/checking/audit/", "gps_checks"))){
    deletion.log.polygon <- readxl::read_excel(make.filename.xlsx("output/checking/audit/", "gps_checks")) %>% 
      select(uuid,!!sym(directory_dictionary$enum_colname), GPS_MATCH) %>% 
      rename(reason = GPS_MATCH,
             col_enum  = !!sym(directory_dictionary$enum_colname))
    
    deletion.log.coord <- rbind(deletion.log.coord,deletion.log.polygon)
  }
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
    
  }}