if(file.exists(make.filename.xlsx(directory_dictionary$dir.audits.check, "geospatial_check"))){
  deletion.log.coord <- readxl::read_excel(make.filename.xlsx(directory_dictionary$dir.audits.check,
                                                              "geospatial_check"))
  
  if(file.exists(make.filename.xlsx("output/checking/audit/", "gps_checks"))){
    deletion.log.polygon <- readxl::read_excel(make.filename.xlsx("output/checking/audit/", 
                                                                  "gps_checks")) %>% 
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

# audit checks

if(file.exists( make.filename.xlsx(directory_dictionary$dir.audits.check, "audit_checks_full"))){
  deletion.log.audit.issues <- readxl::read_excel( make.filename.xlsx(directory_dictionary$dir.audits.check,
                                                                      "audit_checks_full"), sheet = 'Audit issues summary') %>% 
    rename(reason = issue)
  
  deletion.log.speed <- readxl::read_excel( make.filename.xlsx(directory_dictionary$dir.audits.check,
                                                               "audit_checks_full"), sheet = 'Speed issues summary')
  if(nrow(deletion.log.speed)>0){
    deletion.log.speed <- deletion.log.speed %>% 
      mutate(reason = 'Enumerator\'s movement speed throught the interview is not realistic') %>% 
      select(-mean_speed)
  }else{
    deletion.log.speed <- data.frame()
  }
  
  deletion.log.location <- readxl::read_excel( make.filename.xlsx(directory_dictionary$dir.audits.check,
                                                               "audit_checks_full"), sheet = 'Location issues summary')
  if(nrow(deletion.log.location)>0){
    deletion.log.location <- deletion.log.location %>% 
      mutate(reason = 'Enumerator wasn\'t in the location indicated in the data') %>% 
      select(uuid, reason, col_enum)
  }else{
    deletion.log.location <- data.frame()
  }
  
  deletion.log.audits <- rbind(deletion.log.location,deletion.log.audit.issues,deletion.log.speed)
  
  
  if(nrow(deletion.log.audits)>0){
    
    deletion.log.new <- bind_rows(deletion.log.new, deletion.log.audits)
    
    #################################################
    ##   removing weird coordinate submissions  ##
    raw.main  <- raw.main[!(raw.main$uuid %in% deletion.log.audits$uuid),]
    if(length(sheet_names_new)>0){
      for(loop in sheet_names_new){
        txt <- paste0(loop,'<-',loop,'[!(',loop,'$uuid %in% deletion.log.audits$uuid),]')
        eval(parse(text=txt))
      }
    }
    #################################################
    
  }}









