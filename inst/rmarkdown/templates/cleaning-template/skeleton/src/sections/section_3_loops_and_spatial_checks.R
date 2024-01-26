
# cat(paste0("This section is only for assessments that includes loops and need to be checked with their respective calculations."))
# cat(paste0("Make sure to adjust variable names accordingly."))
# ## check for inconsistency in loops:
# 
# counts_loop1 <- raw.loop1 %>% 
#   group_by(uuid) %>% 
#   summarize(loop1_count = n())
# loop_counts_main <- raw.main %>% select(uuid, !!sym(enum_colname), date_interview, hh_size) %>% left_join(counts_loop1) %>% 
#   mutate(hh_size = ifelse(hh_size == "999", NA, as.numeric(hh_size))) %>% 
#   filter(hh_size > 1 & loop1_count %!=na% (hh_size - 1))
# 
# if(nrow(loop_counts_main) > 0){
#   # look at the loop_counts (perhaps just send a screenshot to AO)
#   loop_counts_main %>% view(title = "Inconsistencies in loop1")
#   # find loops for inconsistent uuids:
#   inconsistent_loop1 <- loop_counts_main %>% left_join(raw.loop1)
# }else{ cat("No inconsistencies with loops! :)") }
# 
# 
# # DECISION: what to do with these inconsistencies?
# 
# ids_to_clean <- c(
#   # put here the uuids for which *variable* should be adjusted
# )
# loop_indexes_to_delete <- c(
#   # put here the loop indexes which should be removed
#   
# )
# ids_to_delete <- c(
#   # uuids of submissions that will be totally removed
# 
# )
# 
# cleaning.log.loop_inconsitency <- loop_counts_main %>% 
#   filter(uuid %in% ids_to_clean) %>% 
#   mutate(variable = "hh_size", loop_index = NA,
#          old.value = as.character(hh_size), new.value = ifelse(is.na(loop1_count),"1",as.character(loop1_count + 1)), issue = "Inconsistency in number of entries in hh loop") %>% 
#   select(any_of(CL_COLS))
# 
# deletion.log.loop_inconsistency <- tibble()
# dl_inconsistency1 <- create.deletion.log(pull.raw(loop_indexes_to_delete), 
#                                                        enum_colname, "Inconsistency in number of entries in hh loop")
# dl_inconsistency2 <- create.deletion.log(pull.raw(ids_to_delete), 
#                                          enum_colname, "Inconsistency in number of entries in hh loop") %>% 
#   mutate(loop_index = NA)
# deletion.log.loop_inconsistency <- rbind(dl_inconsistency1, dl_inconsistency2)
# 
# ####################################################
# ## run this to delete/clean entries               ##
# raw.loop1 <- raw.loop1[!raw.loop1$loop_index %in% dl_inconsistency1$uuid,]
# ##                                                ##
# raw.main  <- raw.main[! (raw.main$uuid  %in% dl_inconsistency2$uuid),]
# raw.loop1 <- raw.loop1[!(raw.loop1$uuid %in% dl_inconsistency2$uuid),]
# raw.loop2 <- raw.loop2[!(raw.loop2$uuid %in% dl_inconsistency2$uuid),]
# ##                                                ##
# raw.main <- raw.main %>% apply.changes(cleaning.log.loop_inconsitency)
# ####################################################
# 
# deletion.log.new <- bind_rows(deletion.log.new, deletion.log.loop_inconsistency) %>% 
#   relocate(loop_index, .after = uuid)
# cleaning.log <- cleaning.log.loop_inconsitency   # a brand new cleaning log
# 
# cleaning.log <- tribble()
# 
# rm(ids_to_clean, loop_indexes_to_delete, counts_loop1)
# 
# #-------------------------------------------------------------------------------
# 
# ## GPS checks
# warning("No need to run if GPS checks is not needed.")
# 

if(geo_column  %in% names(raw.main)){
  suspicious_geo <- raw.main %>% 
    mutate(check = gsub(".*\\s(\\d+\\.\\d+)$", "\\1",!!sym(geo_column)),
           check = as.numeric(check)) %>%
    filter(check ==0) %>% 
    select(-check)
  
  if(nrow(suspicious_geo)>0){
    warning(paste0('Found ',nrow(suspicious_geo),' entries with suspicious coordinates'))
    
    deletion.log.coord <- utilityR::create.deletion.log(suspicious_geo,
                                                        directory_dictionary$enum_colname, 
                                                        "The geopoint accuracy is 0.0 may mean that the interview is fake.")
    
    write.xlsx(deletion.log.coord, make.filename.xlsx(directory_dictionary$dir.audits.check, "geospatial_check"),
               zoom = 90, firstRow = T)
    
  }
  
}





# ## In case any needed 
# # run this section always (if POL) ------------------------  -------------------
# 
# if(country == "Poland"){
#   if(!"admin2" %in% colnames(raw.main)){ stop("admin2 column is missing from data")
#   }else{
#     # find if there are any "geopoint" variables in this data:
#     gps_vars <- tool.survey %>% filter(type == "geopoint") %>% pull(name)
#     if(length(gps_vars) == 0){ stop("there are no geopoint variables in tool.survey.")
#     } else if(length(gps_vars) > 1){
#       warning("Found more than one geopoint variable in tool.survey: ", paste(gps_vars, collapse = ", "),"\nOnly the first one will be used to match admin2!")
#       gps_vars <- gps_vars[1]
#     } else{
#       gps_cols <- raw.main %>% select(contains(gps_vars)) %>% names   # there should be 4 of these
#       if(length(gps_cols) < 2) {stop("columns with geopoint data were not found in data")}
#     }
#   }
#   sf_use_s2(TRUE)
#   admin2_boundary <- st_read(dsn = "resources/pol_admbnda_adm2.json") %>% 
#     mutate(ADM2_PCODE = ifelse(is.na(ADM2_PCODE), NA, paste0("20", str_replace(ADM2_PCODE, "PL", "POL"))))  # this is to standardize geo data to our pcode format
#   
#   # TODO additional check for low precision??
#   
#   collected_pts <- raw.main %>% filter(!is.na(!!sym(gps_cols[1]))) %>% 
#     select(uuid, !!sym(enum_colname), admin2, contains(gps_vars)) %>% 
#     left_join(admin2.list) %>%
#     rename(selected_admin2_label = label, selected_admin2 = admin2) %>% 
#     mutate(selected_admin2_label = str_to_title(selected_admin2_label, "pl")) 
#   
#   collected_sf <- collected_pts %>% st_as_sf(coords = paste0(gps_vars, c("_longitude", "_latitude")), crs = "+proj=longlat +datum=WGS84")
#   
#   sf_use_s2(FALSE)
#   
#   spatial_join <- st_join(collected_sf, admin2_boundary, join = st_within) %>% 
#     select(-geometry) %>% select(-contains(gps_vars)) %>% 
#     mutate(GPS_MATCH = ifelse(is.na(ADM2_PCODE), "outside POL", ifelse(ADM2_PCODE == selected_admin2, "match", "WRONG")))
#   
#   if(any(spatial_join$GPS_MATCH != "match")){
#     
#     check_spatial <- tibble(spatial_join) %>% 
#       rename(within_admin2 = ADM2_PCODE, within_admin2_label = ADM2_PL) %>% 
#       left_join(collected_pts %>% select(uuid, contains(gps_vars))) %>% 
#       select(uuid, !!sym(enum_colname), GPS_MATCH, contains(gps_vars), contains("admin2")) %>% 
#       filter(GPS_MATCH != "match")
#     # %>% view
#     
#     write.xlsx(check_spatial, make.filename.xlsx("output/checking/audit/", "gps_checks"), overwrite = T)
#     rm(collected_sf, spatial_join, admin2_boundary)
#     
#   }else cat("All GPS points are matching their selected poviat :)")
# }
# 
# #-------------------------------------------------------------------------------
# 
# # run this section only if there is need to recode spatial data 
# 
# cleaning.log.spatial <- tibble()
# 
# if(country == "Poland"){
#   # for gps points outside Poland, set them to NA immediately
#   check_outside_POL <- check_spatial %>% filter(GPS_MATCH == "outside POL")
#   
#   cleaning.log.spatial <- rbind(cleaning.log.spatial, check_outside_POL %>% 
#     recode.set.NA.regex(gps_cols, ".*", "GPS point is falling outside of Poland"))
#   
#   # how about the other points?
#   check_wrong_admin2 <- check_spatial %>% filter(GPS_MATCH == "WRONG")
# }
# 
# # DECISION: what to do with these inconsistencies:
# 
# # for these uuids, admin2 will be recoded to match the geolocation:
# ids <- c(
#   ## POL
#   "1f43f45b-2dd8-4553-9d1d-a50dc79b841e",
#   "88ff086f-32dc-48c2-b791-b65e85246fa9",    
#   "e1cb77b8-abfe-485f-b3cd-709baa88c419"
#   ##
# )
# cl.spatial_recode <- check_wrong_admin2 %>% filter(uuid %in% ids) %>%  
#   mutate(old.value = selected_admin2, new.value = within_admin2, variable = "admin2", issue = "Enumerator selected wrong poviat by mistake") %>% 
#   select(any_of(CL_COLS))
# 
# # for these uuids, remove geolocation data
# ids <- c(
#   
# )
# cl.spatial_remove_geo <- recode.set.NA.regex(pull.raw(ids), gps_cols, ".*", "Mismatch between selected admin2 and GPS location")
# 
# cleaning.log.spatial <- rbind(cleaning.log.spatial, cl.spatial_recode, cl.spatial_remove_geo)
# 
# # do we remove any suspicious surveys because of GPS mismatch?
# ids_remove <- c(
#   
# )
# deletion.log.new <- rbind(deletion.log.new,
#                           create.deletion.log(pull.raw(ids_remove), enum_colname, "Mismatch between selected admin2 and GPS location"))
# 
# # ------------------------------------
# raw.main <- raw.main %>% apply.changes(cleaning.log.spatial)
# cleaning.log <- bind_rows(cleaning.log, cleaning.log.spatial)

#################################################
raw.main  <- raw.main[! (raw.main$uuid  %in% deletion.log.new$uuid),]
raw.loop1 <- raw.loop1[!(raw.loop1$uuid %in% deletion.log.new$uuid),]
# raw.loop2 <- raw.loop2[!(raw.loop2$uuid %in% deletion.log.new$uuid),]
#################################################

# deletion log should be now finalized

# Save deletion.log file
#deletion.log.whole <- rbind(deletion.log.previous, deletion.log.new)
#write.xlsx(deletion.log.whole, make.filename.xlsx("output/deletion_log/", "deletion_log", no_date = T), overwrite=T)
write.xlsx(deletion.log.new, make.filename.xlsx("output/deletion_log/", "deletion_log", no_date = T), overwrite=T)