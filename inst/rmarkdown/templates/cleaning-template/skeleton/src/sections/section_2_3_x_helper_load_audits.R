if(!exists('audits')){
  if (use_API){
    
    asset <- kobo_asset(asset_uid)
    audits <- kobo_audit(asset)
    
    if(!'uuid' %in% names(audits)){
      audits <- audits %>%
        mutate(`_id` = as.character(`_id`)) %>%
        left_join(raw.main %>%
                    select(`_id`,`uuid`) %>% 
                    mutate(`_id` = as.character(`_id`)))
      
    }
    
    audits <- audits %>%
      dplyr::group_by(uuid) %>%
      dplyr::mutate(inter_q_duration = (start - dplyr::lag(end))) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(duration = (end - start),
                    group = sapply(stringr::str_split(node, "\\/"), function(x) {
                      id.group <- ifelse("G_survey" %in% x, 4, 3)
                      return(x[id.group])}),
                    question = sapply(stringr::str_split(node, "\\/"), function(x) {
                      return(x[length(x)])})) %>%
      dplyr::mutate(event = stringr::str_replace_all(event, " ", ".")) %>%
      dplyr::rename(`new.value` = `new-value`,
                    `old.value`=`old-value`,
                    start_readable = start,
                    end_readable = end,
                    start = start_int,
                    end = end_int)
    
    
  }else{
    # load your audit files
    audits <- utilityR::load.audit.files(directory_dictionary$dir.audits, uuids = raw.main$uuid, track.changes = F)
    audits$start_readable <- as.POSIXct(audits$start / 1000, origin = "1970-01-01")
    audits$end_readable <- as.POSIXct(audits$end / 1000, origin = "1970-01-01")
  }
  
  if(non_gis_check){
    if(nrow(audits) > 0) {
      
      audits <- audits %>%
        mutate(inter_q_duration = ifelse(lag(event)=='jump',0,inter_q_duration),
               duration_clean = floor(duration),
               start_clean = floor(start/10),
               end_clean = floor(end/10)) %>%
        group_by(uuid,duration_clean, end_clean, start_clean) %>%
        mutate(n_questions = n()) %>%
        ungroup() %>%
        select(- c(start_clean,end_clean,duration_clean)) %>%
        mutate(n_questions =ifelse(is.na(n_questions) | is.na(duration) ,1,n_questions),
               duration = duration/n_questions,
               inter_q_duration = ifelse(inter_q_duration<0,0,inter_q_duration)) %>%
        select(-n_questions)
      
      if(pre_process_audit_files){
        audits <- utilityR::pre.process.audits(audits, threshold = max_length_answer_1_question)
      }
      
    }
  }
}