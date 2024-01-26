deletion.log.new <- data.frame()
deletion.log.dupl <- data.frame()

#check for duplicates 
ids <- raw.main$uuid[duplicated(raw.main$uuid)]

if (length(ids)>0) {
  warning("Duplicate uuids detected: ", length(ids))
  # add to deletion log
  deletion.log.dupl <- utilityR::create.deletion.log(raw.main %>% filter(duplicated(raw.main[['uuid']])),
                                                     directory_dictionary$enum_colname, "Duplicate") # a brand new deletion log
} else{
  deletion.log.dupl <- data.frame()
}

#del log dupl

if(length(sheet_names_new)>0){
  for(loop in sheet_names_new){
    txt <- paste0(loop,' %>% 
  filter(uuid %in% ids) %>% 
  select(uuid,parent_index) %>%
  distinct() %>% 
  group_by(uuid) %>% 
  mutate(n=1,
         n=cumsum(n)) %>% 
  filter(n>1) %>% 
  select(-n) %>% 
  left_join(',loop,' %>% select(uuid,parent_index, loop_index)) %>% 
  pull(loop_index)')
    ids_loop <- eval(parse(text = txt))
    if (length(ids_loop)>0){
      warning("Duplicate uuids detected: ", length(ids_loop))
      txt <- paste0(loop,' %>% filter(loop_index %in% ids_loop)')
      dupl_df<- eval(parse(text = txt))
      # add to deletion log
      deletion.log.loop <- utilityR::create.deletion.log(dupl_df,
                                                         directory_dictionary$enum_colname, "Duplicate",
                                                         is.loop = T,
                                                         data.main = raw.main) # a brand new deletion log
      deletion.log.dupl <- bind_rows(deletion.log.dupl,deletion.log.loop)
    }
  }
}

## run this to remove duplicates ##
raw.main  <- raw.main %>% 
  group_by(uuid) %>% 
  mutate(n= 1,
         n=cumsum(n)
  ) %>% 
  filter(n==1) %>% 
  select(-n) %>% 
  ungroup()

## run this to remove duplicates from looops##

if(length(sheet_names_new)>0){
  for(loop in sheet_names_new){
    txt <- paste0(loop,'<-',loop,'%>% 
  filter(!loop_index %in% deletion.log.dupl$loop_index)')
    eval(parse(text=txt))
  }
}


# ------------------------------- Check for no consent ----------------------------------
no_consents <- data.frame()

# enter your no consent condition
# some examples:
#(a1_informed_consent == "no", a4_1_displaced_consent == "no", a5_1_residence_change_consent == "no", a5_2_traveling_arrived_consent == "no")
# raw.main$date_calcul = as.numeric(raw.main$date_calcul)
# no_consents <- raw.main %>% filter(date_calcul > 14)


if (nrow(no_consents) > 0){
  warning("No-consent detected: ", nrow(no_consents))
  
  # add to deletion log
  if("no_consent_why" %in% colnames(raw.main)){
    deletion.log.no_consents <- no_consents %>% 
      mutate(reason = paste0("no consent", ifelse(is.na(no_consent_why), "", paste0(": ", no_consent_why)))) %>% select(uuid, !!sym(directory_dictionary$enum_colname), reason)
    # translate the no-consents reasons :)
    deletion.log.no_consents <- utilityR::translate.responses(responses=deletion.log.no_consents,
                                                              values_from = "reason",
                                                              directory = dir.requests,
                                                              api.key = api_key)
    deletion.log.no_consents %>% 
      translate.responses("reason") %>% 
      mutate(reason = str_to_lower(response.en.from.uk)) %>% 
      select(-response.en.from.uk)
  }else{
    deletion.log.no_consents <- no_consents %>% 
      utilityR::create.deletion.log(directory_dictionary$enum_colname, "no consent")
  }
}else{
  deletion.log.no_consents <- data.frame()
}
deletion.log.new <- rbind(deletion.log.new, deletion.log.no_consents)


# if you have any test submissions, define them here 
test_submission <- raw.main %>% 
  filter(grepl('^тест|^test', tolower(!!sym(directory_dictionary$enum_comments))))

if (nrow(test_submission) > 0){
  warning("test_submission detected: ", nrow(test_submission))
deletion.log.test_submission <- test_submission %>% utilityR::create.deletion.log(directory_dictionary$enum_colname, "test_submission")
}else{
  deletion.log.test_submission = data.frame()
}

deletion.log.new <- rbind(deletion.log.new, deletion.log.test_submission)

####################################################
## run this to remove duplicates and no-consents  ##
raw.main  <- raw.main[!(raw.main$uuid %in% deletion.log.new$uuid),]

if(length(sheet_names_new)>0){
  for(loop in sheet_names_new){
    txt <- paste0(loop,'<-',loop,'[!(',loop,'$uuid %in% deletion.log.new$uuid),]')
    eval(parse(text=txt))
}
}
####################################################

rm(test_submission, deletion.log.test_submission,deletion.log.no_consents)
