cleaning.log <- data.frame()

if(name_clean_others_file != ''){
  or.edited  <- utilityR::load.requests(directory_dictionary$dir.requests,
                                        other_request_file_pattern,
                                        sheet = sheet_name_others, validate = T)
  
  # check if all ref.type are filled
  if(any(is.na(or.edited$ref.type))){
    
    problematic_vars <- unique(or.edited[is.na(or.edited$ref.type),]$ref.name)
    
    or.edited[is.na(or.edited$ref.type),]$ref.type = 'select_one'
    
    warning(paste0(paste0(problematic_vars,collapse=','),
                   " didn't have any ref.type attached to them. Replaced with select_one. Please double check them "))
  }
  
  
  # check for the missing variables
  
  names_req <- or.edited %>%
    pull(name,ref.name)
  
  names_list <- names(raw.main)
  
  if(length(sheet_names_new)>0){
    for(frame in sheet_names_new){
      txt <- paste0("names(",frame,")")
      names_loop <- eval(parse(text=txt))
      names_list <- c(names_list,names_loop)
    }
  }
  
  names_missing <- setdiff(names_req,names_list)
  
  if(length(names_missing)>0){
    
    stop((paste0("some of the names in your tool are not present in your dataframe. Please double check if they were renamed: ",
                 paste0(names_missing,collapse = ',\n'))))
  }
  
  
  # check for chosen choices
  
  or.edited <- or.edited %>%
    left_join(tool.survey %>% select(name,list_name) %>% rename(ref.name=name)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(existing.v.choice_label = sapply(stringr::str_split(choice, " "), function(choice_list) {
      if (is.na(existing.v)) {
        return(NA_character_)
      }
      existing.v.list <- unlist(strsplit(existing.v, ";"))
      
      for (ch in choice_list) {
        if ((list_name %in% tool.choices$list_name)) {
          label <- utilityR::get.choice.label(ch, list_name,
                                              directory_dictionary$label_colname, tool.choices)
          if ((is.element(label, existing.v.list))) {
            existing.v.list <- existing.v.list[!existing.v.list %in% label]
          }
        } else {
          stop(paste0("The choice list ", list_name, " does not exist in the tool.choices file"))
        }
      }
      return(paste(existing.v.list, collapse = ";"))
    })) %>%
    dplyr::ungroup() %>%
    mutate(existing.v = ifelse(existing.v.choice_label %in% '', NA_character_, existing.v.choice_label),
           invalid.v = ifelse(existing.v.choice_label %in% '', 'YES', invalid.v))
  
  warn <- nrow(or.edited[or.edited$existing.v.choice_label =='',])
  
  or.edited <- or.edited%>%
    dplyr::select(-existing.v.choice_label)
  
  if(warn>0){
    warning(paste0(warn,' of the entries in the existing column of the requests file were already
chosen by the respondent in the cumulative column. These `other` enries will be coded
as invalid to speed up the recoding process'))
  }
  
  if(any(or.edited$check == 1)){
    issue <- paste0('uuid: ', or.edited[or.edited$check == 1,]$uuid,
                    ', variable: ',or.edited[or.edited$check == 1,]$name)
    warning(paste0('Some of your rows have multiple entries in the tei columns, please double-check: ',
                   paste0(issue,collapse = '\n')))
  }
  
  if(any(or.edited$check == 3)){
    issue <- paste0('uuid: ', or.edited[or.edited$check == 3,]$uuid,
                    ', variable: ',or.edited[or.edited$check == 3,]$name)
    stop(paste0('Some of your entries are empty, please double-check: ',
                paste0(issue,collapse = '\n')))
  }
  
  # check that the labels in existing.v match the labels in the tool
  
  consistency_check <- or.edited %>% select(uuid, existing.v, ref.name) %>%
    filter(!is.na(existing.v)) %>%
    tidyr::separate_rows(existing.v  , sep= "[;\r\n]") %>%
    mutate(existing.v = trimws(existing.v)) %>%
    filter(!existing.v=='') %>%
    left_join((tool.survey %>% select(name, list_name)), join_by(ref.name==name )) %>%
    anti_join(tool.choices %>% select(list_name,directory_dictionary$label_colname) %>%
                rename('existing.v'=directory_dictionary$label_colname))
  
  
  
  if(nrow(consistency_check)>0){
    stop("Some of the choices that you've selected in the recode.others
    file do not match the labels that you have in your
         tool. Please check the consistency_check object for more details")
  }
  
  # check if all of `None type answers are used`
  none_selection = c('do_not_know','prefer_not_to_answer','none','none_of_the_above',
                     'dont_know','do_not_want_to_answer')
  
  none_check <- or.edited %>%
    dplyr::filter(!is.na(existing.v)) %>%
    dplyr::mutate(existing2=tolower(gsub('[[:punct:]]','',existing.v))) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(similarity =max(stringdist::stringsim(existing2,gsub('_',' ',none_selection),
                                                        method = 'jaccard',q=1))) %>%
    dplyr::filter(similarity>0.8)
  
  if(nrow(none_check)>0){
    
    warning(paste0(
      "It seems some of the values in the existing.v column are to be recoded to `None`,`Don't know`,'Prefer not to answer',etc.
Values please make sure that their choice names are present in the `none_selection` object.
We have found the following cases: ",
      paste0('uuid: ',none_check$uuid,
             ' ref.column: ',none_check$ref.name,
             ' value: ', none_check$existing.v,collapse=";\n"),
      '\nIf you recoded more `None` cases then we found, please make sure that their choice names are also present in the `none_selection` object'))
  }
  
  
  # check for mistakes when coding select_one
  select_one_issues <- or.edited[
    unlist(lapply(stringi::stri_split(or.edited$existing.v,fixed=';'),function(x){length(x[x!=''])}))>1 &
      or.edited$ref.type=='select_one',]
  
  if(nrow(select_one_issues)>0){
    print(select_one_issues[,c('uuid','loop_index','ref.type','existing.v')])
    stop('Error, some of the select_one variables in your recode file have multiple entries in existing.v, please fix this')
  }
  
  
  or.edited$existing.v <- ifelse(or.edited$existing.v =='\r\n',NA,or.edited$existing.v)
  
  # run the bits below
  
  # separate the other translations to fit each individual dataframe that you have - no unnecessary variables in each
  raw.main_requests <- or.edited %>%
    filter(name %in% names(raw.main))
  if(exists('raw.loop1')){
    raw.loop1_requests <- or.edited %>%
      filter(name %in% names(raw.loop1))
    if(nrow(raw.loop1_requests)==0){
      rm(raw.loop1_requests)
    }
  }
  if(exists('raw.loop2')){
    raw.loop2_requests <- or.edited %>%
      filter(name %in% names(raw.loop2))
    if(nrow(raw.loop2_requests)==0){
      rm(raw.loop2_requests)
    }
  }
  if(exists('raw.loop3')){
    raw.loop3_requests <- or.edited %>%
      filter(name %in% names(raw.loop3))
    if(nrow(raw.loop3_requests)==0){
      rm(raw.loop3_requests)
    }
  }
  
  
  
  
  # If you face any weird double spaces
  tool.choices$`label::English`=str_squish(tool.choices$`label::English`)
  tool.choices$name=str_squish(tool.choices$name)
  
  # Create a cleaning log file for each loop if there's a need for it.
  cleaning.log.other.main <- utilityR::recode.others(
    data = raw.main,
    or.edited = raw.main_requests,
    orig_response_col = 'responses',
    is.loop = F,
    tool.choices = tool.choices,
    tool.survey = tool.survey,
    none_selection = none_selection)
  
  # Recode elsewhere functionality
  if(nrow(raw.main_requests[!is.na(raw.main_requests$true_elsewhere),])>0){
    
    raw.main_requests_elsewhere <- raw.main_requests[!is.na(raw.main_requests$true_elsewhere),]
    
    cleaning.log.other.main.elsewhere <- utilityR::recode.others.elsewhere(
      data= raw.main,
      or.edited = raw.main_requests_elsewhere,
      tool.survey = tool.survey,
      is.loop = F)
    
  }else{cleaning.log.other.main.elsewhere <- data.frame()}
  
  cleaning.log.other.main <- rbind(cleaning.log.other.main,cleaning.log.other.main.elsewhere)
  
  if(exists('raw.loop1_requests')){
    
    cleaning.log.other.loop1 <- utilityR::recode.others(
      data = raw.loop1,
      or.edited = raw.loop1_requests,
      orig_response_col = 'responses',
      is.loop = T,
      tool.choices = tool.choices,
      tool.survey = tool.survey,
      none_selection = none_selection)
    
    # Recode elsewhere functionality
    if(nrow(raw.loop1_requests[!is.na(raw.loop1_requests$true_elsewhere),])>0){
      
      raw.loop1_requests_elsewhere <- raw.loop1_requests[!is.na(raw.loop1_requests$true_elsewhere),]
      
      cleaning.log.other.loop1.elsewhere <- utilityR::recode.others.elsewhere(
        data= raw.loop1,
        or.edited = raw.loop1_requests_elsewhere,
        tool.survey = tool.survey,
        is.loop = T)
      
    }else{cleaning.log.other.loop1.elsewhere <- data.frame()}
    
    cleaning.log.other.loop1 <- rbind(cleaning.log.other.loop1,cleaning.log.other.loop1.elsewhere)
    
  }else{cleaning.log.other.loop1 <- data.frame()}
  
  if(exists('raw.loop2_requests')){
    
    cleaning.log.other.loop2 <- utilityR::recode.others(
      data = raw.loop2,
      or.edited = raw.loop2_requests,
      orig_response_col = 'responses',
      is.loop = T,
      tool.choices = tool.choices,
      tool.survey = tool.survey,
      none_selection = none_selection)
    
    # Recode elsewhere functionality
    if(nrow(raw.loop2_requests[!is.na(raw.loop2_requests$true_elsewhere),])>0){
      
      raw.loop2_requests_elsewhere <- raw.loop2_requests[!is.na(raw.loop2_requests$true_elsewhere),]
      
      cleaning.log.other.loop2.elsewhere <- utilityR::recode.others.elsewhere(
        data= raw.loop2,
        or.edited = raw.loop2_requests_elsewhere,
        tool.survey = tool.survey,
        is.loop = T)
      
    }else{cleaning.log.other.loop2.elsewhere <- data.frame()}
    
    cleaning.log.other.loop2 <- rbind(cleaning.log.other.loop2,cleaning.log.other.loop2.elsewhere)
    
  }else{cleaning.log.other.loop2 <- data.frame()}
  
  if(exists('raw.loop3_requests')){
    
    cleaning.log.other.loop3 <- utilityR::recode.others(
      data = raw.loop3,
      or.edited = raw.loop3_requests,
      orig_response_col = 'responses',
      is.loop = T,
      tool.choices = tool.choices,
      tool.survey = tool.survey,
      none_selection = none_selection)
    
    # Recode elsewhere functionality
    if(nrow(raw.loop3_requests[!is.na(raw.loop3_requests$true_elsewhere),])>0){
      
      raw.loop3_requests_elsewhere <- raw.loop3_requests[!is.na(raw.loop3_requests$true_elsewhere),]
      
      cleaning.log.other.loop3.elsewhere <- utilityR::recode.others.elsewhere(
        data= raw.loop3,
        or.edited = raw.loop3_requests_elsewhere,
        tool.survey = tool.survey,
        is.loop = T)
      
    }else{cleaning.log.other.loop3.elsewhere <- data.frame()}
    
    cleaning.log.other.loop3 <- rbind(cleaning.log.other.loop3,cleaning.log.other.loop3.elsewhere)
    
  }else{cleaning.log.other.loop3 <- data.frame()}
  
  #--------------------# Recode followup relevancies ----------------------
  
  # these are the variables that have the relevancies related to their _other responses
  
  select_multiple_list_relevancies <- c()
  
  if(length(select_multiple_list_relevancies)>0){
    # get the dictionary of the relevancies
    relevancy_dictionary <- utilityR::find.relevances(tool.survey = tool.survey,
                                                      var_list = select_multiple_list_relevancies)
    print('check out the dictionary to make sure that it works')
    View(relevancy_dictionary)
    
    cleaning.log.other.main.relevances <- utilityR::recode.other.relevances(
      data = raw.main,
      cleaning.log.other = cleaning.log.other.main,
      relevancy_dictionary = relevancy_dictionary,
      is.loop = F)
    
    
    cleaning.log.other.main <- rbind(cleaning.log.other.main,cleaning.log.other.main.relevances)
    
    if(exists('raw.loop1_requests')){
      cleaning.log.other.loop1.relevances <- utilityR::recode.other.relevances(
        data = raw.loop1,
        cleaning.log.other = cleaning.log.other.loop1,
        relevancy_dictionary = relevancy_dictionary,
        is.loop = F)
      
      cleaning.log.other.loop1 <- rbind(cleaning.log.other.loop1,cleaning.log.other.loop1.relevances)
      
    }
    
    if(exists('raw.loop2_requests')){
      cleaning.log.other.loop2.relevances <- utilityR::recode.other.relevances(
        data = raw.loop2,
        cleaning.log.other = cleaning.log.other.loop2,
        relevancy_dictionary = relevancy_dictionary,
        is.loop = F)
      
      cleaning.log.other.loop2 <- rbind(cleaning.log.other.loop2,cleaning.log.other.loop2.relevances)
      
    }
    
    if(exists('raw.loop3_requests')){
      cleaning.log.other.loop3.relevances <- utilityR::recode.other.relevances(
        data = raw.loop3,
        cleaning.log.other = cleaning.log.other.loop3,
        relevancy_dictionary = relevancy_dictionary,
        is.loop = F)
      
      cleaning.log.other.loop3 <- rbind(cleaning.log.other.loop3,cleaning.log.other.loop3.relevances)
      
    }
    
  }
  
  
  ## Apply changes from the cleaning log onto our raw data
  raw.main <- utilityR::apply.changes(raw.main, clog = cleaning.log.other.main,is.loop = F)
  
  if(nrow(cleaning.log.other.loop1>0)){
    raw.loop1 <- utilityR::apply.changes(raw.loop1,clog = cleaning.log.other.loop1,is.loop = T)
  }
  if(nrow(cleaning.log.other.loop2>0)){
    raw.loop2 <- utilityR::apply.changes(raw.loop2,clog = cleaning.log.other.loop2,is.loop = T)
  }
  if(nrow(cleaning.log.other.loop3>0)){
    raw.loop3 <- utilityR::apply.changes(raw.loop3,clog = cleaning.log.other.loop3,is.loop = T)
  }
  
  # Create the cleaning log for recoding others
  cleaning.log.other <- rbind(cleaning.log.other.main,cleaning.log.other.loop1,
                              cleaning.log.other.loop2,
                              cleaning.log.other.loop3
  )
  # bind it with the main cleaning log
  cleaning.log <- bind_rows(cleaning.log, cleaning.log.other)
  cleaning.log <- cleaning.log %>% select(-uniqui)
}

### ----------------------Add translation cleaning if needed. ------------------------------

if(name_clean_trans_file!= ''){
  
  trans <-  utilityR::load.requests(directory_dictionary$dir.requests, text_request_file_pattern, validate = F)
  
  # run the bits below
  cleaning.log.trans <- utilityR::recode.trans.requests(trans, response_col = 'responses')
  
  # Separate the cleaning log files so that we only apply changes to those data that need it.
  raw.main_trans <- cleaning.log.trans %>%
    filter(variable %in% names(raw.main))
  
  if(exists('raw.loop1')){
    raw.loop1_trans <- cleaning.log.trans %>%
      filter(variable %in% names(raw.loop1))
  }
  if(exists('raw.loop2')){
    raw.loop2_trans <- cleaning.log.trans %>%
      filter(variable %in% names(raw.loop2))
  }
  if(exists('raw.loop3')){
    raw.loop3_trans <- cleaning.log.trans %>%
      filter(variable %in% names(raw.loop3))
  }
  
  # apply changes to the frame
  raw.main <- utilityR::apply.changes(raw.main,clog = raw.main_trans,is.loop = F)
  if(exists('raw.loop1_trans')){
    if(nrow(raw.loop1_trans)>0){
      raw.loop1 <- utilityR::apply.changes(raw.loop1,clog = raw.loop1_trans,is.loop = T)
      
    }
  }
  if(exists('raw.loop2_trans')){
    if(nrow(raw.loop2_trans)>0){
      raw.loop2 <- utilityR::apply.changes(raw.loop2,clog = raw.loop2_trans,is.loop = T)
    }
  }
  if(exists('raw.loop3_trans')){
    if(nrow(raw.loop3_trans)>0){
      raw.loop3 <- utilityR::apply.changes(raw.loop3,clog = raw.loop3_trans,is.loop = T)
    }
  }
  cleaning.log <- bind_rows(cleaning.log, cleaning.log.trans)
  
}
