# load the data through API if that was specified

if (use_API){
  
  # check if file exists first (maybe somebody forgot that they've already uploaded it)
  if(!file.exists(directory_dictionary$filename.tool)){
    
    
    tool <- kobo_form(assest_uid)
    tool <- tool %>%
      select(any_of(c('name','list_name','type','label','lang',
             'calculation','required','relevant','appearance','choice_filter', 'constraint',
             'constraint_message', 'hint', 'choices', 'kuid')))
    # remove unnecessary rows from the tool
    tool_cl <- tool %>%
      select(-any_of(c('label','lang','constraint_message','hint'))) %>%
      distinct()
    
    # create the labels
    tool_cl <- tool_cl %>%
      left_join(
        tool %>%
          select(any_of(c('kuid','label','lang','constraint_message','hint'))) %>%
          pivot_longer(any_of(c('label','constraint_message','hint')), names_to = 'column',values_to = 'text') %>%
          mutate(names_col = ifelse(column =='label',
                                    paste0(gsub("\\::.*","",directory_dictionary$label_colname),'::', lang),
                                    paste0(column,'::',lang))) %>%
          pivot_wider(id_cols = kuid, names_from = names_col, values_from = text)
      )
    
    # process the choices
    tool.choices <- tool_cl %>%
      select(any_of(c('list_name','kuid', 'choices'))) %>% # choices are saved in a nested dataframe for each row. We'll unnest them
      rowwise() %>%
      filter(nrow(choices)>0) %>% # but first we'll remove the empty rows
      ungroup() %>%
      unnest(choices) %>%
      select(-any_of(c('value_version'))) %>%
      mutate(names_col = paste0(gsub("\\::.*","",directory_dictionary$label_colname),'::', value_lang)) %>%
      distinct() %>%
      pivot_wider(id_cols = any_of(c('kuid','list_name','value_name')), names_from = names_col, values_from = value_label) %>%
      rename(name = value_name)
    
    tool.survey <- tool_cl %>%
      mutate(q.type = type,
             type = paste(list_name, name)) %>%
      select(type,name,starts_with('label'), any_of(c('calculation','required','relevant', 'appearance', 'choice_filter')),
             starts_with('constraint'),
             starts_with('hint'),list_name, kuid)
    
    # write the tool into an excel
    wb <- createWorkbook()
    
    addWorksheet(wb, "survey")
    addWorksheet(wb, "choices")
    
    writeData(wb, "survey", tool.survey)
    writeData(wb, "choices", tool.choices)
    
    saveWorkbook(wb, directory_dictionary$filename.tool, overwrite = TRUE)
    
    
  }else{
    warning('The tool has already been uploaded into the resources folder and will not be loaded from Kobo server.
            If you want to load it from the server, remove the tool from the resources folder or rename it.')
  }
  
  # load the data
  if(!file.exists(paste0(directory_dictionary$data_path,directory_dictionary$data_name))){
    df <- as.list(kobo_submissions(assest_uid))
    write.xlsx(df,paste0(directory_dictionary$data_path,directory_dictionary$data_name))
    
  }else{
    warning('The data has already been uploaded into the kobo_export folder and will not be loaded from Kobo server.
            If you want to load it from the server, remove the data from the folder.')
  }
  
}



# load TOOL
cat("\n- LOADING tool ...\n")

cat("\nLoading Kobo tool from file", directory_dictionary$filename.tool, "...\n")
label_colname <- utilityR::load.label.colname(directory_dictionary$filename.tool)
tool.survey  <- utilityR::load.tool.survey(directory_dictionary$filename.tool, label_colname = directory_dictionary$label_colname)
tool.choices <- utilityR::load.tool.choices(directory_dictionary$filename.tool, label_colname = directory_dictionary$label_colname)

cat("..OK\n")


# load a single raw Kobo data export:

# and loads the data into kobo.raw.main, kobo.raw.loop1...
# also included are the standard steps of renaming uuid, and adding the loop_index

raw_data_filename <- paste0(directory_dictionary$data_path,directory_dictionary$data_name)

if(!file.exists(raw_data_filename)) {
  stop(paste0("No datafile with the name ",raw_data_filename,
              " found in the kobo_export folder."))
}else{
  
  ls <- excel_sheets(path = raw_data_filename)
  sheet_names <- if(length(ls)>1){
    c('kobo.raw.main',paste0('kobo.raw.loop',1:(length(ls)-1)))
  }else{
    'kobo.raw.main'
  }
  for(i in 1:length(ls)){
    if(i==1){
      kobo.raw.main <- readxl::read_xlsx(raw_data_filename, col_types = "text", sheet = ls[i]) %>%
        rename(uuid = "_uuid",
               submission_time = "_submission_time",
               index = "_index") %>%
        rename_all(~sub("_geolocation","geolocation", .x))
    }else{
      txt <- paste0(sheet_names[i],'=readxl::read_xlsx(raw_data_filename, col_types = "text", sheet = "',ls[i],'")%>%
    rename(uuid = "_submission__uuid",
           loop_index = "_index",
           parent_index = "_parent_index",
           submission_id = "_submission__id",
           submission_submission_time ="_submission__submission_time") %>%
    mutate(loop_index = paste0("loop',i-1,'_", loop_index))'
      )
      eval(parse(text = txt))
    }
  }
  
  dataset_creation_time <- as.Date(file.info(raw_data_filename)$ctime)
  dctime_short <- str_extract(gsub('-', '', str_sub(dataset_creation_time, 3)), "\\d+")
}


rm(raw_data_filename)
