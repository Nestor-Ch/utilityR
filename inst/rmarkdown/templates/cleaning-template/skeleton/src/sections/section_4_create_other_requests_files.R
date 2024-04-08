
# get all of the other questions
other.db <- utilityR::get.other.db(tool.choices = tool.choices,
                                   tool.survey = tool.survey,
                                   label_colname = label_colname)


# Separate the other questions files by loop
other.db.main  <- other.db[other.db$name %in% colnames(raw.main),]
# find _other responses in main
other.responses <- utilityR::find.responses(raw.main, other.db.main)

# same process for loops
if(length(sheet_names_new)>0){
  for(loop in 1:length(sheet_names_new)){
    txt <- paste0('other.db.loop',loop,'<- other.db[other.db$name %in% colnames(raw.loop',loop,'),]')
    eval(parse(text=txt))
    
    txt <- paste0(
      'if(nrow(other.db.loop',loop,')>0){
      other.responses.loop',loop,' <- utilityR::find.responses(raw.loop',loop,', other.db.loop',loop,', is.loop = T)
      }else{other.responses.loop',loop,' <- data.frame()}'
    )
    eval(parse(text=txt))
    
    txt <- paste0('other.responses <- rbind(other.responses, other.responses.loop',loop,')')
    
    eval(parse(text=txt))
    
  }
}

if(nrow(other.responses)>0){
  
# translate your data
other.responses.j <- utilityR::translate.responses(responses = other.responses,
                                                   values_from = 'responses',
                                                   directory = directory_dictionary$dir.requests,
                                                   api.key = api_key
)

utilityR::save.other.requests(utilityR::create.translate.requests(other.responses.j),
                              directory = directory_dictionary$dir.requests,
                              make.short.name("other_requests_final"), use_template = F)
}

# ------------------------------------------------------------------------------

trans.responses <- data.frame()


trans.db <- utilityR::get.trans.db(tool.choices = tool.choices,
                                   tool.survey = tool.survey,
                                   label_colname = label_colname) %>% 
  filter(!name %in% trans_cols_to_skip)


# if there are any variables that have text columns and are missing from trans DB - add them here
missing_vars <- data.frame(name = c('xxxx',
                                    'xxxx'),
                           label = c('xxxx',
                                     'xxxx'))

trans.db <- rbind(trans.db, missing_vars)


# separate all open questions by loop and locate the answers to these questions
trans.db.main <- trans.db[trans.db$name %in% colnames(raw.main),]
trans.responses.main <- utilityR::find.responses(raw.main, trans.db.main)

trans.responses <- rbind(trans.responses,trans.responses.main)

if(length(sheet_names_new)>0){
  for(loop in 1:length(sheet_names_new)){
    txt <- paste0('trans.db.loop',loop,'<- trans.db[trans.db$name %in% colnames(raw.loop',loop,'),]')
    eval(parse(text=txt))
    
    txt <- paste0(
      'if(nrow(trans.db.loop',loop,')>0){
      trans.responses.loop',loop,' <- utilityR::find.responses(raw.loop',loop,', trans.db.loop',loop,', is.loop = T)
      }else{trans.responses.loop',loop,' <- data.frame()}'
    )
    eval(parse(text=txt))
    
    txt <- paste0('trans.responses <- rbind(trans.responses, trans.responses.loop',loop,')')
    
    eval(parse(text=txt))
    
  }
}
if(nrow(trans.responses)>0){
# translate all of the responses
trans.responses.j <- utilityR::translate.responses(responses = trans.responses,
                                                   values_from = 'responses',
                                                   directory = directory_dictionary$dir.requests,
                                                   api.key = api_key)

# save the translation requests
utilityR::save.trans.requests(utilityR::create.translate.requests(trans.responses.j),
                              directory = directory_dictionary$dir.requests,
                              make.short.name("text_requests_final"), use_template = F)

}