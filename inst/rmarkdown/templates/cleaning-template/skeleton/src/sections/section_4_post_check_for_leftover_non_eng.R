id_columns <- c('uuid','loop_index')

non.eng.main <- raw.main %>%
  select(-any_of(vars_to_omit)) %>%
  pivot_longer(cols= names(raw.main)[!names(raw.main) %in% vars_to_omit], names_to = 'variable', values_to = 'value') %>%
  filter(grepl("[^ -~]",value, perl=T)) %>%
  rowwise() %>%
  mutate(non_eng_char = regmatches(value,
                                    gregexpr("[^ -~]",
                                             value, perl = TRUE)) %>%unlist  %>%  paste(collapse = ''))

if(nrow(non.eng.main)>0){
  warning('Your data sill has non.eng entries, please check file non.eng.main for details')
}


if(length(sheet_names_new)>0){
  for(i in sheet_names_new){
    txt <- paste0('non.eng.',i,' <- ',i,' %>%
    select(-any_of(vars_to_omit)) %>%
    pivot_longer(cols= names(',i,')[!names(',i,') %in% vars_to_omit], names_to = "variable", values_to = "value") %>%
                  filter(grepl("[^ -~]",value, perl=T))%>%
                  rowwise() %>%
                  mutate(non_eng_char = regmatches(value,
                                    gregexpr("[^ -~]",
                                             value, perl = TRUE)) %>%unlist  %>%  paste(collapse = ""))')
    eval(parse(text=txt))
    txt <- paste0('if(nrow(non.eng.',i,')>0){
    warning("Your data sill has non.eng entries, please check file non.eng.',i,' for details")
    }')
    eval(parse(text=txt))
  }
}
