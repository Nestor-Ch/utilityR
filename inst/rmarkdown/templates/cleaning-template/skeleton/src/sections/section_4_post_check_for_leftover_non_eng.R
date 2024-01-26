id_columns <- c('uuid','loop_index')

cyrillic.main <- raw.main %>% 
  select(-any_of(vars_to_omit)) %>% 
  pivot_longer(cols= names(raw.main)[!names(raw.main) %in% vars_to_omit], names_to = 'variable', values_to = 'value') %>% 
  filter(grepl("[^ -~]",value, perl=T)) %>% 
  rowwise() %>% 
  mutate(cyrillic_char = regmatches(value, 
                                    gregexpr("[^ -~]", 
                                             value, perl = TRUE)) %>%unlist  %>%  paste(collapse = ''))

if(nrow(cyrillic.main)>0){
  warning('Your data sill has cyrillic entries, please check file cyrillic.main for details')
}


if(length(sheet_names_new)>0){
  for(i in sheet_names_new){
    txt <- paste0('cyrillic.',i,' <- ',i,' %>% 
    select(-any_of(vars_to_omit)) %>% 
    pivot_longer(cols= names(',i,')[!names(',i,') %in% vars_to_omit], names_to = "variable", values_to = "value") %>% 
                  filter(grepl("[^ -~]",value, perl=T))%>% 
                  rowwise() %>% 
                  mutate(cyrillic_char = regmatches(value, 
                                    gregexpr("[^ -~]", 
                                             value, perl = TRUE)) %>%unlist  %>%  paste(collapse = ""))')
    eval(parse(text=txt))
    txt <- paste0('if(nrow(cyrillic.',i,')>0){
    warning("Your data sill has cyrillic entries, please check file cyrillic.',i,' for details")
    }')
    eval(parse(text=txt))
  }
}
