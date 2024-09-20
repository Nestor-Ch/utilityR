non_gis_check <- TRUE
source('src/sections/section_2_3_x_helper_load_audits.R')

# ------------------------------testing ---------------------------



# macro <- read.xlsx('C:/Users/reach/Desktop/Git/CINM/data/MSNA2403_2024_final_anonymized_data_19July2024_cooccurence_added.xlsx') %>% 
#   select(uuid, macroregion)
# 
# 
# raw.main <- raw.main %>%
#   left_join(macro %>%
#               select(uuid,!!sym(geo_column)) %>%
#               distinct())


audits <- audits %>% left_join(
  raw.main %>% 
    select(uuid,!!sym(geo_column)) %>% 
    distinct()
  ) 


# test of individual interviews
issues <- data.frame()
for (lst in ls_group){

  res <- anomaly_test_tmln_int(ls = lst,audit = audits,data = raw.main,
                         geo_column = geo_column,enum_name =directory_dictionary$enum_colname)
  
  issues <- rbind(res, issues)
}


# test of enumerators
issues_test <- data.frame()
for (lst in ls_group){

  res <- anomaly_test_enum_diffr(variable_list = lst,
                               audit = audits, 
                               data_frame = raw.main, 
                               enum_name = directory_dictionary$enum_colname, 
                               geo_column = geo_column)
  
  issues_test <- rbind(res, issues_test)
}

issues_full <- full_join(issues, issues_test) %>% 
  mutate(
         s1 = (perc_issues-mean(perc_issues, na.rm=T))/sd(perc_issues, na.rm=T),
         s2 = (issues_per_questions-mean(issues_per_questions, na.rm=T))/sd(issues_per_questions, na.rm=T),
         s3 = (problematic_interviews_perc-mean(problematic_interviews_perc, na.rm=T))/sd(problematic_interviews_perc, na.rm=T),

         s1 = ifelse(s1>0,s1,NA),
         s2 = ifelse(s2>0,s2,NA),
         s3 = ifelse(s3>0,s3,NA)
         ) %>% 
  rowwise() %>% 
  mutate(problem_definer = mean(c(s1,s2,s3), na.rm = T)) %>% 
  ungroup() %>% 
  select(-(starts_with('s')))
  

problematic_enums <- issues_full %>% 
  distinct(!!sym(directory_dictionary$enum_colname)) %>% 
  mutate(problematic_enumerator = TRUE) %>% 
  left_join(issues_full %>% 
              filter(problem_definer>2) %>% 
              distinct(!!sym(directory_dictionary$enum_colname)) %>% 
              mutate(problematic_enumerator_to_check = TRUE)) %>% 
  mutate(problematic_enumerator_to_check = problematic_enumerator_to_check %in% TRUE)


df_enum_tests <- list(
  'Problematic enumerators' = problematic_enums,
  'Interview duration problems' = issues %>% arrange(desc(issues_per_questions)),
  'Enumerator duration problems' = issues_test %>% arrange(desc(perc_issues))
)

write.xlsx(paste0(directory_dictionary$dir.audits.check,'Enumerator_anomalies.xlsx'))



