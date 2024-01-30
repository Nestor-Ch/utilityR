names_tool <- tool.survey %>% 
  filter(grepl('(select_one)|(select_multiple)|(integer)|(decimal)',type)) %>% pull(name)

names_list <- names(raw.main)

if(length(sheet_names_new)>0){
  for(frame in sheet_names_new){
    txt <- paste0("names(",frame,")")
    names_loop <- eval(parse(text=txt))
    names_list <- c(names_list,names_loop)
  }
}

names_missing <- setdiff(names_tool,names_list)

if(length(names_missing)>0){
  
  stop((paste0("some of the names in your tool are not present in your dataframe. Please double check if they were renamed: ",
                  paste0(names_missing,collapse = ',\n'))))
}

# # Find the question column in the XLSForm
# question_column <- "reasons_for_displacement_opt"
# new_option <- "cash_payments"
# new_option_label <- "Cash payments"
# 
# # Add the new option to the choices in the XLSForm
# new_choice <- paste0("list_name: reasons_for_displacement_opt, name: ", new_option, ", label::English: ", new_option_label)
# xlsform[[question_column]] <- c(xlsform[[question_column]], new_choice)
# 
# # Save the modified XLSForm
# write_xlsx(xlsform, "resources/tool.xlsx")
# 
# # Dataset modification
# dataset_file <- "path/to/your/dataset.xlsx"
# dataset <- readxl::read_xlsx(dataset_file)
# 
# # Find the corresponding column in the dataset
# dataset_column <- "a11_1_reasons_for_displacement"
# 
# # Add the new option to the dataset column
# dataset[[dataset_column]] <- c(dataset[[dataset_column]], new_option)

# Save the modified dataset
#write_xlsx(dataset, "path/to/modified/dataset.xlsx")