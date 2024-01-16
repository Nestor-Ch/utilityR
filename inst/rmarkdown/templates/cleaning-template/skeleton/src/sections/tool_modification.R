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