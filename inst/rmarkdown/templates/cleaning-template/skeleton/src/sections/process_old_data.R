
# uuids_to_remove <- c()

# update this to the latest data log for this country or leave as-is
# filename_dataset_previous <- "output/data_log/..."
# 
# if(filename_dataset_previous != "output/data_log/..."){
#   main.data.previous <- read_xlsx(filename_dataset_previous, col_types = "text", sheet = 1)
#   loop1.previous <- read_xlsx(filename_dataset_previous, col_types = "text", sheet = 2)
#   loop2.previous <- read_xlsx(filename_dataset_previous, col_types = "text", sheet = 3)
#   uuids_to_remove <- main.data.previous$uuid
# }
# 
# deletion.log.previous <- load.requests("output/deletion_log", paste0(country_short, ".*_deletion_log"))
# uuids_to_remove <- c(uuids_to_remove, deletion.log.previous$uuid)

# ########################################
# ##  load & remove previous data        ##
# raw.main <- kobo.raw.main %>%          ##
#   filter(!(uuid %in% uuids_to_remove))
# raw.loop1 <- kobo.raw.loop1 %>%        ##
#   filter(!(uuid %in% uuids_to_remove))
# raw.loop2 <- kobo.raw.loop2 %>%        ##
#   filter(!(uuid %in% uuids_to_remove))
#                                        ##