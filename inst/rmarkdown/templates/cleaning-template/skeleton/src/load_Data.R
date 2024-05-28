# load a single raw Kobo data export:

# and loads the data into kobo.raw.main, kobo.raw.loop1...
# also included are the standard steps of renaming uuid, and adding the loop_index

raw_data_filename <- list.files(directory_dictionary$data_path, full.names = T, pattern = 'xlsx$')

if(length(raw_data_filename) > 1) { stop("Found multiple files containing raw Kobo data! Please clean up the kobo_export folder.")
}else if(length(raw_data_filename) == 0){
  warning("Raw Kobo data not found!")
  kobo.raw.main <- data.frame()
  kobo.raw.loop1 <- data.frame()
  dataset_creation_time <- NA
  dctime_short <- ""
}else if(length(raw_data_filename) == 1){

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
