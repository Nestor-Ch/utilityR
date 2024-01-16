cleaning.log.outliers <- data.frame()

cols.integer_main <- filter(tool.survey, type == "integer")
# get the integer columns in main
if (length(cols.integer_raw.main) == 0) cols.integer_raw.main <- cols.integer_main[cols.integer_main$name %in% colnames(raw.main),] %>% pull(name)
# set up empty lists
outliers.list <- c()
raw.data_frames.list <- c()
columns.list <- c()


if (length(cols.integer_raw.main) != 0) {
  # get the outliers
  raw.main.outliers <- utilityR::detect.outliers(
    df = raw.main,
    id = 'uuid',
    colnames = cols.integer_raw.main,
    is.loop = F,
    n.sd = n.sd,
    method = method,
    ignore_0=ignore_0)
  #bind them to the list of outliers
  outliers.list <- c(outliers.list, list(raw.main.outliers))
  # bind the dataframe to the list to feed into the visualization function
  raw.data_frames.list <- c(raw.data_frames.list, list(raw.main))
  # create a list of integer columns
  columns.list <- c(columns.list, list(cols.integer_raw.main))
} else raw.main.outliers <- data.frame()

cleaning.log.outliers <- rbind(raw.main.outliers,cleaning.log.outliers)

# same for loops
if(length(sheet_names_new)>0){
  for(i in 1:length(sheet_names_new)){
    txt <- paste0('if (length(cols.integer_raw.loop',i,') == 0){cols.integer_raw.loop',i,' <- cols.integer_main[cols.integer_main$name %in% colnames(raw.loop',i,'),] %>% pull(name)}')
    eval(parse(text=txt))
    
    txt <- paste0(
      'if (length(cols.integer_raw.loop',i,') != 0) {
      raw.loop',i,'.outliers <- utilityR::detect.outliers(
      df = raw.loop',i,',
      id = "loop_index",
      colnames = cols.integer_raw.loop',i,',
      is.loop = T,
      n.sd = n.sd,
      method = method,
      ignore_0=ignore_0)
      outliers.list <- c(outliers.list, list(raw.loop',i,'.outliers))
      raw.data_frames.list <- c(raw.data_frames.list, list(raw.loop',i,'))
      columns.list <- c(columns.list, list(cols.integer_raw.loop',i,'))
      } else raw.loop',i,'.outliers <- data.frame()'
    )
    eval(parse(text=txt))
    
    
  }}

# generate the boxplot
utilityR::generate.boxplot(outliers.list=outliers.list,
                           raw.data_frames.list=raw.data_frames.list,
                           columns.list=columns.list,
                           n.sd=n.sd, boxplot.path = "output/checking/outliers/outlier_analysis_")

# bind the cleaning logs for loops
if(length(sheet_names_new)>0){
  for(i in 1:length(sheet_names_new)){
    txt <- paste0('cleaning.log.outliers <- rbind(cleaning.log.outliers,raw.loop',i,'.outliers)')
    eval(parse(text=txt))
  }}

