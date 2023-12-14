#' Looking for outliers in the provided data
#'
#' @param df Dataframe containing columns which will be observed on outliers
#' @param id row's unique identifier
#' @param n.sd number of standard deviations will be used in algorithm
#' @param method method for outliers detecting: o1, o2, o3, o4. More detailed about methods below
#' @param is.loop Is df a loop data or not
#' @param colnames columns which will be observed on outliers
#' @param ignore_0 ignore 0 values or not, should be TRUE in o2 method, and in another too

#' @returns DataFrame containing outliers
#'
#' @examples
#' \dontrun{
#' raw.main.outliers <- detect.outliers(
#'  df=raw.main,
#'  id='uuid',
#'  colnames=cols.integer_raw.main,
#'  is.loop=F,
#'  n.sd=n.sd,
#'  method="o1"
#'  ignore_0=T)
#' }
#'
#' @method o1 Method based on Z score and logarithmization of the values
#' @method o2 Modified Z score which based on the median absolute deviation, recommended n.sd
#' @method o3 Method based on the interquartile range
#' @method o4 Method based on the median absolute deviation
#'
#' @note o2 method has some bias for the lowest values
#'

detect.outliers <- function(df, id, n.sd, method="o1", is.loop, colnames, ignore_0=T){

  if (is.loop == TRUE){
    if(!"loop_index" %in% colnames(df)) {
      stop("uniquis are loop indexes, but data does not contain column loop_index!")
    }
  } else {
    if ("loop_index" %in% colnames(df)) {
      stop("uniquis are not loop indexes, but data contains column loop_index!")
    }
  }
  if (!(id %in% colnames(df))) {
    stop(paste("data does not contain column", id, " or uuid!"))
  }

  if (!("uuid" %in% colnames(df))) {
    stop(paste("data does not contain column uuid !"))
  }

  if (!(all(colnames %in% colnames(df)))) {
    stop(paste("data does not contain all columns from colnames list"))
  }

  for (col in colnames) {
    can_convert <- suppressWarnings(all(!is.na(as.numeric(na.omit(df[[col]])))))
    if (!can_convert) {
      stop(paste("Not all values in the column", col, "could be converted to the numeric value!"))
    }
  }
  res <- data.frame()
  for (col in colnames){
    df.temp <- data.frame(uuid = df$uuid ,id=df[[id]], value=as.numeric(df[[col]])) %>%
      dplyr::filter(!is.na(value))
    if (ignore_0) {
      df.temp <- df.temp %>% dplyr::filter(value > 0)
    }
    if (method == "o1") {
      df.temp <- df.temp %>% dplyr::mutate(check.value = ifelse(value==0, 1, value))
    } else {
      df.temp <- df.temp %>% dplyr::mutate(check.value = value)
    }
    df.temp <- df.temp %>%
      dplyr::mutate(col.log=log10(check.value),
             is.outlier=case_when(
               method=="o1" ~ ifelse(col.log > (mean(col.log, na.rm=T) + n.sd * sd(col.log, na.rm=T)) |
                                       col.log < (mean(col.log, na.rm=T) - n.sd * sd(col.log, na.rm=T)) |
                                       value > (mean(check.value, na.rm=T) + n.sd * sd(check.value, na.rm=T)) |
                                       value < (mean(check.value, na.rm=T) - n.sd * sd(check.value, na.rm=T)), T, F),
               method=="o2" ~ abs(0.6745 * (check.value - median(check.value, na.rm = T)) / median(abs(check.value - median(check.value, na.rm = T)), na.rm = T)) > n.sd,
               method=="o3" ~ ifelse(check.value > quantile(check.value, 0.75) +
                                       1.5 * (quantile(check.value, 0.75) - quantile(check.value, 0.25)) |
                                       value < quantile(check.value, 0.25) -
                                       1.5 * (quantile(check.value, 0.75) - quantile(check.value, 0.25)), T, F),
               method=="o4" ~ ifelse(check.value > median(check.value) + n.sd * mad(check.value) |
                                       check.value < median(check.value) - n.sd * mad(check.value), T, F)),
             variable=col) %>%
      dplyr::filter(is.outlier) %>%
      dplyr::mutate(issue="Outlier", old.value=value, new.value=NA)
    res <- rbind(res, df.temp)
  }
  if(is.loop == F){
    res <- res %>%
      dplyr::mutate(loop_index = NA) %>%
      select(-id)
  }else{
    res <- res %>%
      rename(loop_index = id)
  }
  res <- res %>%
    dplyr::select(uuid, loop_index,issue, variable, old.value, new.value)

  return(res)
}

#' Visualisation function for the outliers
#'
#' @param outliers.list List of DataFrames from the detect.outliers function
#' @param raw.data_frames.list List of raw DataFrames in which outliers were searched, such DataFrames sequence should match outliers in the outliers.list
#' @param columns.list List of colnames from the detect.outliers that contains columns were observed on outliers
#' @param n.sd number of standard deviations from the mean
#' @param boxplot.path path for saving outliers visualisation
#'
#' @examples
#' \dontrun{
#' generate.boxplot(outliers.list=list(raw.main.outliers, raw.loop2.outliers, raw.loop3.outliers),
#'                 raw.data_frames.list=list(raw.main, raw.loop2, raw.loop3),
#'                 columns.list=list(cols.integer_raw.main, cols.integer_raw.loop2, cols.integer_raw.loop3),
#'                 n.sd=n.sd, boxplot_path = "output/checking/outliers/main_outlier_prices_analysis_")
#' }
#'
#' @note be careful with order of your input lists, outliers and colnames should match raw.data

generate.boxplot <- function(outliers.list, raw.data_frames.list, columns.list, n.sd,
                             boxplot.path="output/checking/outliers/main_outlier_prices_analysis_") {
  if (length(outliers.list) != length(raw.data_frames.list) ||
      length(raw.data_frames.list) != length(columns.list) ||
      length(outliers.list) != length(columns.list)) {
    stop("Length of input lists are mismatching")
  }

  # merging block
  all_outliers <- list()
  for (i in seq_along(outliers.list)) {
    outliers.data_frame <- outliers.list[[i]]
    raw.data_frame <- raw.data_frames.list[[i]]
    columns <- columns.list[[i]]

    if (!("variable" %in% colnames(outliers.data_frame)) ||
        !("old.value" %in% colnames(outliers.data_frame)) ||
        !("issue" %in% colnames(outliers.data_frame))) {
          stop("Wrong outliers DataFrame format")
    }

    if (!("loop_index" %in% colnames(raw.data_frame))) {
      id <- "uuid"
      if (sum(!is.na(outliers.data_frame$loop_index) != 0)) {
        stop("Mismatching between raw data frame and outliers data frame!")
      }
    } else {
      if (sum(!is.na(outliers.data_frame$loop_index) == 0)) {
        stop("Mismatching between raw data frame and outliers data frame!")
      }
      id <- "loop_index"
    }

    outliers_output <- raw.data_frame %>%
      dplyr::select(!!sym(id), all_of(columns)) %>%
      dplyr::mutate(across(all_of(columns), as.numeric)) %>%
      pivot_longer(cols = all_of(columns), names_to = 'variable', values_to = 'value') %>%
      dplyr::filter(!is.na(value) & value > 0) %>%
      left_join(
        outliers.data_frame %>% dplyr::select(!!sym(id), variable, old.value, issue) %>%
          rename(is.outlier = issue,
                 value = old.value)
      ) %>% dplyr::mutate(is.outlier = ifelse(is.na(is.outlier), 'Regular', as.character(is.outlier)))

    all_outliers <- bind_rows(all_outliers, outliers_output)
  }

  g.outliers_main <- ggplot(all_outliers) +
    geom_boxplot(aes(x = variable, y = value), width = 0.2) + ylab("Values") +
    geom_point(aes(x = variable, y = value, shape = is.outlier, color = is.outlier, fill = is.outlier), size = 2) +
    facet_wrap(~variable, ncol = 4, scales = "free_y") +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    scale_shape_manual(values = c(21, 16)) +
    scale_color_manual(values = c('red', 'black')) +
    scale_fill_manual(values = c('red', 'black'))

  # generation
  ggsave(paste0(boxplot.path, n.sd, "sd.pdf"), g.outliers_main,
         width = 40, height = 80, units = "cm", device = "pdf")
}
