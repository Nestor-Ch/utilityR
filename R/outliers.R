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

  can_convert <- suppressWarnings(all(sapply(colnames, function(col) all(!is.na(as.numeric(na.omit(df[[col]])))))))
  if (!can_convert) {
    stop("Not all values in the specified columns could be converted to the numeric value!")
  }

  res <- data.frame()
  for (col in colnames){
    df.temp <- data.frame(uuid = df$uuid ,id=df[[id]], value=as.numeric(df[[col]])) %>% filter(!is.na(value) & value>0)
    if (ignore_0 & method == "o2") {
      df.temp <- df.temp %>% mutate(value = ifelse(value==0, 1, value))
    }
    df.temp <- df.temp %>%
      mutate(col.log=log10(value),
             is.outlier=case_when(
               method=="o1" ~ abs(0.6745 * (value - median(value, na.rm = T)) / median(abs(value - median(value, na.rm = T)), na.rm = T)) > n.sd,
               method=="o2" ~ ifelse(col.log > (mean(col.log, na.rm=T) + n.sd * sd(col.log, na.rm=T)) |
                                       col.log < (mean(col.log, na.rm=T) - n.sd * sd(col.log, na.rm=T)), T, F),
               method=="o3" ~ ifelse(value > quantile(value, 0.75) +
                                       1.5 * (quantile(value, 0.75) - quantile(value, 0.25)) |
                                       value < quantile(value, 0.25) -
                                       1.5 * (quantile(value, 0.75) - quantile(value, 0.25)), T, F),
               method=="o4" ~ ifelse(value > median(value) + 3 * mad(value) |
                                       value < median(value) - 3 * mad(value), T, F)),
             variable=col) %>%
      filter(is.outlier) %>%
      mutate(issue="Outlier", old.value=value, new.value=NA)
    res <- rbind(res, df.temp)
  }
  if(is.loop == F){
    res <- res %>%
      mutate(loop_index = NA) %>%
      select(-id)
  }else{
    res <- res %>%
      rename(loop_index = id)
  }
  res <- res %>%
    select(uuid, loop_index,issue, variable, old.value, new.value)

  return(res)
}

generate.boxplot <- function(outliers.list, raw.data_frames.list, columns.list, n.sd,
                             boxplot_path="output/checking/outliers/main_outlier_prices_analysis_") {
  # merging block
  all_outliers <- list()
  for (i in seq_along(outliers.list)) {
    outliers.data_frame <- outliers.list[[i]]
    raw.data_frame <- raw.data_frames.list[[i]]
    columns <- columns.list[[i]]

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
      dplyr::select(!!sym(id), columns) %>%
      dplyr::mutate(across(all_of(columns), as.numeric)) %>%
      pivot_longer(cols = all_of(columns), names_to = 'variable', values_to = 'value') %>%
      filter(!is.na(value) & value > 0) %>%
      dplyr::mutate(value.log = log10(value)) %>%
      left_join(
        outliers.data_frame %>% dplyr::select(!!sym(id), variable, old.value, issue) %>%
          rename(is.outlier = issue,
                 value = old.value)
      ) %>% dplyr::mutate(is.outlier = ifelse(is.na(is.outlier), 'Regular', as.character(is.outlier)))

    all_outliers <- bind_rows(all_outliers, outliers_output)
  }

  # visualization
  g.outliers_main <- ggplot(all_outliers) +
    geom_boxplot(aes(x = variable, y = value), width = 0.2) + ylab("Values") +
    geom_point(aes(x = variable, y = value, colour = is.outlier)) +
    facet_wrap(~variable, ncol = 4, scales = "free_y") +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    scale_color_manual(values = c('red', 'black'))
  # generation
  ggsave(paste0(boxplot_path, n.sd, "sd.pdf"), g.outliers_main,
         width = 40, height = 80, units = "cm", device = "pdf")
}

