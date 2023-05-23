#' Equality checker
#'
#' @param a - a  variable present in the environment
#' @param b - a  variable present in the environment
#'
#' @return - A boolean for the equality check
#' @export
#'
#' @examples
#' a <- 2
#' a %==% 2
"%==%" <- function(a, b){
  ifelse(!is.na(a), a == b, F)
}




#' Inequality checker
#'
#' @param a - a variable present in the environment
#' @param b - a variable present in the environment
#'
#' @return - A boolean for the inequality check
#' @export
#'
#' @examples
#' a <- 3
#' a %not=% 2
"%not=%" <- function(a, b){
  ifelse(!is.na(a), a != b, F)
}


#' Check if a is greater than b
#'
#' @param a - a variable present in the environment
#' @param b - a variable present in the environment
#'
#' @return - A boolean for the check
#' @export
#'
#' @examples
#' a <- 3
#' a %_>_% 2
"%_>_%" <- function(a, b){
  ifelse(!is.na(a), as.numeric(a)>b, F)
}


#' Check if a is greater than or equal to b
#'
#' @param a - a variable present in the environment
#' @param b - a variable present in the environment
#'
#' @return - A boolean for the check
#' @export
#'
#' @examples
#' a <- 5
#' a %_>=_% 2
"%_>=_%" <- function(a, b){
  ifelse(!is.na(a), as.numeric(a)>=b, F)
}


#' Check if a is less than b
#'
#' @param a - a variable present in the environment
#' @param b - a variable present in the environment
#'
#' @return - A boolean for the check
#' @export
#'
#' @examples
#' a <- 5
#' a %_<_% 6
"%_<_%" <- function(a, b){
  ifelse(!is.na(a), as.numeric(a)<b, F)
}


#' Check if a is less than or equal to b
#'
#' @param a - a variable present in the environment
#' @param b - a variable present in the environment
#'
#' @return - A boolean for the check
#' @export
#'
#' @examples
#' a <- 5
#' a %_<=_% 7
"%_<=_%" <- function(a, b){
  ifelse(!is.na(a), as.numeric(a)<=b, F)
}


#' Produce a sum of two numeric variables
#'
#' @param a - a variable present in the environment
#' @param b - a variable present in the environment
#'
#' @return  - A numeric sum
#' @export
#'
#' @examples
#' a <- 4
#' a %_+_% 2
"%_+_%" <- function(a, b){
  as.numeric(a) + as.numeric(b)
}

#' Check if two variables are the same and are not NA
#'
#' @param e1 - a variable present in the environment
#' @param e2 - a variable present in the environment
#'
#' @return - a boolean result of the comparison
#' @export
#'
#' @examples
#' a <- 4
#' b <- 5
#' a %==na% b
"%==na%" <- function(e1, e2){
  ifelse(is.na(e1 == e2), is.na(e1) == is.na(e2), e1 == e2)
}




#' Check if two variables are non-equal and not NA
#'
#' @param e1 - a variable present in the environment
#' @param e2 - a variable present in the environment
#'
#' @return - a boolean result of the comparison
#' @export
#'
#' @examples
#' a <- 4
#' b <- 5
#' a %not=na% b
"%not=na%" <- function(e1, e2){
  (e1 != e2 | (is.na(e1) & !is.na(e2)) | (is.na(e2) & !is.na(e1))) & !(is.na(e1) & is.na(e2))
}

#' A shorthand for is.na() function
#'
#' @param x - a variable present in the environment
#'
#' @return - a boolean of whether X was NA
#' @export
#'
#' @examples
#' a <- 2
#' isna(2)
isna <- function(x){
  is.na(x)
}



#' Collapse a list of multiple choice answers into a more readable format
#'
#' @param choices - a character variable in your environment
#'
#' @return a single character string composed of the previous vector in a readable format
#' @export
#'
#' @examples
#' choices <- c("a",2,"s","f","a")
#' anychoice_pattern(choices)
anychoice_pattern <- function(choices){
  paste0("(",choices,")", collapse = "|")
}


#' Transpose dataframes, keep colnames and rownames
#'
#' @param df - your dataframe
#'
#' @return - your transposed dataframe
#' @export
#'
#' @examples
#' library(dplyr)
#' df <-  data.frame(col1 = 1:2, col2 = 3:4, row.names = letters[1:2])
#' transpose_df(df)
transpose_df <- function(df) {
  t_df <- data.table::transpose(df)
  colnames(t_df) <- rownames(df)
  rownames(t_df) <- colnames(df)
  t_df <- t_df %>%
    tibble::rownames_to_column() %>%
    tibble::as_tibble()
  return(t_df)
}



#' Factorize a variable
#'
#' @param x - vector to be transformed
#' @param min_freq - all levels less than this percentage of records will be bucketed
#' @param min_n - all levels less than this number of records will be bucketed
#' @param NA_level - name of the level created for NA values
#' @param blank_level - name of the level created for "" values
#' @param infrequent_level - name of the level created for bucketing rare values
#' @param infrequent_can_include_blank_and_NA - If TRUE, infrequent_level will be extended to include NA and black observations
#' @param order # default to ordered
#' @param reverse_order  # default to increasing order
#'
#' @return An ordered factor variable
#' @export
#'
#' @examples
#' a <- letters[1:10]
#' factorize(a)
factorize <- function(
    x,  # vector to be transformed
    min_freq = .01,  # all levels < this % of records will be bucketed
    min_n = 1,  # all levels < this # of records will be bucketed
    NA_level = '(NA)',  # level created for NA values
    blank_level = '(blank)',  # level created for "" values
    infrequent_level = 'Other',  # level created for bucketing rare values
    infrequent_can_include_blank_and_NA = F,  # default NA and blank are not bucketed
    order = T,  # default to ordered
    reverse_order = F  # default to increasing order
) {
  if (class(x) != 'factor'){
    x <- as.factor(x)
  }
  # suspect this is faster than reassigning new factor object
  levels(x) <- c(levels(x), NA_level, infrequent_level, blank_level)

  # Swap out the NA and blank categories
  x[is.na(x)] <- NA_level
  x[x == ''] <- blank_level

  # Going to use this table to reorder
  f_tb <- table(x, useNA = 'always')

  # Which levels will be bucketed?
  infreq_set <- c(
    names(f_tb[f_tb < min_n]),
    names(f_tb[(f_tb/sum(f_tb)) < min_freq])
  )

  # If NA and/or blank were infrequent levels above, this prevents bucketing
  if(!infrequent_can_include_blank_and_NA){
    infreq_set <- infreq_set[!infreq_set %in% c(NA_level, blank_level)]
  }

  # Relabel all the infrequent choices
  x[x %in% infreq_set] <- infrequent_level

  # Return the reordered factor
  stats::reorder(droplevels(x), rep(1-(2*reverse_order),length(x)), FUN = sum, order = order)
}






