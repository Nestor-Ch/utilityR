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


#' Sum of two numerics
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

#' Compare two variables are the same and are not NA
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




#' Check variables are non-equal and not NA
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

#' A shorthand is NA function
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












