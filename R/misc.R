#' Equality checker
#'
#' @param a - a numeric/string variable present in the environment
#' @param b - a numeric/string variable present in the environment
#'
#' @return - A boolean for the equality check
#' @export
#'
#' @examples
#' a <- 2
#' a %==% 2
"%==%" <- function(a, b){
  ifelse(!is.na(a), a==b, F)
}




#' Inequality checker
#'
#' @param a - a numeric/string variable present in the environment
#' @param b - a numeric/string variable present in the environment
#'
#' @return - A boolean for the inequality check
#' @export
#'
#' @examples
#' a <- 3
#' a %!=% 2
"%!=%" <- function(a, b){
  ifelse(!is.na(a), a!=b, F)
}
