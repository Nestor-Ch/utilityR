% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find.R
\name{find.relevances}
\alias{find.relevances}
\title{Find the questions that have the questions from `var_list` as parents in their relevances}
\usage{
find.relevances(tool.survey, var_list)
}
\arguments{
\item{tool.survey}{your tool.survey object}

\item{var_list}{the list of variables you want to check}
}
\description{
Find the questions that have the questions from `var_list` as parents in their relevances
}
\note{
this function won't work if you have multiple relevance conditions in your rows.
They will be filtered out
}
\examples{
\dontrun{
relevancy_dictionary <- find.relevances(
 tool.survey=tool.survey,
 var_list=c('variable1','variable2'))
}
}
