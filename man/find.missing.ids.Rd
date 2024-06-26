% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find.R
\name{find.missing.ids}
\alias{find.missing.ids}
\title{Find ids that are missing from the dataframe}
\usage{
find.missing.ids(data, uniquis, print_warnings = T, is.loop = F)
}
\arguments{
\item{data}{Your dataframe}

\item{uniquis}{uuids or loop indeces that you're trying to double-check with the data}

\item{print_warnings}{whether the function should print any warnings}

\item{is.loop}{Whether the data and the uniquis you're feeding it belong to a loop}
}
\value{
Returns a list of uniquis that are missing from your data
}
\description{
Find ids that are missing from the dataframe
}
\examples{
\dontrun{
find.missing.ids(data, uniquis = uuid_list)
}
}
