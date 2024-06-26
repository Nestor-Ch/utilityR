% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find.R
\name{analyse.similarity}
\alias{analyse.similarity}
\title{Similarity analysis function}
\usage{
analyse.similarity(outdata, enum.column, visualise = F, boxplot.path = "")
}
\arguments{
\item{outdata}{Result from the find.similar.surveys function}

\item{enum.column}{Name of column that represent unique identifier for the enumerators}

\item{visualise}{Whether to render visualisation plot or not}

\item{boxplot.path}{path for storing visualisation file}
}
\value{
Returns analysis and outliers data frames.
Analysis data frame contains grouped information for every enumerators,
while outliers data frame about enumerators identified as outliers in the analysis.
}
\description{
Similarity analysis function
}
\examples{
\dontrun{
result.analysis <- analyse.similarity(
 outdata=outdata,
 enum.column='a2_1_enum_id',
 visualise=T,
 boxplot.path = "path/to/store/visualisation")
 analysis <- result.analysis$analysis
 outliers <- result.analysis$outliers
}
}
