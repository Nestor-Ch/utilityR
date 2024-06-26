% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process.R
\name{process.uuid}
\alias{process.uuid}
\title{Process audit files}
\usage{
process.uuid(df)
}
\arguments{
\item{df}{Audit files loaded with `load.audit.files` function}
}
\value{
A dataframe describing time-related parameters of the interviews\cr
- \code{n.iteration} - The number of iterations per interviews \cr
- \code{tot.t} - Total time of the interview. Calculated as `start` of the last `form.exit` event minus
the `start` of `form.start` event \cr
- \code{tot.rt} - The total response time of the interview. The sum of the `duration`
column in the loaded audits dataframe \cr
- \code{tot.rt.inter} - The total time between questions in the interview.
The sum of the `inter_q_duration` column in the loaded audits dataframe\cr
- \code{t} - Time of each iteration. Calculated as `start` of the iteration's `form.exit` event
minus the `start` of the iterations`form.start` event\cr
- \code{rt} - Response time of each iteration. The sum of the `duration` column in the loaded audits dataframe for the iteration\cr
- \code{q} - Number of questions per iteration\cr
- \code{j} - Number of jump events per iteration\cr
- \code{e} - Number of edits per iteration Calculated as the number of non NA entries in the `old.value` column\cr
- \code{w} - Waiting time - the `start` column of iteration's `form.resume`event - the `start`\cr
- \code{tag} - If you've pre-processed files, this column will tag the uuid-question pairs that were outside of the set threshold
for the column of the pervious iterations `form.exit` event
}
\description{
Process audit files
}
\note{
This function is non vectorized. Mostly used in pipes after a `group_by(uuid)` command
}
\examples{
\dontrun{
loaded.audits \%>\%
  dplyr::group_by(uuid) \%>\%
  dplyr::group_modify(~process.uuid(.x)) \%>\%
  dplyr::ungroup()
}
}
