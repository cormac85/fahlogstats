% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process.R
\name{get_daily_duration}
\alias{get_daily_duration}
\title{Get Daily Duration}
\usage{
get_daily_duration(parsed_log)
}
\arguments{
\item{parsed_log}{A tibble of FAH Client logs that are parsed
(\code{read_fah_logs()}) and cleaned (\code{clean_logs()}).}
}
\value{
A dataframe representing the total active log hours per day,
i.e. how long the FAH Client was active, logging and could have been
folding if work was available.
}
\description{
Calculates the total length of time (in hours) that the FAH Client
was active per day.
}
\examples{
read_fah_logs("~/../AppData/Roaming/FAHClient/logs/") \%>\%
  clean_logs() \%>\%
  get_total_log_duration() \%>\%
  paste("Total time client open:", ., "hours")
}
