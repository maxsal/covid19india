% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_nat_tests.R
\name{get_nat_tests}
\alias{get_nat_tests}
\title{Pull covid19india national time series test data}
\usage{
get_nat_tests(
  path = "https://data.covid19india.org/csv/latest/tested_numbers_icmr_data.csv",
  raw = FALSE
)
}
\arguments{
\item{path}{The URL path for the data. Default: \verb{https://api.covid19india.org/data.json}}

\item{raw}{Pull raw unaltered data. Default is \code{FALSE}}
}
\value{
Pulls the time-series test data directly from covid19india.org.
}
\description{
Pull covid19india national time series test data
}
\examples{
\dontrun{
get_nat_tests()
}

}
