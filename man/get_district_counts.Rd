% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_district_counts.R
\name{get_district_counts}
\alias{get_district_counts}
\title{Pull covid19india district-level data}
\usage{
get_district_counts(
  path = "https://api.covid19india.org/csv/latest/districts.csv",
  raw = FALSE
)
}
\arguments{
\item{path}{The URL path for the data. Default: \verb{https://api.covid19india.org/csv/latest/districts.csv}}

\item{raw}{Pull raw unaltered data. Default is \code{FALSE}}
}
\value{
Pulls the district-level time-series case, death, and recovered data directly from covid19india.org.
}
\description{
Pull covid19india district-level data
}
\examples{
\dontrun{
get_district_counts()
}
}
