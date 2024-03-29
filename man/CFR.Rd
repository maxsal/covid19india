% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_cfr.R
\name{CFR}
\alias{CFR}
\title{Helper function for calculating case-fatality rate}
\usage{
CFR(C, D)
}
\arguments{
\item{C}{Cases vector}

\item{D}{Deaths vector}
}
\value{
A case-fatality rate estimate and 95\% confidence limits
}
\description{
Helper function for calculating case-fatality rate
}
\examples{
\dontrun{
national_data <- get_nat_counts()
CFR(C = national_data$total_cases, D = national_data$total_deaths)
}


}
\keyword{internal}
