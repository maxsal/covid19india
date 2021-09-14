#' Pull covid19india national time series test data
#' @param path The URL path for the data. Default: `https://api.covid19india.org/data.json`
#' @param raw Pull raw unaltered data. Default is `FALSE`
#' @return Pulls the time-series test data directly from covid19india.org.
#' @import data.table
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom janitor clean_names
#' @importFrom stringr word
#' @export
#' @examples
#' \dontrun{
#' get_nat_tests()
#' }
#'

get_nat_tests <- function(
  path       = "https://api.covid19india.org/data.json",
  raw        = FALSE
) {

  request  <- httr::GET(path)
  json     <- httr::content(request)
  d        <- data.table::rbindlist(json[["tested"]])

  d <- d |>
    data.table::DT(, .(
      Cases = totalpositivecases,
      Tests = totalsamplestested,
      Date  = testedasof)) |>
    data.table::DT(, `:=` (
      Date    = as.Date(stringr::word(Date, 1), format = "%d/%m/%Y"),
      Cases   = as.numeric(gsub(",", "", Cases)),
      Tests   = as.numeric(gsub(",", "", Tests)),
      Country = "India"
    )) |>
    {\(x) data.table::setnames(x, names(x), janitor::make_clean_names(names(x)))}() |>
    {\(x) x[x[, .I[tests == max(tests)], by = "date"]$V1]}() |>
    data.table::DT(, .(date, place = country, total_tests = tests)) |>
    data.table::DT(order(date), `:=` (
      daily_tests = total_tests - data.table::shift(total_tests),
      ppt         = total_tests / (covid19india::pop[place == "India", population])
    )) |>
    data.table::DT(!is.na(date)) |>
    data.table::setkeyv(cols = c("place", "date")) |>
    data.table::setcolorder(c("place", "date", "daily_tests", "total_tests", "ppt")) |>
    data.table::DT()

  return(d)

}

