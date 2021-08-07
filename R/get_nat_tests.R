#' Pull covid19india national time series test data
#' @param path The URL path for the data. Default: `https://api.covid19india.org/data.json`
#' @param raw Pull raw unaltered data. Default is `FALSE`
#' @param useDT Use data.table backend rather than tidyverse. Default is `FALSE`
#' @return Pulls the time-series test data directly from covid19india.org.
#' @import dplyr
#' @import httr
#' @import data.table
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
  raw        = FALSE,
  useDT      = FALSE
) {

  request  <- httr::GET(path)
  json     <- httr::content(request)
  d        <- purrr::map_dfr(json[['tested']], ~ .x)

  if (useDT == FALSE) {
    if (raw == FALSE) {

      d <- d %>%
        dplyr::select(
          Cases = totalpositivecases,
          Tests = totalsamplestested,
          Date  = testedasof
        ) %>%
        dplyr::mutate(
          Date    = as.Date(stringr::word(Date, 1), format = "%d/%m/%Y"),
          Cases   = as.numeric(gsub(",", "", Cases)),
          Tests   = as.numeric(gsub(",", "", Tests)),
          Country = "India"
        ) %>%
        janitor::clean_names() %>%
        dplyr::select(date, place = country, total_tests = tests) %>%
        dplyr::group_by(date) %>%
        dplyr::filter(total_tests == max(total_tests)) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(date) %>%
        dplyr::mutate(
          daily_tests = total_tests - dplyr::lag(total_tests),
          ppt         = total_tests / (covid19india::pop %>% dplyr::filter(place == "India") %>% dplyr::pull(population))
        )

    }
  } else {

    d <- setDT(d)[, .(
      Cases = totalpositivecases,
      Tests = totalsamplestested,
      Date  = testedasof
      )][, `:=` (
        Date    = as.Date(stringr::word(Date, 1), format = "%d/%m/%Y"),
        Cases   = as.numeric(gsub(",", "", Cases)),
        Tests   = as.numeric(gsub(",", "", Tests)),
        Country = "India"
        )]

    data.table::setnames(d, names(d), janitor::make_clean_names(names(d)))

    d <- d[d[, .I[tests == max(tests)], by = "date"]$V1][, .(date, place = country, total_tests = tests)][order(date), `:=` (
      daily_tests = total_tests - data.table::shift(total_tests),
      ppt         = total_tests / (covid19india::pop[place == "India", population])
    )][]

    data.table::setcolorder(d, c("place", "date", "daily_tests", "total_tests", "ppt"))

  }

  return(d)

}

