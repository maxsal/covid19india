#' Pull covid19india national time series data
#' @param path The URL path for the data. Default: https://api.covid19india.org/csv/latest/case_time_series.csv
#' @param raw Pull raw unaltered data. Default is `FALSE`
#' @param corr_check Check for data correction. Default is `FALSE`
#' @param useDT Use data.table backend rather than tidyverse. Default is `TRUE`
#' @return Pulls the time-series case, death, and recovered data directly from covid19india.org.
#' @import dplyr
#' @importFrom data.table fread
#' @importFrom data.table setnames
#' @importFrom data.table setkeyv
#' @importFrom data.table setcolorder
#' @importFrom readr read_csv
#' @importFrom janitor clean_names
#' @export
#' @examples
#' \dontrun{
#' get_nat_counts()
#' }
#'

get_nat_counts <- function(
  path       = "https://api.covid19india.org/csv/latest/case_time_series.csv",
  raw        = FALSE,
  corr_check = FALSE,
  useDT      = TRUE
) {

  if (useDT == FALSE) {
  d <- readr::read_csv(path,
                       col_types = readr::cols())

  if (raw == FALSE) {

    d <- d %>%
      janitor::clean_names() %>%
      dplyr::select(-date) %>%
      dplyr::rename(
        date         = date_ymd,
        daily_cases  = daily_confirmed,
        total_cases  = total_confirmed,
        daily_deaths = daily_deceased,
        total_deaths = total_deceased
        ) %>%
      dplyr::mutate(
        place = "India"
      ) %>%
      dplyr::select(place, date, daily_cases, daily_recovered, daily_deaths, everything())

  }

  if (corr_check == TRUE) {

    if (raw == TRUE) {
      stop("`raw` must be FALSE to use `corr_check = TRUE` argument")
    } else {

    d <- d %>%
      covid19india::check_for_data_correction(var = "daily_cases")

    }
  }

  } else {

    d <- data.table::fread(path, showProgress = FALSE)

    if (raw == FALSE) {

    d[, Date := NULL]

    data.table::setnames(d, names(d), janitor::make_clean_names(names(d)))
    data.table::setnames(d,
                         c("date_ymd", "daily_confirmed", "total_confirmed", "daily_deceased", "total_deceased"),
                         c("date", "daily_cases", "total_cases", "daily_deaths", "total_deaths"))

    d[, place := "India"][]

    data.table::setcolorder(d, neworder = c("place", "date", "daily_cases", "daily_recovered", "daily_deaths"))
    data.table::setkeyv(d, cols = c("place", "date"))

    }

    if (corr_check == TRUE) {

      if (raw == TRUE) {
        stop("`raw` must be FALSE to use `corr_check = TRUE` argument")
      } else {
        d <- covid19india::check_for_data_correction(dat = d, var = "daily_cases")
      }
    }

  }

  return(d)

}
