#' Pull covid19india national time series data
#' @param path The URL path for the data. Default: https://api.covid19india.org/csv/latest/case_time_series.csv
#' @param raw Pull raw unaltered data. Default is `FALSE`
#' @param corr_check Check for data correction. Default is `FALSE`
#' @param mohfw switch to mohfw default is `TRUE`
#' @return Pulls the time-series case, death, and recovered data directly from covid19india.org.
#' @import data.table
#' @importFrom cli cli_alert_info
#' @importFrom janitor clean_names
#' @export
#' @examples
#' \dontrun{
#' get_nat_counts_dt()
#' }
#'

get_nat_counts <- function(
  path       = "https://api.covid19india.org/csv/latest/case_time_series.csv",
  raw        = FALSE,
  corr_check = TRUE,
  mohfw      = TRUE
) {

  if (mohfw == FALSE) {

    d <- data.table::fread(path, showProgress = FALSE)

    if (raw == FALSE) {

      d <- d[, !c("Date")]
      setnames(d, names(d), janitor::make_clean_names(names(d)))
      setnames(d,
               c("date_ymd", "daily_confirmed", "total_confirmed", "daily_deceased", "total_deceased"),
               c("date", "daily_cases", "total_cases", "daily_deaths", "total_deaths"))

      d <- d[, `:=` (place = "India", date = as.Date(date))]

      setcolorder(d,
                  neworder = c("place", "date", "daily_cases", "daily_recovered", "daily_deaths", "total_cases", "total_recovered", "total_deaths"))

      setkeyv(d, cols = c("place", "date"))

    }

    if (corr_check == TRUE) {

      if (raw == TRUE) {
        stop("`raw` must be FALSE to use `corr_check = TRUE` argument")
      } else {
        d <- covid19india::check_for_data_correction(dat = d, var = "daily_cases")
      }
    }

  }

  if (mohfw == TRUE) {

    d <- fread("https://raw.githubusercontent.com/umich-cphds/cov-ind-19-data/master/source_data/source_data_latest.csv",
               showProgress = FALSE, fill = TRUE)

    setnames(d,
             old = c("State", "Date", "Cases", "Recovered", "Deaths", "Active"),
             new = c("place", "date", "total_cases", "total_recovered", "total_deaths", "total_active"))

    d <- d[, date := as.Date(date, "%d/%m/%Y")]

    d <- d[, lapply(.SD, sum, na.rm = TRUE), by = date, .SDcols = c("total_cases", "total_deaths", "total_recovered")][
      , place := "India"
      ][
        , `:=` (
          daily_cases     = total_cases - shift(total_cases),
          daily_deaths    = total_deaths - shift(total_deaths),
          daily_recovered = total_recovered - shift(total_recovered)
        )
      ][]

    # d <- d[daily_cases < 0, daily_cases := NA][daily_deaths < 0, daily_deaths := NA][daily_recovered < 0, daily_recovered := NA][order(date), c("daily_cases", "daily_deaths", "daily_recovered") := lapply(.SD, nafill, type = "locf"), .SDcols = c("daily_cases", "daily_deaths", "daily_recovered")][]
    # d <- covid19india::check_for_data_correction(dat = d, var = "daily_cases")
    # d <- covid19india::check_for_data_correction(dat = d, var = "daily_deaths")
    # d <- covid19india::check_for_data_correction(dat = d, var = "daily_recovered")
    #
    # d <- d[daily_cases < 0, daily_cases := NA][daily_deaths < 0 , daily_deaths := NA][daily_recovered < 0, daily_recovered := NA][order(date), c("daily_cases", "daily_deaths", "daily_recovered") := lapply(.SD, nafill, type = "locf"), .SDcols = c("daily_cases", "daily_deaths", "daily_recovered")][]

    d <- d[date >= "2021-10-10" & daily_cases < 0, daily_cases := 0]
    d <- d[date >= "2021-10-10" & daily_deaths < 0, daily_deaths := 0]
    d <- d[date >= "2021-10-10" & daily_recovered < 0, daily_recovered := 0]

    setcolorder(d,
                neworder = c("place", "date", "daily_cases", "daily_recovered", "daily_deaths", "total_cases", "total_recovered", "total_deaths"))

    setkeyv(d, cols = c("place", "date"))

  }

  cli::cli_alert_info(paste0("Data through ", d[, max(date)]))

  return(d)

}
