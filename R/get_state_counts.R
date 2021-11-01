#' Pull covid19india state
#' @param path The URL path for the data. Default: `https://api.covid19india.org/csv/latest/state_wise_daily.csv`
#' @param raw Pull raw unaltered data. Default is `FALSE`
#' @param keep_nat Keep the national data as well. Default is `FALSE`
#' @param corr_check Check for data correction. Default is `FALSE`
#' @param mohfw switch to mohfw default is `TRUE`
#' @return Pulls the time-series case, death, and recovered data directly from covid19india.org.
#' @import data.table
#' @importFrom cli cli_alert_info
#' @importFrom janitor clean_names
#' @export
#' @examples
#' \dontrun{
#' get_state_counts()
#' }

get_state_counts <- function(
  path       = "https://api.covid19india.org/csv/latest/state_wise_daily.csv",
  raw        = FALSE,
  keep_nat   = FALSE,
  corr_check = FALSE,
  mohfw      = TRUE
) {

  if (mohfw == FALSE) {
    d <- data.table::fread(path, showProgress = FALSE)

    if (raw == FALSE) {

      d <- d[, !c("Date")][, DH := DD + DN][, !c("DD", "DN")]
      setnames(d, names(d), janitor::make_clean_names(names(d)))
      setnames(d, "date_ymd", "date")

      d <- data.table::melt(d, id.vars = c("date", "status"), variable.name = "abbrev")
      d <- data.table::dcast(d, date + abbrev ~ status)
      d <- d[abbrev != "un"]

      setnames(d,
               c("Confirmed", "Deceased", "Recovered"),
               c("daily_cases", "daily_deaths", "daily_recovered"))

      d <- d[daily_cases >= 0][order(date),
                               `:=` (
                                 total_cases     = cumsum(daily_cases),
                                 total_deaths    = cumsum(daily_deaths),
                                 total_recovered = cumsum(daily_recovered)
                               ),
                               by = abbrev][,
                                            date := as.Date(date)]

      d <- data.table::merge.data.table(d, covid19india::pop[, !c("population")], by = "abbrev", all.x = TRUE)[, !c("abbrev")]

      suppressWarnings({ d <- d[!(place %in% c("Haryana***", "Kerala***"))] })

      setkeyv(d, cols = c("place", "date"))
      setcolorder(d, neworder = c("place", "date", "daily_cases", "daily_recovered", "daily_deaths", "total_cases", "total_recovered", "total_deaths"))

    }

    if (keep_nat == FALSE) {
      if (raw == FALSE) {
        d <- d[place != "India"]
      }
      if (raw == TRUE) {
        d <- d[, !c("TT")]
      }

    }

    if (corr_check == TRUE) {

      if (raw == TRUE) {

        stop("`raw` must be FALSE to use `corr_check = TRUE` argument")

      } else {

        d <- data.table::rbindlist(
          lapply(d[, unique(place)],
                 function(x) covid19india::check_for_data_correction(d[place == x]))
        )

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

    d <- d[order(date)
      , `:=` (
        daily_cases     = total_cases - shift(total_cases),
        daily_deaths    = total_deaths - shift(total_deaths),
        daily_recovered = total_recovered - shift(total_recovered)
      ), by = place
    ][]

    d <- d[date >= "2021-10-10" & daily_cases < 0, daily_cases := 0]
    d <- d[date >= "2021-10-10" & daily_deaths < 0, daily_deaths := 0]
    d <- d[date >= "2021-10-10" & daily_recovered < 0, daily_recovered := 0]

    suppressWarnings({ d <- d[place == "Puducherry", place := "Pondicherry"]})

    d <- d[place == "Kerala***", place := "Kerala"][]

    setcolorder(d,
                neworder = c("place", "date", "daily_cases", "daily_recovered", "daily_deaths", "total_cases", "total_recovered", "total_deaths"))

    setkeyv(d, cols = c("place", "date"))

  }

  cli::cli_alert_info(paste0("Data through ", d[, max(date)]))

  return(d)

}
