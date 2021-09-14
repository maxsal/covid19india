#' Pull covid19india state
#' @param path The URL path for the data. Default: `https://api.covid19india.org/csv/latest/state_wise_daily.csv`
#' @param raw Pull raw unaltered data. Default is `FALSE`
#' @param keep_nat Keep the national data as well. Default is `FALSE`
#' @param corr_check Check for data correction. Default is `FALSE`
#' @return Pulls the time-series case, death, and recovered data directly from covid19india.org.
#' @import data.table
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
  corr_check = FALSE
) {

    d <- data.table::fread(path, showProgress = FALSE)

    if (raw == FALSE) {

      d <- d |>
        data.table::DT(, `:=` (Date = NULL, DH = DD + DN)) |>
        data.table::DT(, `:=` (DD = NULL, DN = NULL)) |>
        {\(x) data.table::setnames(x, names(x), janitor::make_clean_names(names(x)))}() |>
        data.table::setnames("date_ymd", "date") |>
        data.table::melt(id.vars = c("date", "status"), variable.name = "abbrev") |>
        data.table::dcast(date + abbrev ~ status) |>
        data.table::DT(abbrev != "un") |>
        data.table::setnames(
          c("Confirmed", "Deceased", "Recovered"),
          c("daily_cases", "daily_deaths", "daily_recovered")
          ) |>
        data.table::DT(daily_cases >= 0) |>
        data.table::DT(order(date), `:=` (
          total_cases     = cumsum(daily_cases),
          total_deaths    = cumsum(daily_deaths),
          total_recovered = cumsum(daily_recovered)
        ),
        by = abbrev) |>
        data.table::DT(, date := as.Date(date)) |>
        data.table::merge.data.table(as.data.table(covid19india::pop)[, !c("population")], by = "abbrev", all.x = TRUE) |>
        data.table::DT(, !c("abbrev")) |>
        data.table::setkeyv(cols = c("place", "date")) |>
        data.table::setcolorder(neworder = c("place", "date", "daily_cases", "daily_recovered", "daily_deaths", "total_cases", "total_recovered", "total_deaths")) |>
        data.table::DT()

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
               \(x) covid19india::check_for_data_correction(d[place == x]))
      )

    }

  }

  return(d)

}
