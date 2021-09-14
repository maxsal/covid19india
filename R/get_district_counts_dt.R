#' Pull covid19india district-level data - data.table style
#' @param path The URL path for the data. Default: `https://api.covid19india.org/csv/latest/districts.csv`
#' @param raw Pull raw unaltered data. Default is `FALSE`
#' @return Pulls the district-level time-series case, death, and recovered data directly from covid19india.org.
#' @import data.table
#' @importFrom janitor clean_names
#' @export
#' @examples
#' \dontrun{
#' get_district_counts()
#' }

get_district_counts_dt <- function(
  path       = "https://api.covid19india.org/csv/latest/districts.csv",
  raw        = FALSE
) {

  d <- data.table::fread(path, showProgress = FALSE)

  if (raw == FALSE) {

    d <- d |>
      {\(x) setnames(x, names(x), janitor::make_clean_names(names(x)))}() |>
      {\(x) setnames(x, c("confirmed", "recovered", "deceased"), c("total_cases", "total_recovered", "total_deaths"))}() |>
      data.table::DT(, !c("other")) |>
      data.table::melt(id.vars = c("date", "state", "district")) |>
      data.table::DT(value >= 0) |>
      data.table::dcast.data.table(date + state + district ~ variable) |>
      data.table::DT(order(date)) |>
      data.table::DT(, `:=` (
        daily_cases     = total_cases - data.table::shift(total_cases),
        daily_recovered = total_recovered - data.table::shift(total_recovered),
        daily_deaths    = total_deaths - data.table::shift(total_deaths)
      ), by = c("state", "district")) |>
      data.table::DT(!is.na(daily_cases) & !is.na(daily_recovered) & !is.na(daily_deaths)) |>
      data.table::DT(, .(state, district, date,
                         daily_cases, daily_recovered, daily_deaths,
                         total_cases, total_recovered, total_deaths))

  }

  return(d)

}
