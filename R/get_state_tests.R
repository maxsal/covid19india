#' Pull covid19india state-level testing data
#' @param path The URL path for the data. Default: https://api.covid19india.org/csv/latest/statewise_tested_numbers_data.csv
#' @param raw Pull raw unaltered data. Default is FALSE
#' @return Pulls the time-series state-level testing data directly from covid19india.org.
#' @import data.table
#' @importFrom janitor clean_names
#' @export
#' @examples
#' \dontrun{
#' get_state_tests()
#' }

get_state_tests <- function(
  path       = "https://api.covid19india.org/csv/latest/statewise_tested_numbers_data.csv",
  raw        = FALSE
) {

    d <- data.table::fread(path, showProgress = FALSE)

    if (raw == FALSE) {

      d <- d |>
        {\(x) data.table::setnames(x, names(x), janitor::make_clean_names(names(x)))}() |>
        data.table::DT(, date := as.Date(updated_on, "%d/%m/%Y")) |>
        data.table::DT(, .(date, place = state, total_tests = total_tested)) |>
        unique() |>
        data.table::DT(order(date), daily_tests := total_tests - data.table::shift(total_tests), by = place) |>
        data.table::DT(, .(place, date, daily_tests, total_tests)) |>
        data.table::merge.data.table(unique(covid19india::pop[, !c("abbrev")]), all.x = TRUE, by = "place") |>
        data.table::DT(, ppt := total_tests / population) |>
        data.table::DT(, !c("population")) |>
        data.table::setkeyv(cols = c("place", "date")) |>
        data.table::setcolorder(c("place", "date", "daily_tests", "total_tests", "ppt"))

    }

  return(d)

}
