#' Pull covid19india state-level testing data
#' @param path The URL path for the data. Default: https://api.covid19india.org/csv/latest/statewise_tested_numbers_data.csv
#' @param raw Pull raw unaltered data. Default is FALSE
#' @param useDT Use data.table backend rather than tidyverse. Default is `FALSE`
#' @return Pulls the time-series state-level testing data directly from covid19india.org.
#' @import dplyr
#' @import data.table
#' @importFrom janitor clean_names
#' @importFrom readr read_csv
#' @export
#' @examples
#' \dontrun{
#' get_state_counts()
#' }

get_state_tests <- function(
  path       = "https://api.covid19india.org/csv/latest/statewise_tested_numbers_data.csv",
  raw        = FALSE,
  useDT      = FALSE
) {

  if (useDT == FALSE) {
  d <- suppressWarnings(readr::read_csv(path,
                                        col_types = readr::cols()))

  if (raw == FALSE) {

    d <- d %>%
      janitor::clean_names() %>%
      dplyr::mutate(
        date = as.Date(updated_on, "%d/%m/%Y")
      ) %>%
      dplyr::select(date, place = state, total_tests = total_tested) %>%
      distinct() %>%
      dplyr::group_by(place) %>%
      dplyr::arrange(date) %>%
      dplyr::mutate(
        daily_tests = total_tests - dplyr::lag(total_tests)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(place, date, daily_tests, total_tests) %>%
      dplyr::left_join(covid19india::pop %>%
                         dplyr::select(-abbrev) %>%
                         dplyr::distinct(),
                       by = "place") %>%
      dplyr::mutate(ppt = total_tests / population) %>%
      dplyr::select(-population)

  }

  } else {

    d <- data.table::fread(path, showProgress = FALSE)

    if (raw == FALSE) {

      data.table::setnames(d, names(d), janitor::make_clean_names(names(d)))

      d <- unique(d[, date := as.Date(updated_on, "%d/%m/%Y")][, .(date, place = state, total_tests = total_tested)])

      d[order(date), daily_tests := total_tests - data.table::shift(total_tests), by = place][, .(place, date, daily_tests, total_tests)]

      d <- data.table::merge.data.table(d, unique(covid19india::pop[, !c("abbrev")]), all.x = TRUE, by = "place")[, ppt := total_tests / population][, !c("population")]

      data.table::setkeyv(d, cols = c("place", "date"))
      data.table::setcolorder(d, c("place", "date", "daily_tests", "total_tests", "ppt"))

    }

  }

  return(d)

}
