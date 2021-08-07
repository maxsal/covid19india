#' Pull covid19india state-level vaccine data
#' @param path The URL path for the data. Default: https://api.covid19india.org/csv/latest/vaccine_doses_statewise_v2.csv
#' @param raw Pull raw unaltered data. Default is FALSE
#' @param keep_nat Keep national level data? Default is TRUE
#' @param useDT Use data.table backend rather than tidyverse. Default is `FALSE`
#' @return Pulls the time-series state-level vaccine data directly from covid19india.org.
#' @import dplyr
#' @import data.table
#' @importFrom cli cli_alert_info
#' @importFrom janitor clean_names
#' @importFrom readr read_csv
#' @export
#' @examples
#' \dontrun{
#' get_state_counts()
#' }

get_state_vax <- function(
  path       = "https://api.covid19india.org/csv/latest/vaccine_doses_statewise_v2.csv",
  raw        = FALSE,
  keep_nat   = TRUE,
  useDT      = FALSE
) {

  if (keep_nat == TRUE) {
    cli::cli_alert_info("Also pulling national level data under `place = \"India\"`. Set `keep_nat == FALSE` to exclude by default")
  }

  if (useDT == FALSE) {
  d <- suppressWarnings(readr::read_csv(path,
                                        col_types = readr::cols()))

  if (raw == FALSE) {

    d <- d %>%
      janitor::clean_names() %>%
      dplyr::mutate(
        date = as.Date(vaccinated_as_of, "%d/%m/%Y")
      ) %>%
      dplyr::select(place = state, date,
                    first_dose = first_dose_administered,
                    second_dose = second_dose_administered,
                    total_doses = total_doses_administered) %>%
      dplyr::mutate(
        place = dplyr::case_when(place == "Total" ~ "India", T ~ place)
      ) %>%
      dplyr::left_join(covid19india::pop %>%
                         dplyr::select(-abbrev) %>%
                         distinct(),
                       by = "place") %>%
      dplyr::mutate(
        pct_one_doses = round(first_dose * 100 / population, 4),
        pct_two_doses = round(second_dose * 100/ population, 4)
      ) %>%
      dplyr::select(-population) %>%
      dplyr::group_by(place) %>%
      dplyr::arrange(date) %>%
      dplyr::mutate(
        daily_doses = total_doses - dplyr::lag(total_doses)
      ) %>%
      ungroup()

  }

  } else {

    d <- data.table::fread(path, showProgress = FALSE)

    if (raw == FALSE) {

      data.table::setnames(d, names(d), janitor::make_clean_names(names(d)))

      d <- d[, date := as.Date(vaccinated_as_of, "%d/%m/%Y")][, .(place = state, date,
                                                             first_dose = first_dose_administered,
                                                             second_dose = second_dose_administered,
                                                             total_doses = total_doses_administered)][
                                                               place == "Total", place := "India"
                                                             ]

      d <- data.table::merge.data.table(
        d,
        unique(covid19india::pop[, !c("abbrev")]),
        by = "place",
        all.x = "TRUE"
      )[, `:=` (pct_one_doses = round(first_dose * 100 / population, 4),
                pct_two_doses = round(second_dose * 100/ population, 4))][, !c("population")][
                  order(date), daily_doses := total_doses - data.table::shift(total_doses), by = "place"
                ][]

    }

  }

  if (keep_nat == FALSE & raw == FALSE) {
    d <- d %>%
      dplyr::filter(place != "India")
  }

  return(d)

}
