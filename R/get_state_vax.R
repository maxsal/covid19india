#' Pull covid19india state-level vaccine data
#' @param path The URL path for the data. Default: https://api.covid19india.org/csv/latest/cowin_vaccine_data_statewise.csv
#' @param raw Pull raw unaltered data. Default is FALSE
#' @param keep_nat Keep national level data? Default is TRUE
#' @return Pulls the time-series state-level vaccine data directly from covid19india.org.
#' @import dplyr
#' @importFrom janitor clean_names
#' @export
#' @examples
#' \dontrun{
#' get_state_counts()
#' }

get_state_vax <- function(
  path       = "https://api.covid19india.org/csv/latest/cowin_vaccine_data_statewise.csv",
  raw        = FALSE,
  keep_nat   = TRUE
) {

  if (keep_nat == TRUE) {
    message("Also pulling national level data under `state = \"India\"`. Set `keep_nat == FALSE` to exclude by default")
  }

  d <- suppressWarnings(readr::read_csv(path,
                                        col_types = readr::cols()))

  if (raw == FALSE) {

    d <- d %>%
      janitor::clean_names() %>%
      dplyr::mutate(
        date = as.Date(updated_on, "%d/%m/%Y")
      ) %>%
      dplyr::select(
        date, state,
        total_vax_ppl = total_individuals_vaccinated,
        total_doses   = total_doses_administered,
        first_doses   = first_dose_administered,
        second_doses  = second_dose_administered
      ) %>%
      dplyr::group_by(state) %>%
      dplyr::mutate(
        daily_doses = total_doses - dplyr::lag(total_doses)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(pop %>% dplyr::select(-abbrev), by = c("state" = "place")) %>%
      dplyr::mutate(
        pct_one_doses = round(total_vax_ppl * 100 / population, 4),
        pct_two_doses = round(second_doses * 100/ population, 4)
      ) %>%
      dplyr::select(-population)

  }

  if (keep_nat == FALSE & raw == FALSE) {
    d <- d %>%
      dplyr::filter(state != "India")
  }

  return(d)

}
