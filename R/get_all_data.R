#' Pull all covid19india count, test, and vaccine data for states and nation
#' @param keep_nat Keep the national data as well. Default is `FALSE`
#' @param covind19_name_scheme Variable naming scheme used for development of [`covind19.org`](https://umich-biostatistics.shinyapps.io/covid19/) application
#' @param corr_check Check for data corrections of X-times magnitude. Default is `TRUE`
#' @param useDT Use data.table backend rather than tidyverse. Default is `FALSE`
#' @return Pulls the district-level time-series case, death, and recovered data directly from [`covid19india.org`](https://www.covid19india.org).
#' @import dplyr
#' @import tidyr
#' @import data.table
#' @importFrom cli cli_alert_warning
#' @importFrom janitor clean_names
#' @export
#' @examples
#' \dontrun{
#' get_all_data()
#' }

get_all_data <- function(
  keep_nat             = TRUE,
  covind19_name_scheme = FALSE,
  corr_check           = TRUE,
  useDT                = FALSE
) {

  if (useDT == FALSE) {

  d <- dplyr::bind_rows(
    covid19india::get_nat_counts(useDT = useDT),
    covid19india::get_state_counts(useDT = useDT)
    ) %>%
    dplyr::left_join(
      dplyr::bind_rows(
        covid19india::get_nat_tests(useDT = useDT),
        covid19india::get_state_tests(useDT = useDT)
      ),
      by = c("place", "date")) %>%
    dplyr::left_join(
      base::suppressMessages(get_r0(., corr_check = corr_check)) %>%
        dplyr::rename(
          r_est   = r,
          r_lower = lower,
          r_upper = upper
        ),
      by = c("place", "date")) %>%
    dplyr::left_join(
      covid19india::pop %>%
        dplyr::distinct(place, .keep_all = TRUE) %>%
        dplyr::select(-population),
      by = "place") %>%
    dplyr::mutate(tpr = daily_cases / daily_tests) %>%
    dplyr::left_join(
      base::suppressMessages(covid19india::get_state_vax()),
        by = c("place", "date")
    )

  if (keep_nat == FALSE) {

    d %>% dplyr::filter(place != "India")

  }

  if (covind19_name_scheme == TRUE) {

    cli::cli_alert_warning("{covid19india} variable naming scheme is encouraged (`covind19_naming_scheme == FALSE`)")

    d <- d %>%
      dplyr::rename(
        cases            = daily_cases,
        recovered        = daily_recovered,
        deaths           = daily_deaths,
        total_vacc       = total_doses,
        pct_at_least_one = pct_one_doses,
        pct_second       = pct_two_doses,
        daily_vax_dose   = daily_doses
      )

  }


  } else {

    d <- data.table::merge.data.table(
    data.table::rbindlist(list(
      covid19india::get_nat_counts(useDT = TRUE),
      covid19india::get_state_counts(useDT = TRUE)
    )),
    data.table::rbindlist(list(
      covid19india::get_nat_tests(useDT = TRUE),
      covid19india::get_state_tests(useDT = TRUE)
    )),
    by = c("place", "date"),
    all.x = TRUE
    )

    d <- data.table::merge.data.table(
      d,
      setDT(get_r0(dat = d, corr_check = corr_check))[, .(place, date, r_est = r, r_lower = lower, r_upper = upper)],
      by    = c("place", "date"),
      all.x = TRUE
      )

    d <- data.table::merge.data.table(
      d,
      unique(covid19india::pop[, .(place, abbrev)][, head(.SD, 1), by = "place"]),
      by    = "place",
      all.x = TRUE
    )

    d <- data.table::merge.data.table(
      d[, tpr := daily_cases / daily_tests],
      base::suppressMessages(covid19india::get_state_vax()),
      by = c("place", "date"),
      all.x = TRUE
    )

    if (keep_nat == FALSE) {

      d <- d[place != "India"]

    }

    if (covind19_name_scheme == TRUE) {

      cli::cli_alert_warning("{covid19india} variable naming scheme is encouraged (`covind19_naming_scheme == FALSE`)")

      setnames(d,
               old = c("daily_cases", "daily_recovered", "daily_deaths", "total_doses", "pct_one_doses",
                       "pct_two_doses", "daily_doses"),
               new = c("cases", "recovered", "deaths", "total_vacc", "pct_at_least_one", "pct_second", "daily_vax_dose"))
    }

  }

  return(d)

}
