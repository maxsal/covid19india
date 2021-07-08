#' Pull all covid19india count, test, and vaccine data for states and nation
#' @param keep_nat Keep the national data as well. Default is `FALSE`
#' @return Pulls the district-level time-series case, death, and recovered data directly from covid19india.org.
#' @import dplyr
#' @import tidyr
#' @importFrom janitor clean_names
#' @export
#' @examples
#' \dontrun{
#' get_all_data()
#' }

get_all_data <- function(
  keep_nat = TRUE
) {

  d <- dplyr::bind_rows(
    covid19india::get_nat_counts(),
    covid19india::get_state_counts()
    ) %>%
    dplyr::full_join(
      dplyr::bind_rows(
        covid19india::get_nat_tests(),
        covid19india::get_state_tests()
      ),
      by = c("place", "date")) %>%
    dplyr::left_join(
      base::suppressMessages(get_r0(.)) %>%
        dplyr::rename(
          r_est   = r,
          r_lower = lower,
          r_upper = upper
        ),
      by = c("place", "date")) %>%
    dplyr::left_join(
      pop %>%
        dplyr::distinct(place, .keep_all = TRUE) %>%
        dplyr::select(-population),
      by = "place") %>%
    dplyr::mutate(tpr = daily_cases / daily_tests) %>%
    dplyr::left_join(
      base::suppressMessages(covid19india::get_state_vax()),
        by = c("place", "date")
    )

  if (keep_nat == FALSE) {

    d %>% filter(place != "India")

  }

  return(d)

}
