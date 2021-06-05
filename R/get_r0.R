#' Helper function for calculating R_0
#' @param dat Input dataset. Expects \code{daily_cases}, \code{total_cases}, and \code{place} columns
#' @param daily_filter Theshold for minimum daily cases. Default = 0.
#' @param total_filter Threshold for minimum total cases reported to date. Default = 50.
#' @param min_date Threshold for earliest date to report R_0. Default = "2020-03-23".
#' @return Pulls the time-series state-level testing data directly from covid19india.org.
#' @import dplyr
#' @importFrom janitor clean_names
#' @export
#' @examples
#' \dontrun{
#' # used in get_r0() function
#' }
#'

get_r0 <- function(
  dat,
  daily_filter = 0,
  total_filter = 50,
  min_date     = "2020-03-23"
  ) {

  message("getting r0...")

  tmp_dat <- dat %>%
    dplyr::filter(daily_cases > daily_filter & total_cases >= total_filter) %>%
    dplyr::group_by(place) %>%
    dplyr::mutate(
      ns = n()
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(ns >=7)

  options(warn = -1)
  tmp_est <- tmp_dat %>%
    dplyr::select(date, daily_cases, place) %>%
    tidyr::nest(data = c(-place)) %>%
    dplyr::mutate(
      covid19india::estR0 = purrr::map(data, ~estR0_out(dat = .x))
    ) %>%
    tidyr::unnest(estR0) %>%
    dplyr::select(-data) %>%
    dplyr::filter(date >= min_date) %>%
    tidyr::drop_na()
  options(warn = 1)

  return(tmp_est)
}
