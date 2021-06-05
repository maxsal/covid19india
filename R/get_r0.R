#' Helper function for calculating R_0
#' @param dat Input dataset
#' @return Pulls the time-series state-level testing data directly from covid19india.org. Expects a \code{daily_cases} column
#' @keywords internal
#' @import dplyr
#' @import EpiEstim
#' @importFrom janitor clean_names
#' @examples
#' \dontrun{
#' # used in get_r0() function
#' }
#'

estR0_out <- function(dat) {

  t_start <- seq(2, nrow(dat) - 4)
  t_end   <- t_start + 4

  res <- EpiEstim::estimate_R(
    incid = dat$daily_cases,
    method = "parametric_si",
    config = EpiEstim::make_config(list(
      mean_si             = 7,
      std_si              = 4.5,
      si_parametric_distr = "G",
      t_start             = t_start,
      t_end               = t_end,
      seed                = 46342))
  )

  tibble::tibble(
    date_num = res$dates
  ) %>% dplyr::left_join(
    res$R, by = c("date_num" = "t_end")
  ) %>%
    dplyr::select(
      date_num, t_start, r = `Mean(R)`, lower = `Quantile.0.025(R)`, upper = `Quantile.0.975(R)`
    ) %>%
    tibble::add_column(date = dat$date) %>%
    dplyr::select(-c(date_num, t_start)) %>%
    dplyr::select(date, tidyselect::everything())

}

#' Calculate r0
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
      estR0 = purrr::map(data, ~covid19india:::estR0_out(dat = .x))
    ) %>%
    tidyr::unnest(estR0) %>%
    dplyr::select(-data) %>%
    dplyr::filter(date >= min_date) %>%
    tidyr::drop_na()
  options(warn = 1)

  return(tmp_est)
}
