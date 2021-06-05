#' Helper function for calculating R_0
#' @param dat Input dataset
#' @return Pulls the time-series state-level testing data directly from covid19india.org. Expects a \code{daily_cases} column
#' @import dplyr
#' @importFrom janitor clean_names
#' @export
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
    config = make_config(list(
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
    dplyr::select(-date_num) %>%
    dplyr::select(date, tidyselect::everything())

}
