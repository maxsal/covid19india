#' Helper function for calculating R_0 - data.table style
#' @param x Input dataset
#' @param incubation_days Number of days from infection to symptoms
#' @return Pulls the time-series state-level testing data directly from covid19india.org.
#' @keywords internal
#' @import data.table
#' @import EpiEstim
#' @importFrom janitor clean_names
#' @examples
#' \dontrun{
#' estR0_out_dt(x = get_nat_counts())
#' }
#'

estR0_out <- function(x, incubation_days = 3) {

  t_start   <- seq(2, nrow(x) - 4)
  t_end     <- t_start + 4

  res <- EpiEstim::estimate_R(
    incid = x$daily_cases,
    method = "parametric_si",
    config = EpiEstim::make_config(list(
      mean_si             = incubation_days,
      std_si              = 4.5,
      si_parametric_distr = "G",
      t_start             = t_start,
      t_end               = t_end,
      seed                = 46342))
  )

  tmp_out <- merge.data.table(data.table(date_num = res$dates), res$R, by.x = "date_num", by.y = "t_end", all.x = TRUE)[
    , .(r = `Mean(R)`, lower = `Quantile.0.025(R)`, upper = `Quantile.0.975(R)`)][
      , `:=` (date = x$date, place = x$place)]
  setcolorder(tmp_out, "date")

  return(tmp_out[])

}

#' Calculate r0
#' @param dat Input dataset. Expects \code{daily_cases}, \code{total_cases}, and \code{place} columns
#' @param daily_filter Threshold for minimum daily cases. Default = `0`.
#' @param total_filter Threshold for minimum total cases reported to date. Default = `50`.
#' @param min_date Threshold for earliest date to report R_0. Default = `"2020-03-23"`.
#' @param corr_check Check for data corrections of X-times magnitude. Default is `FALSE`
#' @param inc_days Number of days from infection to symptoms
#' @return Pulls the time-series state-level testing data directly from covid19india.org. Expects columns named `place`, `daily_cases`, and `total_cases`. Can specify corresponding variables through other arguments.
#' @import data.table
#' @importFrom janitor clean_names
#' @importFrom stats na.omit
#' @export
#' @examples
#' \dontrun{
#' get_r0(dat = get_nat_counts())
#' }
#'

get_r0 <- function(
  dat,
  daily_filter = 0,
  total_filter = 50,
  min_date     = "2020-03-23",
  corr_check   = FALSE,
  inc_days     = c(7, 5, 3)
  ) {

  if(!is.null(corr_check) & is.logical(corr_check)) {

    if(corr_check == TRUE) {

      tmp_dat <- dat[daily_cases > daily_filter & total_cases >= total_filter][, ns := .N, by = "place"][ns >= 7]

      tmp_dat <- rbindlist(
          lapply(tmp_dat[, unique(place)], function(x) covid19india::check_for_data_correction(tmp_dat[place == x]))
        )

    } else {

      tmp_dat <- dat[daily_cases > daily_filter & total_cases >= total_filter][, ns := .N, by = "place"][ns >= 7]

    }

  } else {

    stop("corr_check must be TRUE/FALSE or set to NULL.")

  }

  # 2020
  suppressWarnings(
    t2020 <- stats::na.omit(
      data.table::rbindlist(
        lapply(tmp_dat[date <= "2020-12-31"][, unique(place)],
               function(x) estR0_out(tmp_dat[date <= "2020-12-31"][place == x], incubation_days = inc_days[1]))
      )[date >= min_date]
    )
  )

  # 2021
  suppressWarnings(
    t2021 <- stats::na.omit(
      data.table::rbindlist(
        lapply(tmp_dat[date <= as.Date("2021-12-31")][, unique(place)],
               function(x) estR0_out(tmp_dat[date <= as.Date("2021-12-31")][place == x], incubation_days = inc_days[2]))
      )[date >= "2021-01-01"]
    )
  )

  # 2022
  suppressWarnings(
    t2022 <- stats::na.omit(
    data.table::rbindlist(
      lapply(tmp_dat[, unique(place)],
             function(x) estR0_out(tmp_dat[place == x], incubation_days = inc_days[3]))
      )[date >= "2022-01-01"]
    )
    )

  return(
    rbindlist(
      list(
        t2020, t2021, t2022
      )
    )
  )

}
