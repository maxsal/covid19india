#' Check for data corrections of X-times magnitude
#' @param dat data set
#' @param var variable for which to check for corrections
#' @param magnitude magnitude of difference that qualifies as a data correction. Default is 10.
#' @return Data set with data correction observations removed
#' @import dplyr
#' @importFrom cli cli_alert_info
#' @export
#' @examples
#' \dontrun{
#' get_nat_counts() %>% check_for_data_correction(var = "daily_cases", magnitude = 10)
#' }

check_for_data_correction <- function(dat, var, magnitude = 10) {

  v <- dat %>% dplyr::pull(var)

  times <- v / dplyr::lag(v)

  times_loc <- which(times >= magnitude)

  if(length(times_loc) == 0) {
    cli::cli_alert_info("no data corrections of magnitude {magnitude} detected. returning full dataset")
    dat
  } else {
    cli::cli_alert_info("{length(times_loc)} data corrections of magnitude {magnitude} detected. {length(times_loc)} observations removed")
  dat %>%
    dplyr::slice(-times_loc)
  }

}
