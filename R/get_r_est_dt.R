#' Helper function for pulling latest R estimates - data.table style
#' @param dat data set containing R estimates
#' @return Pulls 7-day trailing average R estimates and 95% confidence intervals
#' @import dplyr
#' @export
#' @examples
#' \dontrun{
#' get_all_data() %>% get_r_est()
#' }

get_r_est_dt <- function(x) {

  x[x[, .I[(.N - 6):.N], by = "place"]$V1][, .(
    r = mean(r_est, na.rm = TRUE),
    lower = mean(r_lower, na.rm = TRUE),
    upper = mean(r_upper, na.rm = TRUE)),
    .(place)]

}
