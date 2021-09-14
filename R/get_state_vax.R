#' Pull covid19india state-level vaccine data
#' @param path The URL path for the data. Default: https://api.covid19india.org/csv/latest/vaccine_doses_statewise_v2.csv
#' @param raw Pull raw unaltered data. Default is FALSE
#' @param keep_nat Keep national level data? Default is TRUE
#' @return Pulls the time-series state-level vaccine data directly from covid19india.org.
#' @import data.table
#' @importFrom cli cli_alert_info
#' @importFrom janitor clean_names
#' @export
#' @examples
#' \dontrun{
#' get_state_vax()
#' }

get_state_vax <- function(
  path       = "https://api.covid19india.org/csv/latest/vaccine_doses_statewise_v2.csv",
  raw        = FALSE,
  keep_nat   = TRUE
) {

  if (keep_nat == TRUE) {
    cli::cli_alert_info("Also pulling national level data under `place = \"India\"`. Set `keep_nat == FALSE` to exclude by default")
  }

    d <- data.table::fread(path, showProgress = FALSE)

    if (raw == FALSE) {

      d <- d |>
        {\(x) data.table::setnames(x, names(x), janitor::make_clean_names(names(x)))}() |>
        data.table::DT(, date := as.Date(vaccinated_as_of, "%d/%m/%Y")) |>
        data.table::DT(, .(place       = state, date,
                           first_dose  = first_dose_administered,
                           second_dose = second_dose_administered,
                           total_doses = total_doses_administered)) |>
        data.table::DT(place == "Total", place := "India") |>
        data.table::merge.data.table(unique(covid19india::pop[, !c("abbrev")]), by = "place", all.x = TRUE) |>
        data.table::DT(, `:=` (pct_one_dose  = round(first_dose * 100 / population, 4),
                               pct_two_doses = round(second_dose * 100/ population, 4))) |>
        data.table::DT(, !c("population")) |>
        data.table::DT(order(date), daily_doses := total_doses - data.table::shift(total_doses), by = "place") |>
        {\(x) data.table::setcolorder(x, c("place", "date", "first_dose", "second_dose", "total_doses",
                                        "pct_one_dose", "pct_two_doses", "daily_doses"))}() |>
        data.table::setkeyv(cols = c("place", "date")) |>
        data.table::DT()

    }

  if (keep_nat == FALSE & raw == FALSE) {
    d <- d[place != "India"]
  }

  return(d)

}
