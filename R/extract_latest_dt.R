#' Helper function - data.table style
#' @param dat data set
#' @param group place variable
#' @param cols columns to be extracted
#' @return Data set of recent observations of selected variables
#' @import dplyr
#' @importFrom tidyselect all_of
#' @export
#' @examples
#' \dontrun{
#' get_all_data() %>% extract_latest()
#' }

extract_latest_dt <- function(dat, group = place, clmns = c("total_tests", "tpr", "dbl", "ppt")) {

  out <- dat |>
    {\(x) x[x[, .I[date == max(date)], by = "place"]$V1]}() |>
    # data.table::setcolorder(neworder = c("place", "date", clmns)) |>
    data.table::DT(, c("place", "date", clmns), with = FALSE)

  if ("India" %in% out[[paste0(substitute(group))]]) {

    out[, place := data.table::fcase(place == "India", "National estimate", place != "India", place)]

  }

  return(out)

}
