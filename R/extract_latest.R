#' Helper function
#' @param dat data set
#' @param group place variable
#' @param cols columns to be extracted
#' @return Data set of recent observations of selected variables
#' @import tidyverse
#' @export
#' @examples
#' \dontrun{
#'
#' }

extract_latest <- function(data, group = place, cols = c("total_tests", "tpr", "dbl", "ppt")) {
  out <- data %>%
    group_by({{ group }}) %>%
    filter(date == max(date)) %>%
    distinct(date, .keep_all = TRUE) %>%
    ungroup() %>%
    select({{ group }}, date, all_of(cols))
  if ("India" %in% data[[paste0(substitute(group))]]) {
    out[[paste0(substitute(group))]] <- recode(out[[paste0(substitute(group))]],
                                               "India" = "National estimate")
  }
  return(out)
}
