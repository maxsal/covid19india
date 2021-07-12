#' Install and load libraries from CRAN and GitHub
#' @param ... Packages as character ("") or bare (i.e. without quotes) names to install and load. Use `user_name/package_name` syntax for GitHub packages.
#' @return Install and load packages from CRAN and GitHub.
#' @import remotes
#' @import cli
#' @export
#' @examples
#' \dontrun{
#' biblioteca(c("tidyverse", "janitor", "maxsal/covid19india"))
#' }

biblioteca <- function(...) {

  libs      <- as.character(eval(substitute(alist(...))))
  preloaded <- 0
  installed <- 0
  newloads  <- 0

  for (i in seq_along(libs)) {

    tmp_lib <- sub(".*/", "", libs[i])

    if (tmp_lib %in% (.packages())) {
      preloaded <- preloaded + 1
      next
    }

    # non-github packages ----------
    if (grepl(pattern = "/", x = libs[i]) == FALSE) {

      if (nzchar(system.file(package = libs[i])) == FALSE) {

        cli::cli_alert_info(paste0("installing ", libs[i], "..."))
        install.packages(libs[i])
        installed <- installed + 1

      } else {

      # load library -----------
      suppressPackageStartupMessages(library(libs[i], character.only = T))
      newloads <- newloads + 1

      }

    # github packages ----------
    if (grepl(pattern = "/", x = libs[i]) == TRUE) {

      if (nzchar(system.file(package = tmp_lib)) == FALSE) {

        cli::cli_alert_info(paste0("installing ", libs[i], "..."))
        remotes::install_github(libs[i])
        installed <- installed + 1

      }

      # load library -----------
      suppressPackageStartupMessages(library(tmp_lib, character.only = T))
      newloads <- newloads + 1

      }
    }
  }

  # output summary ----------
  cli::cli_h1("Summary")
  if (preloaded > 0) cli::cli_alert_info("{preloaded} librar{?y/ies} already loaded")

  if (installed > 0) cli::cli_alert_info("{installed} librar{?y/ies} installed")

  if (newloads > 0) cli::cli_alert_success("{newloads} librar{?y/ies} loaded")

}

