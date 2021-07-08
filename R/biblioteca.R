#' Install and load libraries from CRAN and GitHub
#' @param libs A character vector of libraries to install and load. Use `user_name/package_name` syntax for GitHub packages.
#' @return Install and load packages from CRAN and GitHub.
#' @import remotes
#' @export
#' @examples
#' \dontrun{
#' biblioteca(c("tidyverse", "janitor", "maxsal/covid19india"))
#' }

biblioteca <- function(libs) {

  for (i in seq_along(libs)) {

    # non-github packages ----------
    if (grepl(pattern = "/", x = libs[i]) == FALSE) {

      if (nzchar(system.file(package = libs[i])) == FALSE) {

        message(paste0("installing ", libs[i], "..."))
        install.packages(libs[i])

      }

      # load library -----------
      message(paste0("loading `", libs[i], "` library"))
      suppressPackageStartupMessages(library(libs[i], character.only = T))

    }

    # github packages ----------
    if (grepl(pattern = "/", x = libs[i]) == TRUE) {

      tmp_lib <- sub(".*/", "", libs[i])

      if (nzchar(system.file(package = tmp_lib)) == FALSE) {

        message(paste0("installing ", libs[i], "..."))
        remotes::install_github(libs[i])

      }

      # load library -----------
      message(paste0("loading `", libs[i], "` library"))
      suppressPackageStartupMessages(library(tmp_lib, character.only = T))

    }

  }

}

