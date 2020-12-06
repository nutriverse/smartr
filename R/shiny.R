################################################################################
#
#' Run the smartr Shiny app
#'
#' @return NULL
#'
#' @examples
#' if (interactive()) run_smartr()
#'
#' @export
#'
#
################################################################################

run_smartr <- function() {
  appDir <- system.file("smartr", package = "smartr")

  if (appDir == "") {
    stop("Could not find Shiny directory. Try re-installing smartr.",
         call. = FALSE)
  }

  shiny::runApp(appDir, launch.browser = TRUE, display.mode = "normal")
}
