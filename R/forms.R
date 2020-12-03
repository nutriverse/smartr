################################################################################
#
#'
#' Create a Google Sheets data entry form
#'
#' @param name A string to name the newly created spreadsheet.
#' @param sheets Optional input for initialising worksheets.
#' @param view Logical. Should the newly created sheet be opened in the
#'   current browser? Default to TRUE.
#'
#' @return A spreadsheet with specified name and specified sheets
#'   in user's Google Sheets account.
#'
#' @author Ernest Guevarra
#'
#' @examples
#' ## Create a Google Sheet named "village_list"
#' if (googlesheets4::gs4_has_token()) {
#'   create_gsheet_form("SMART Survey")
#' }
#'
#' @export
#'
#
################################################################################

create_gsheet_form <- function(name, sheets = NULL, view = TRUE) {
  if (!requireNamespace(package = "googlesheets4", quietly = TRUE)) {
    stop("The 'googlesheets4' package is required. Install to use this function.",
         call. = TRUE)
  }

  name <- names %>%
    stringr::str_replace_all(pattern = " ", replacement = "_")

  googlesheets4::gs4_create(name = name, sheets = sheets)
}

