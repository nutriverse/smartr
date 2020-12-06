################################################################################
#
#'
#' Calculate number of households to sample given a target sample size of
#' children 6-59 months old
#'
#' @param ss Target sample size of children 6-59 months old
#' @param hh_size Average household size. Default to 5 household members
#' @param u5 Proportion of the population who are under 5 years old. Default to
#'   0.15 (15\%)
#' @param f Proportion of under 5 who are 6-59 months in age. Default to 0.9
#'   (90\%)
#'
#' @return A numeric value for number of households to sample
#'
#' @examples
#' calculate_ss_hh(ss = 500, hh_size = 6.2)
#'
#' @export
#'
#
################################################################################

calculate_ss_hh <- function(ss, hh_size = 5, u5 = 0.15, f = 0.9) {
  ## Check input values are numeric
  if (!is.numeric(ss)) {
    stop("Sample size should be numeric. Try again.")
  }

  if (!is.numeric(hh_size)) {
    stop("Household size should be numeric. Try again.")
  }

  if (!is.numeric(u5)) {
    stop("Proportion under 5 should be numeric. Try again.")
  }

  if (!is.numeric(f)) {
    stop("Proportion 6-59 months should be numeric. Try again.")
  }

  ## Calculate
  hh_ss <- ss / (hh_size * u5 * f)

  ## Return
  return(hh_ss)
}
