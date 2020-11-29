################################################################################
#
#'
#' Get sampling interval
#'
#' @param pop Total population
#' @param m Number of clusters to be selected
#'
#' @return An integer value for the sampling interval
#'
#' @examples
#' get_si(clusterList$pop, m = 30)
#'
#' @export
#'
#
################################################################################

get_si <- function(pop, m) {
  si <- floor(pop / m)
  return(si)
}


################################################################################
#
#'
#' Get the cumulative population size range
#'
#' @param v A vector of population sizes to apply the cumulative population
#'   size range calculations on
#' @param na How should missing values be treated? Either `na.rm` to remove or
#'   `na.omit` to omit by assigning a value of 0. Default is to omit.
#' @param as_char Logical. Should output be a character vector or a
#'   data.frame of two values. Default to FALSE.
#'
#' @return A vector of cumulative population size range
#'
#' @examples
#' roll_sum_range(v = clusterList$pop)
#'
#' @export
#'
#
################################################################################

roll_sum_range <- function(v,
                           na = c("na.rm", "na.omit"),
                           as_char = FALSE) {
  ## Check for NA
  na <- match.arg(na)

  if(na == "na.rm") {
    v <- v[!is.na(v)]
  }

  if(na == "na.omit") {
    v[is.na(v)] <- 0
  }

  ## calculate the ranges from 1 to X per row
  ur <- cumsum(x = v)
  lr <- (ur - v) + 1

  ## Check type of output required
  if(as_char) {
    z <- paste(lr, ur, sep = "-")
  } else {
    z <- data.frame(lower_range = lr, upper_range = ur)
  }

  ## Return z
  return(z)

}


################################################################################
#
#'
#' Perform proportional to population size (PPS) sampling based on SMART
#' approach
#'
#' @param df A data.frame of clusters/villages with their population size
#' @param pop A character value specifying the name of the variable in `df`
#'   for the population size information
#' @param m Number of clusters
#'
#' @return A data.frame which is a subset of `df` of the selected clusters for
#'   the survey sample. A new variable called `cluster_id` is created which
#'   identifies the cluster number and the reserved clusters.
#'
#' @examples
#' get_pps(df = clusterList, pop = "pop", m = 30)
#'
#' @export
#'
#
################################################################################

get_pps <- function(df, pop, m) {
  ## Determine how many clusters to select to include reserves
  #m_rc <- m + ceiling(m * 0.1)
  m_rc <- ifelse(m %in% 25:29, m + 3, ifelse(m %in% 30:39, m + 4, m + 5))

  ## Calculate the sampling interval
  si <- get_si(pop = sum(df[[pop]], na.rm = TRUE), m = m_rc)

  ## Determine the random start
  rd <- sample(x = 1:si, size = 1)

  ## Determine population sequence to choose clusters on
  clust_seq <- seq.int(from = rd, by = si, length.out = m_rc)

  ## Get cumulative population size range
  x <- roll_sum_range(v = df[[pop]], na = "na.omit", as_char = FALSE)

  ## Concatenating object
  y <- NULL

  ## Select clusters from list
  for (i in clust_seq) {
    y <- rbind(y, df[i >= x$lower_range & i <= x$upper_range, ])
  }

  ## Randomly select reserved clusters
  rc <- sample(x = 1:nrow(y), size = m_rc - m)

  ## Identify reserved clusters from list
  z1 <- y[!(1:nrow(y)) %in% rc, ]
  z2 <- y[rc, ]

  ## Concatenate
  z <- data.frame(rbind(z1, z2))

  ## Rename rows
  row.names(z) <- 1:nrow(z)

  ## Create cluser ID
  cluster_id <- c(paste("C", 1:nrow(z1), sep = ""),
                  paste("RC", 1:nrow(z2), sep = ""))

  z <- data.frame(cluster_id, z)

  ## Return z
  return(z)
}

