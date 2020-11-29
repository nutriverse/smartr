## Load libraries
library(readxl)
library(dplyr)

## Read cluster assignment data
clusterList <- read_xlsx(path = "data-raw/smartfiles/Cluster assignment.xlsx",
                         range = "A3:C284")

village_id <- 1:nrow(clusterList)

clusterList <- tibble::tibble(village_id, clusterList)

names(clusterList) <- c("village_id", "village_name", "pop", "cum_pop")

usethis::use_data(clusterList, overwrite = TRUE, compress = "xz")

