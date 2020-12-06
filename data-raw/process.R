## Load libraries
library(readxl)
library(dplyr)

## Read cluster assignment data
villageList1 <- read_xlsx(path = "data-raw/smartfiles/Cluster assignment.xlsx",
                         range = "A3:C284")

village_id <- 1:nrow(villageList1)

villageList1 <- tibble::tibble(village_id, villageList1)

names(villageList1) <- c("village_id", "village_name", "pop", "cum_pop")

usethis::use_data(villageList1, overwrite = TRUE, compress = "xz")

write.csv(villageList1, "data-raw/smartfiles/villageList1.csv", row.names = FALSE)

## Create shorter village list

villageList2 <- data.frame(village_id = paste("Village", 1:10),
                           pop = c(500, 400, 160, 650, 520,
                                   640, 700, 100, 470, 60))

villageList2 <- tibble::tibble(villageList2)

usethis::use_data(villageList2, overwrite = TRUE, compress = "xz")

write.csv(villageList2, "data-raw/smartfiles/villageList2.csv", row.names = FALSE)
