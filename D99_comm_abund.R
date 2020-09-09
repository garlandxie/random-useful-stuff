# libraries -------------------------------------------------------------------
library(here)  # for creating relative file-paths
library(dplyr)
library(vegan)
library(ggplot2)

# import ----------------------------------------------------------------------

# wasps
wasp_comm <- read.csv(
  here(
    "data/final",
    "comm_matrix_WB.csv"
    ),
  row.names = 1
  )

# bees
bees_comm <- read.csv(
  here(
    "data/final",
    "comm_matrix_BB.csv"
  ),
  row.names = 1
)

# data cleaning  ---------------------------------------------------------------

# abundance cut-off: plots with more than 7 individuals
bees_comm_10 <- bees_comm[rowSums(bees_comm) >= 10, ]
wasp_comm_10 <- wasp_comm[rowSums(wasp_comm) >= 10, ]

# save to disk -----------------------------------------------------------------

# wasps
saveRDS(
  wasp_comm_10, 
  file = here(
    "data/working", 
    "wasp_comm_10.rds"
    )
  )

# bees
saveRDS(
  bees_comm_10, 
  file = here(
    "data/working", 
    "bees_comm_10.rds"
  )
)
