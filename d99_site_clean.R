# libraries ----
library(readxl)    # for reading excel files
library(here)      # for creative relative file-paths
library(tidyverse)

# import ---

# site info
site_raw <- read_excel(
  here(
    "data/original", 
    "JSM_Data_TrapnestSynthesis2020_FINAL.xlsx"
    ), 
  sheet = "A-metadata(per-site)"
)

# check package -----
str(site_raw)
head(site_raw, n = 5)
tail(site_raw, n = 5)

# data clean ----
site_tidy <- site_raw %>%
  janitor::clean_names() 

# quick data viz check ----

# GTA coordinates
site_tidy %>%
  ggplot(aes(x = longitude, y = latitude)) + 
  geom_point()

# frequency plot of habitat types
site_tidy %>%
  ggplot(aes(x = habitat_type)) + 
  geom_bar() + 
  labs(x = "Habitat Type") + 
  coord_flip()

# frequency plot of local plant diversity
site_tidy %>%
  ggplot(aes(x = plant_diversity_category)) + 
  geom_bar() + 
  labs(x = "Plant Diversity") + 
  coord_flip()

# save to disk ----
write.csv(site_tidy, 
          here("data/working",
               "site_tidy.csv")
          )
