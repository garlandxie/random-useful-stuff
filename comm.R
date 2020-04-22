# libraries -------------------------------------------------------------------
library(readxl)
library(dplyr)
library(stringr)

# import ----------------------------------------------------------------------

comm_df <- read_excel("D99_community_matrix.xlsx", skip = 1)

# data cleaning: separate datasets by year  -----------------------------------

# 2011
comm_11 <- comm_df %>%
  janitor::clean_names() %>%
  select(no, site, row_labels, lat, long, matches("11")) %>%
  select(-matches("abund"))

# 2012
comm_12 <- comm_df %>%
  janitor::clean_names() %>%
  select(no, site, row_labels, lat, long, matches("12")) %>%
  select(-matches("abund"))

# 2013 
comm_13 <- comm_df %>%
  janitor::clean_names() %>%
  select(no, site, row_labels, lat, long, matches("13")) %>%
  select(-matches("abund"))

# data cleaning: extracting bee data ------------------------------------------

# abbreviated species codes
bee_spp <-c("anth_man", "ant_term", "her_car", "her_var")
            
# abbreviated genus codes            
bee_genus <- c("chel", "hop", "hyl", "meg", 
               "osm", "coel", "camp", "stel")
           

# 2011
bee_11 <- comm_11 %>%
  select(no, site, row_labels, 
         lat, long, 
         matches(c(bee_spp, bee_genus))) %>%
  rename_at(vars(ends_with("11")), 
            .funs = ~ str_replace(., pattern = "_11", replacement = ""))

# 2012
bee_12 <- comm_12 %>%
  select(no, site, row_labels, 
         lat, long, 
         matches(c(bee_spp, bee_genus))) %>%
  rename_at(vars(ends_with("12")), 
            .funs = ~ str_replace(., pattern = "_12", replacement = ""))

# 2013
bee_13 <- comm_13 %>%
  select(no, site, row_labels, 
         lat, long, 
         matches(c(bee_spp, bee_genus))) %>%
  rename_at(vars(ends_with("13")), 
            .funs = ~ str_replace(., pattern = "_13", replacement = ""))

# data cleaning: abundance across 2011 - 2013 ---------------------------------

# get code names for solitary bee species (e.g, meg_rot)
bee_codes <- bee_11 %>%
  select(matches(c(bee_spp, bee_genus))) %>%
  colnames()

# sum up abundances for each species at each site across 
# 2011-2013
bee_total <- bee_11[, bee_codes] + 
             bee_12[, bee_codes] +
             bee_13[, bee_codes]

# attach site information to total species-level abundance
bee_final <- bee_11 %>%
  select(no, site, row_labels, lat, long) %>%
  cbind(bee_total)