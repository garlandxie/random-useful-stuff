### Data import and cleaning for IJ Rhizo histogram outputs
### This script is part of a research project @ Lundholm Lab (SMU)
### Code is written by Garland Xie (garlandxie@gmail.com)

# import libraries -------------------------------------------------------------
library(tidyverse) # for tidy data 
library(here)      # for creating relative file paths

# get file names for each histogram --------------------------------------------
folder_name <- "plma-04_output" # for interns

histo_list <- list.files(path = here(folder_name), 
                         pattern = "Histo")

# read a histogram file --------------------------------------------------------
read_histograms <- function(txt_file) {
  
  # works only for Windows OS though
  # get block id, pot id, treatment id and scan id from file name 
  # assuming the file name follows a specific format
  # e.g., C:/users/folder/B1_SYNO-01_1_600dpi_GX
  ids <- str_split(txt_file, pattern = "/") %>% 
    unlist %>% 
    tail(n = 1) %>% # grab the last index
    str_split(pattern = "_") %>%
    unlist
  
  # make df of traits from a given scan
  trait_df <- read_delim(txt_file, delim = "     ") %>%
    
    # select only two columns and rename them 
    select("length_mm" = "Root Length (mm)",
           "radius_mm" = "Root Radius (mm)") %>%
    
    # convert columns as numeric values 
    mutate(length_mm = length_mm %>% as.numeric, 
           radius_mm = radius_mm %>% as.numeric) %>% 
    
    # remove the last row (number doesn't make sense here)
    slice(-nrow(.)) %>% 
    
    # root length mm has to be above zero mm AND 
    # root diameter has to be below two mm 
    filter(length_mm > 0 & radius_mm < 2) %>% 
    
    # add in useful information to complete df 
    mutate(block   = ids[1], 
           pot_id  = ids[2],
           trt     = ids[3],
           scan    = ids[4] %>% as.numeric)  
  
  # return df 
  return(trait_df)
}

# row bind every histogram file by iteration -----------------------------------
final_df <- map_dfr(here(folder_name, histo_list), 
                    read_histograms)

# summarize data ---------------------------------------------------------------
summary_df <- final_df %>% 
  group_by(block, pot_id, trt) %>%
  summarise(mean_radius_mm = mean(radius_mm, na.rm = TRUE),
            sum_length_mm  = sum(length_mm, na.rm = TRUE))


