# Urbanization Data
# By: Madison Marshall and Garland Xie
# Description: Find percent urbanization (impervious survface cover) of 500m
#              buffers set from sites in the GTA using City of Toronto 2018
#              land cover data.

# Set working directory and install packages ---------------------------------

#setwd("UofT2019-2020/Master's/BIMBY/Mapping")

# install packages
#install.packages('sf')
#install.packages('sp')
#install.packages('readxl')
#install.packages('dplyr')
#install.packages('ggplot2')

# load packages
library(sf) # for manipulating GIS data
library(sp) # for manipulating GIS data
library(readxl) # to read excel files
library(dplyr) # for that weird %>% thing
library(ggplot2) # for plotting and visualizing data

# Import data ----------------------------------------------------------------------

# land cover RDS objects
lc_5 <- readRDS("roads.RDS")
lc_6 <- readRDS("building.RDS")
lc_7 <- readRDS("other_paved.RDS")

# site data
site_df <- read.csv("SiteData.csv", stringsAsFactors = FALSE)

# Data cleaning ---------------------------------------------------------------

# combine land cover into a single df
lc_urban <- rbind(lc_5, lc_6, lc_7)


# set CRS (UTM 17N)
# convert lat longs
lat_longs <- site_df %>%
  mutate(dec_deg_long = dec_deg_long * -1) %>%
  st_as_sf(coords = c("dec_deg_long", "dec_deg_lat"), remove = FALSE) %>%
  st_set_crs(4326) %>%
  st_transform(crs = "+proj=tmerc +lat_0=0 +lon_0=-79.5 +k=0.9999 +x_0=304800 +y_0=0 +ellps=clrk66 +units=m +no_defs")

# convert land cover data
lc_u <- lc_urban %>%
  st_as_sf() %>%
  st_combine()

# Geospatial analysis ---------------------------------------------------------

# create buffers
site_buffers = st_buffer(lat_longs, 500)

# create intersection of lc_u polygons and site_buffer polygons
site_urban = st_intersection(site_buffers, lc_u)

# area of a single buffer
area_buffer <-st_area(site_buffers[1,])

# get percent impervious cover
summary_site_urban <- site_urban %>%
  group_by(ID, dec_deg_lat, dec_deg_long) %>%
  summarize(area_imperv = sum(st_area(geometry))) %>%
  as_tibble() %>%
  select(-geometry)

percent_imp <- summary_site_urban %>%
  mutate(area_imperv = area_imperv/area_buffer)

# Export ----------------------------------------------------------------------

write.csv(site_df, file = "SiteUrbanizationData.csv")
st_write(site_urban, file = "SiteUrbanizationData.shp")
