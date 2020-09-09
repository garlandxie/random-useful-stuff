# calculating regression models for functional diversity -----------------------
# author(s): Nicholas Sookhan, Garland Xie

# libraries --------------------------------------------------------------------
library(here)    # for creating relative file-paths
library(dplyr)
library(ggplot2)
library(GGally)
library(car)
library(visreg)
library(sp)
library(spdep)

# import -----------------------------------------------------------------------

# site info
site <- read.csv(here("data/working", "site_tidy.csv"))

# ses mpd 
ses_mpd_full <- readRDS(here("data/working", "ses_mpd_full.rds"))
ses_mpd_3ab <- readRDS(here("data/working", "ses_mpd_3ab.rds"))
ses_mpd_5ab <- readRDS(here("data/working", "ses_mpd_5ab.rds"))
ses_mpd_10ab <- readRDS(here("data/working", "ses_mpd_10ab.rds"))

# land cover
l_250 <- read.csv(here("data/working", "land_use_250.csv"))
l_500 <- read.csv(here("data/working", "land_use_500.csv"))

# functions
source(here("src", "functions.R"))

# data cleaning ----------------------------------------------------------------

# 250m 
reg_250 <-  ses_mpd_10ab %>%
  tibble::rownames_to_column(var = "id") %>%
  full_join(l_250, by = c("id" = "site")) %>%
  full_join(site, by = c("id" = "site_id")) %>%
  select(site = id, 
         longs = longitude, 
         lats  = latitude, 
         ses_mpd = mpd.obs.z, 
         prop_tree_250, 
         prop_grass_250, 
         prop_urb_250) %>%
  filter(!is.na(ses_mpd))

# 500m
reg_500 <-  ses_mpd_10ab %>%
  tibble::rownames_to_column(var = "id") %>%
  full_join(l_500, by = c("id" = "site")) %>%
  full_join(site, by = c("id" = "site_id")) %>%
  select(site = id, 
         longs = longitude, 
         lats  = latitude, 
         ses_mpd = mpd.obs.z, 
         prop_tree_500, 
         prop_grass_500, 
         prop_urb_500) %>%
  filter(!is.na(ses_mpd))

# exploratory data analysis: environmental gradient ----------------------------

# percent impervious cover 
reg_250 %>%
  ggplot(aes(x = longs, y = lats, col = prop_urb_250)) + 
  geom_point() + 
  scale_colour_gradientn(colours = terrain.colors(10)) + 
  theme_minimal()

reg_500 %>%
  ggplot(aes(x = longs, y = lats, col = prop_urb_500)) + 
  geom_point() + 
  scale_colour_gradientn(colours = terrain.colors(10)) + 
  theme_minimal()

# percent tree cover 
reg_250 %>%
  ggplot(aes(x = longs, y = lats, col = prop_tree_250)) + 
  geom_point() + 
  scale_colour_gradientn(colours = terrain.colors(10)) + 
  theme_minimal()

reg_500 %>%
  ggplot(aes(x = longs, y = lats, col = prop_tree_500)) + 
  geom_point() + 
  scale_colour_gradientn(colours = terrain.colors(10)) + 
  theme_minimal()

# percent grass cover 
reg_250 %>%
  ggplot(aes(x = longs, y = lats, col = prop_grass_250)) + 
  geom_point() + 
  scale_colour_gradientn(colours = terrain.colors(10)) + 
  theme_minimal()

reg_500 %>%
  ggplot(aes(x = longs, y = lats, col = prop_tree_500)) + 
  geom_point() + 
  scale_colour_gradientn(colours = terrain.colors(10)) + 
  theme_minimal()

# exploratory data analysis: outliers  -----------------------------------------

# sensu Zuur et al. 2010. MEE
# general regression models: diversity ~ % tree + % grass + % urb

# find outliers for indepndent variables using cleveland dot-plots

# 250

plot_clev_dot(df = reg_250, 
              var = prop_tree_250, 
              x_axis_title = "Percent Grass Cover (250m)")

plot_clev_dot(df = reg_250, 
              var = prop_grass_250, 
              x_axis_title = "Percent Tree Cover (250m)")

plot_clev_dot(df = reg_250, 
              var = prop_urb_250, 
              x_axis_title = "Percent Impervious Surface (250m)")

# 500m 

plot_clev_dot(df = reg_500, 
              var = prop_tree_500, 
              x_axis_title = "Percent Grass Cover (500m)")

plot_clev_dot(df = reg_500, 
              var = prop_grass_500, 
              x_axis_title = "Percent Tree Cover (500m)")

plot_clev_dot(df = reg_500, 
              var = prop_urb_500, 
              x_axis_title = "Percent Impervious Surface (500m)")

# find outliers for response variables using cleveland dot-plots

plot_clev_dot(df = reg_250, 
              var = ses_mpd,
              x_axis_title = "standardized MPD")

# exploratory data analysis: correlation matrix --------------------------------

# check for the following:
# (1) collinear variables 
# (2) normally distributed data

reg_250 %>%
  select(ses_mpd, prop_tree_250, prop_grass_250, prop_urb_250) %>%
  ggpairs()

reg_500 %>%
  select(ses_mpd, prop_tree_500, prop_grass_500, prop_urb_500) %>%
  ggpairs()

# exploratory data analysis: relationships between X and Y variables -----------

# check for clear patterns between response and explanatory relationships
# use a LOESS smoother to aid visual interpretation
# remove 95% CI's since we're still exploring data

# 250 m
reg_250 %>%
  ggplot(aes(x = prop_urb_250, y = ses_mpd)) + 
  geom_smooth(method = "loess", se = FALSE) + 
  geom_point() + 
  labs(x = "Percent impervious surface (250m)",
       y = "ses MPD")

reg_250 %>%
  ggplot(aes(x = prop_tree_250, y = ses_mpd)) + 
  geom_smooth(method = "loess", se = FALSE) + 
  geom_point() + 
  labs(x = "Percent tree cover (250m)",
       y = "ses MPD")

reg_250 %>%
  ggplot(aes(x = prop_grass_250, y = ses_mpd)) + 
  geom_smooth(method = "loess", se = FALSE) + 
  geom_point() + 
  labs(x = "Percent grass cover (250m)",
       y = "ses MPD")

# 500m
reg_500 %>%
  ggplot(aes(x = prop_urb_500, y = ses_mpd)) + 
  geom_smooth(method = "loess", se = FALSE) + 
  geom_point() + 
  labs(x = "Percent impervious surface (500m)",
       y = "ses MPD")

reg_500 %>%
  ggplot(aes(x = prop_tree_500, y = ses_mpd)) + 
  geom_smooth(method = "loess", se = FALSE) + 
  geom_point() + 
  labs(x = "Percent tree cover (500m)",
       y = "ses MPD")

reg_500 %>%
  ggplot(aes(x = prop_grass_500, y = ses_mpd)) + 
  geom_smooth(method = "loess", se = FALSE) + 
  geom_point() + 
  labs(x = "Percent grass cover (500m)",
       y = "ses MPD")

# exploratory data analysis: independent observations for Y variable -----------

reg_250 %>%
  ggplot(aes(x = longs, y = lats, col = ses_mpd)) + 
  geom_point() + 
  labs(x = "Longitude", 
       y = "Latitude") + 
  scale_colour_gradientn(colours = terrain.colors(10)) + 
  theme_minimal()

# hypothesis testing: multiple regression (250m) -------------------------------

# first fit
lm_250 <- lm(ses_mpd ~ prop_grass_250 + prop_tree_250 + prop_urb_250, 
   data = reg_250)
vif(lm_250)

# remove collinear variables
lm_250 <- update(lm_250, ~. -prop_tree_250)
vif(lm_250)

# get summary
summary(lm_250)

# model diagnostics
plot(lm_250)
hist(lm_250$resid)


# hypothesis testing: multiple regression (500m) -------------------------------

# first fit
lm_500 <- lm(ses_mpd ~ prop_grass_500 + prop_tree_500 + prop_urb_500, 
             data = reg_500)
vif(lm_500)

# remove collinear variables
lm_500 <- update(lm_500, ~. -prop_tree_500)
vif(lm_500)

# get summary
summary(lm_500)

# model diagnostics
plot(lm_500)
hist(lm_500$resid)

# spatial autocorrelation test -------------------------------------------------

# 250m 
coords_250 <- reg_250 %>%
  select(site, longs, lats)

coordinates(coords_250) <- ~lats + longs
proj4string(coords_250) <- CRS("+proj=utm +zone=17 +ellps=GRS80 +datum=NAD83 
                            +units=m +no_defs")

lm.morantest(lm_250, 
             listw = nb2listw(knn2nb(knearneigh(x = coords_250, k = 8)),
                              style = "W"))

# 500m 
coords_500 <- reg_500 %>%
  select(site, longs, lats)

coordinates(coords_500) <- ~lats + longs
proj4string(coords_500) <- CRS("+proj=utm +zone=17 +ellps=GRS80 +datum=NAD83 
                            +units=m +no_defs")

lm.morantest(lm_500, 
             listw = nb2listw(knn2nb(knearneigh(x = coords_500, k = 8)),
                              style = "W"))

# save to disk -----------------------------------------------------------------

write.csv(x = reg_250, here("data/final", "reg_mpd_250.csv"))
write.csv(x = reg_500, here("data/final", "reg_mpd_500.csv"))
