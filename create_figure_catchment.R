# Code developed by Garland Xie
# Goal: create a reproducible map showing inequity in large parks in Toronto

# libraries --------------------------------------------------------------------
library(sf)       # for manipulating geospatial files 
library(ggplot2)  # for visualizing data
library(here)     # for creating relative file-paths
library(dplyr)    # for manipulating data
library(stringr)  # for manipulating string data
library(ggsn)     # for adding map symbols 

# import data ------------------------------------------------------------------

catchment <- sf::read_sf("data", "CatchmentAreas")

boundary <- sf::read_sf("data", "TorontoNeighbourhoods")

parks <- sf::read_sf(
    "data", 
    "GreenspaceShapefiles", 
    "SimplifiedLargeParks.shp"
  ) 

neighbourhoods_2016 <- sf::read_sf(
  here("data", 
       "TorontoNeighbourhoods",
       "2016")
  )

# visualize data ---------------------------------------------------------------

(park_area_catch <- ggplot() + 
  
  # toronto boundary
  geom_sf(
    colour = "grey", 
    alpha = 0.6, 
    data = boundary
    ) + 
  
  # neighbourhoods
  geom_sf(
    data = neighbourhoods_2016,
    color = "white", 
    fill = NA
  ) + 
   
  # catchment areas
  geom_sf(
    colour = NA, 
    fill = "orange", 
    alpha = 0.3, 
    data = catchment
    ) + 

  # large parks
  geom_sf(
    colour = "#009E73", 
    fill = "#009E73", 
    data = parks
    ) + 
  
  # north arrow
  ggsn::north(
    data = parks,
    location = "topright",
  ) + 
   
  # scale-bar
  ggsn::scalebar(
    data = boundary, 
    dist = 10 ,
    transform = TRUE, 
    dist_unit = "km",
    height = 0.001, 
    st.size = 3) + 
   
  # labels
  labs(x = NULL, y = NULL) + 

  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = "white"))
)

# save to disk -----------------------------------------------------------------

ggsave(
  plot = park_area_catch, 
  file = here("figures", "park_catch_area.pdf"),
  device = "pdf", 
  units = "in", 
  height = 5, 
  width = 5
)
  
    


