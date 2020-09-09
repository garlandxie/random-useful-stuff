# libraries --------------------------------------------------------------------
library(tidyverse)
library(here)
library(vegan)
library(GGally)

# import -----------------------------------------------------------------------

# community data
comm_BA <- read.csv(here("data/final", "comm_matrix_BA.csv"), row.names = 1)
comm_BB <- read.csv(here("data/final", "comm_matrix_BB.csv"), row.names = 1)

# nestbox data
int_raw <- readxl::read_excel(
  here("data/original", "JSM_Data_TrapnestSynthesis2020_FINAL.xlsx"), 
  sheet = "B-interactiondata(per-nest)"
)

# site info
site_raw <- readxl::read_excel(
  here("data/original", "JSM_Data_TrapnestSynthesis2020_FINAL.xlsx"), 
  sheet = "A-metadata(per-site)"
)

# land use
l_250 <- read.csv(here("data/working", "land_use_250.csv"))
l_500 <- read.csv(here("data/working", "land_use_250.csv"))

# eda: nestbox data ----

int2 <-  int_raw %>%
  janitor::clean_names() %>%
  filter(!is.na(site_id) & taxa_ls == "Bee") %>%
  group_by(site_id, year) %>%
  summarize(tot_broods = sum(no_broodcells),
            tot_num_alive = sum(no_alive),
            spp_rich = length(unique(lower_species)),
            num_tubes = n(),
            prop_tubes = ceiling((num_tubes/30)*100)) %>%
  ungroup()

int2 %>%
  ggplot(aes(x = tot_broods)) +
  geom_histogram() + 
  labs(x = "Total number of brood (bee) cells per site") + 
  facet_wrap(~year) 

int2 %>%
  ggplot(aes(x = tot_num_alive)) +
  geom_histogram() + 
  labs(x = "Total number of alive (bee) organisms per site") + 
  facet_wrap(~year)

int2 %>%
  ggplot(aes(x = spp_rich)) + 
  geom_histogram() + 
  labs(x = "Species richness per site") + 
  facet_wrap(~year)

int2 %>%
  ggplot(aes(x = prop_tubes)) + 
  geom_histogram() + 
  facet_wrap(~year) + 
  labs(x = "Proportion of tubes occupied by bees for each site")

int2 %>%
  filter(year == 2011) %>%
  select(3:7) %>%
  GGally::ggpairs()

int2 %>%
  filter(year == 2012) %>%
  select(3:7) %>%
  GGally::ggpairs()

int2 %>%
  filter(year == 2013) %>%
  select(3:7) %>%
  GGally::ggpairs()

# eda: community data matrix ----

# check packaging

head(comm_BA, n = 5)
tail(comm_BA, n = 5)
dim(comm_BA)
colnames(comm_BA)
rownames(comm_BA)
summary(comm_BA)

head(comm_BB, n = 5)
tail(comm_BB, n = 5)
dim(comm_BB)
colnames(comm_BB)
rownames(comm_BB)
summary(comm_BB) 

# overall distribution of abundance 

# count cases for each abundance class
ab_alive <- table(unlist(comm_BA))
ab_alive <- data.frame(ab_alive)
colnames(ab_alive) <- c("Abundance", "Frequency")

# compare species: number of occurrences
BA_pres <- apply(comm_BA > 0, 2, sum)
BA_pres <- sort(BA_pres)

# compute percentage frequencies
BA_relf <- 100*BA_pres/nrow(comm_BA)
BA_relf <- round(sort(BA_relf), 1)

hist(BA_pres, main = "Species Occurence", 
     right = FALSE, las = 1, 
     xlab = "Number of Occurences", ylab = "Number of Species",
     col = "bisque")

hist(BA_relf, main = "Species Relative Frequencies", 
     right = FALSE, las = 1, 
     xlab = "Frequency of occurrences (%)", ylab = "Number of Species",
     col = "bisque")

# count cases for each abundance class
ab_brood <- table(unlist(comm_BB))
ab_brood <- data.frame(ab_brood)
colnames(ab_alive) <- c("Abundance", "Frequency")

# compare species: number of occurrences
BB_pres <- apply(comm_BB > 0, 2, sum)
BB_pres <- sort(BB_pres)

# compute percentage frequencies
BB_relf <- 100*BA_pres/nrow(comm_BB)
BB_relf <- round(sort(BB_relf), 1)

hist(BB_pres, main = "Species Occurence", 
     right = FALSE, las = 1, 
     xlab = "Number of Occurences", ylab = "Number of Species",
     col = "bisque")

hist(BB_relf, main = "Species Relative Frequencies", 
     right = FALSE, las = 1, 
     xlab = "Frequency of occurrences (%)", ylab = "Number of Species",
     col = "bisque")

# compare sites: species richness
sr_AB <- comm_BA %>%
  decostand(method = "pa") %>%
  rowSums() 

sr_AB <- data.frame(sr = sr_AB, 
                    site_id = names(sr_AB))

sr_BB <- comm_BB %>%
  decostand(method = "pa") %>%
  rowSums()
  
sr_BB <- data.frame(sr = sr_BB, 
                    site_id = names(sr_BB))

hist(sr_AB, main = "Number of Live Organisms", xlab = "Species Richness")
hist(sr_BB, main = "Brood Cells", xlab = "Species Richness")

site_raw %>%
  janitor::clean_names() %>%
  inner_join(sr_AB) %>%
  ggplot(aes(x = latitude, y = longitude, col = habitat_type, size = sr)) + 
  geom_point() + 
  labs(x = "Latitude",
       y = "Longitude") + 
  scale_color_discrete(name = "Habitat Type") + 
  theme_classic()

site_raw %>%
  janitor::clean_names() %>%
  inner_join(sr_BB) %>%
  ggplot(aes(x = latitude, y = longitude, col = habitat_type, size = sr)) + 
  geom_point() + 
  labs(x = "Latitude",
       y = "Longitude") + 
  scale_color_discrete(name = "Habitat Type") + 
  theme_classic()


site_raw %>%
  janitor::clean_names() %>%
  inner_join(sr_AB) %>%
  ggplot(aes(x = habitat_type, y = sr)) + 
  geom_boxplot() +
  labs(x = "Habitat Type", 
       y = "Species Richness A") + 
  theme_classic()

site_raw %>%
  janitor::clean_names() %>%
  inner_join(sr_BB) %>%
  ggplot(aes(x = habitat_type, y = sr)) + 
  geom_boxplot() +
  labs(x = "Habitat Type", 
       y = "Species Richness B") + 
  theme_classic()

# eda: environmental data ------------------------------------------------------

lw_250 <- l_250 %>% 
  select(class, id, prop_land_use) %>%
  pivot_wider(names_from = class, values_from = prop_land_use) %>% 
  select(site = `id`,
         prop_tree = `1`,
         prop_grass = `2`,
         prop_earth = `3`,
         prop_water = `4`,
         prop_build = `5`,
         prop_roads = `6`,
         prop_paved = `7`,
         prop_agri  = `8`
  ) %>%
  mutate(prop_urb = prop_roads + prop_paved + prop_build,
         across(is.numeric, ~replace_na(., 0)))

edge_250 <- l_250 %>%
  group_by(id) %>%
  summarize(sum_edge = sum(edge.density)) %>%
  ungroup()

# clean: 500m  
lw_500 <- l_500 %>% 
  select(class, id, prop_land_use) %>%
  pivot_wider(names_from = class, values_from = prop_land_use) %>% 
  select(site = `id`,
         prop_tree = `1`,
         prop_grass = `2`,
         prop_earth = `3`,
         prop_water = `4`,
         prop_build = `5`,
         prop_roads = `6`,
         prop_paved = `7`,
         prop_agri  = `8`
  ) %>%
  mutate(prop_urb = prop_roads + prop_paved + prop_build,
         across(is.numeric, ~replace_na(., 0)))

edge_500 <- l_500 %>%
  group_by(id) %>%
  summarize(sum_edge = sum(edge.density)) %>%
  ungroup()

# correlation matrix
lw_250 %>%
  inner_join(edge_250, by = c("site" = "id")) %>%
  rename(grass = prop_grass,
         earth = prop_earth, 
         tree  = prop_tree, 
         urban = prop_urb, 
         build = prop_build, 
         paved = prop_paved, 
         water = prop_water,
         roads = prop_roads,
         urb   = prop_urb,
         agr   = prop_agri,
         edge  = sum_edge) %>%
  ggpairs(2:11) + 
  ggtitle("250m spatial scale") + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# correlation matrix: 500m 
lw_500 %>%
  inner_join(edge_500, by = c("site" = "id")) %>%
  rename(grass = prop_grass,
         earth = prop_earth, 
         tree  = prop_tree, 
         urban = prop_urb, 
         build = prop_build, 
         paved = prop_paved, 
         water = prop_water,
         roads = prop_roads,
         urb   = prop_urb,
         agr   = prop_agri) %>%
  ggpairs(2:10) + 
  ggtitle("500m spatial scale") + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# maps: 500m 

site_500 <- site_raw %>%
  janitor::clean_names() %>%
  inner_join(lw_500, by = c("site_id" = "site")) 

site_500 %>%
    ggplot(aes(x = latitude, y = longitude, col = prop_urb)) + 
    geom_point() + 
    scale_colour_gradientn(colours = terrain.colors(10)) + 
    labs(color = NULL,
         title = "% Impervious Surface (500m scale)") +  
    theme_classic()

site_500 %>%
    ggplot(aes(x = latitude, y = longitude, col = prop_grass)) + 
    geom_point() + 
    labs(color = NULL,
         title = "% Grass (500m scale)") + 
    scale_colour_gradient2()+
    theme_classic()

site_500 %>%
    ggplot(aes(x = latitude, y = longitude, col = prop_tree)) + 
    geom_point() + 
    labs(color = NULL,
         title = "% Tree (500m scale)") + 
    scale_colour_gradient2()+
    theme_classic()

# maps: 250m
site_250 <- site_raw %>%
  janitor::clean_names() %>%
  inner_join(lw_250, by = c("site_id" = "site")) 

site_250 %>%
    ggplot(aes(x = latitude, y = longitude, col = prop_urb)) + 
    geom_point() + 
    scale_colour_gradientn(colours = terrain.colors(10)) + 
    labs(color = NULL,
         title = "% Impervious Surface (250m scale)") +  
    theme_classic()

grass_250 <- site_250 %>%
    ggplot(aes(x = latitude, y = longitude, col = prop_grass)) + 
    geom_point() + 
    labs(color = NULL,
         title = "% Grass (250m scale)") + 
    scale_colour_gradient2()+
    theme_classic()

site_250 %>%
    ggplot(aes(x = latitude, y = longitude, col = prop_tree)) + 
    geom_point() + 
    labs(color = NULL,
         title = "% Tree (250m scale)") + 
    scale_colour_gradient2()+
    theme_classic()

# maps - misc 

site_250 %>%
   ggplot(aes(x = latitude, y = longitude, col = plant_diversity_category)) + 
   geom_point()  +
   labs(color = NULL, 
        title = "Plant diversity") + 
   theme_classic()

edge_250 %>%
    ggplot(aes(x = Latitude, y = Longitude, col = sum_edge)) + 
    geom_point() + 
    labs(color = NULL, 
         title = "Edge Density") + 
    theme_classic()
