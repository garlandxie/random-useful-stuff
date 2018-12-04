# libraries --------------------------------------------------------------------
library(tidyverse)

# import data ------------------------------------------------------------------
allpin <- read_delim("allpin.txt", delim = "\t")


# clean ------------------------------------------------------------------------

# long to wide format for percent cover
df <- allpin %>% 
  gather(key = date, 
         value = spp_biomass, 
         c(-Sp, -Treat, -Rep, -QR, -Quad)) %>%
  drop_na()

# get percent per spp in each treatment
per_spp <- df1 %>%
  filter(spp_biomass > 0) %>%
  group_by(date, Treat, Rep, Sp) %>%
  summarize(biomass = sum(spp_biomass)) %>% 
  arrange(date, Treat, Rep, Sp) %>%
  mutate(comm_biomass = sum(biomass), 
         per_biomass  = (biomass/comm_biomass)*100)

# sanity check
check1 <- per_spp %>%
  group_by(date, Treat, Rep) %>%
  summarize(sum_biomass = sum(per_biomass)) %>%
  pull(sum_biomass) %>%
  var() # all values are the same = zero variance
  



  

