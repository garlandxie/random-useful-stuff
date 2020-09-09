# libraries -------------------------------------------------------------------
library(here)
library(vegan)
library(dplyr)
library(readr)
library(tibble)
library(faraway)  # for multicollinearity
library(ape)
library(phytools)

# import ----------------------------------------------------------------------

# read data
env_250 <- read_csv(here("data/working", "land_use_250.csv"))
env_500 <- read_csv(here("data/working", "land_use_500.csv"))
comm <- readRDS(here("data/working", "comm_10Ab.rds"))
tree_ulm <- read.tree(here("data/original", "phylo_tree_ulm.new"))

# clean data: env 250 ----------------------------------------------------------

env_250_tidy <- env_250 %>%
  
  # pick composite measures (e.g., %)
  select(site, prop_tree_250, prop_grass_250, prop_urb_250) %>%
  
  # subset sites by the community data matrix
  filter(site %in% rownames(comm)) 

# data clean: env 500 ----------------------------------------------------------

env_500_tidy <- env_500 %>%
  
  # pick composite measures (e.g., %)
  select(site, prop_tree_500, prop_grass_500, prop_urb_500) %>%
  
  # subset sites by the community data matrix
  filter(site %in% rownames(comm)) 

# clean data: phylogeny --------------------------------------------------------

# tree stuff
tree_ulm <- drop.tip(tree_ulm, tip = c("Coelioxys_alternata",
                                       "Coelioxys_moesta",
                                       "Coelioxys_sayi",
                                       "Stelis_verticalis"))

tree_ulm$tip.label[tree_ulm$tip.label == "Megachile_centucularis"] <- "Megachile_centuncularis"
tree_ulm$tip.label[tree_ulm$tip.label == "Heriades_crucifera"] <- "Heriades_carinata"
tree_ulm$tip.label[tree_ulm$tip.label == "Osmia_atriventis"] <- "Osmia_atriventris"

# RDA: 250m --------------------------------------------------------------------

# check for co-linear environment variables
# % urban and % tree do covary
pairs(env_250_tidy[, -1])

# check for multicollinearity
# VIF values > 10 ==> should drop a variable
vif(env_250_tidy[, -1])

# 
comm_250 <- comm %>%
  rownames_to_column(var = "ID") %>%
  filter(ID %in% env_250_tidy$site) %>%
  column_to_rownames(var = "ID") %>%
  mutate_if(., is.integer, as.numeric) %>%
  decostand(method = "hellinger")

# remove % tree from df
env_250_tidy2 <- env_250_tidy[, c("prop_urb_250", "prop_grass_250")]

# perform RDA
rda_250 <- rda(formula = comm_250 ~ prop_urb_250 + prop_grass_250, 
               data = env_250_tidy2)

# adjusted R-2
(R2adj <- RsquareAdj(rda_250)$adj.r.squared)

# global permutation test
anova(rda_250, permutations = how(nperm = 999))

# permutation test for each canonical axis
anova(rda_250, by = "axis", permutations = how(nperm = 999))

# get species scores that are constrained by env vars
(spp_scores_250 <- rda_250$CCA$v)

# perform Blomberg's K test on species score of RDA axis 1
phylosig(tree = tree_ulm, 
         x = spp_scores_250[, "RDA1"],
         method = "K",
         test = TRUE, 
         nsim = 999)

# RDA: 500 m -------------------------------------------------------------------

# check for co-linear environment variables
# % urban and % tree do covary
pairs(env_500_tidy[, -1])

# check for multicollinearity
# VIF values > 10 ==> should drop a variable
vif(env_500_tidy[, -1])

# pre processing
comm_500 <- comm %>%
  rownames_to_column(var = "ID") %>%
  filter(ID %in% env_500_tidy$site) %>%
  column_to_rownames(var = "ID") %>%
  mutate_if(., is.integer, as.numeric) %>%
  decostand(method = "hellinger")

# remove % tree from df
env_500_tidy2 <- env_500_tidy[, c("prop_urb_500", "prop_grass_500")]

# perform RDA
rda_500 <- rda(formula = comm_500 ~ prop_urb_500 + prop_grass_500, 
               data = env_500_tidy2)

# adjusted R-2
(R2adj <- RsquareAdj(rda_500)$adj.r.squared)

# global permutation test
anova(rda_500, permutations = how(nperm = 999))

# permutation test for each canonical axis
anova(rda_500, by = "axis", permutations = how(nperm = 999))

# get species scores that are constrained by env vars
(spp_scores_500 <- rda_500$CCA$v)

phylosig(tree = tree_ulm, 
         x = spp_scores_500[,"RDA1"],
         method = "K",
         test = TRUE, 
         nsim = 999)

# save to disk -----------------------------------------------------------------

# rda summary
rda_250_summ <- summary(rda_250)
rda_500_summ <- summary(rda_500)

# write
saveRDS(object = rda_250_summ, file = here("data/working", "rda_250.rds"))
saveRDS(object = rda_500_summ, file = here("data/working", "rda_500.rds"))
