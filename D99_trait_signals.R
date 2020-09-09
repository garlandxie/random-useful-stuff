# libraries --------------------------------------------------------------------
library(here) 
library(dplyr)
library(ape)
library(geiger)
library(caper)
library(tibble)

# import -----------------------------------------------------------------------

# trait matrix
trait <- readRDS(here("data/final", "trait_matrix.rds"))

# phylo tree (with soft polytomies)
tree <- read.tree(here("data/original", "phylo_tree_ulm.new"))

# check packaging --------------------------------------------------------------

# raw trait matrix
glimpse(trait)
head(trait, n = 5)
tail(trait, n = 5)

# basic information on the phylogeny
plot(tree)

is.rooted(tree) 
is.ultrametric(tree) 

tree$edge
tree$Nnode
tree$node.label
tree$edge.length
cbind(tree$edge, tree$edge.length)
tree$tip.label

# tree scaling -----------------------------------------------------------------

# imported tree is not ultrametric - requires tree scaling
# estimating divergence times using penalized maximum likelihood 
# REF: Sanderson. 2002. Molecular Biology and Evolution. 19(1): 101-109

# outlined steps below are closely adapted from: 
# Cadotte and Davies. 2015. Phylogenies in Ecology 
# Chapter 2

# first, we need to obtain three set of parameters from a chronos object class
# (1) penalized log-likelihood
# (2) rate transformation for each edge in the phylogeny
# (3) lambda values 

# create a gradient of lambda values
l <- c(0:10000)

# specific an empty object to hold plogLik values
LL.out <- NULL

# run a for loop to contain logLik values
for (i in 1:length(l)) {
  LL.out[i] <- attributes(chronos(tree,lambda=l[i]))$ploglik
}

# select maximum plogLik value and corresponding lambda
# lambda value was 0
lambda.max <- l[LL.out==max(LL.out)]

# double check: plogLik vs log(lambda)
plot(log(l+1), LL.out,
     type = "l", lwd = 3, col = "gray",
     xlab = expression(paste("Log(",lambda, ")")),
     ylab = "Log likelihood") 

# rescale tree using lambda.max value
tree_rescale <- chronos(tree, lambda = lambda.max)

# more double-checks
plot(tree_rescale)
is.ultrametric(tree_rescale)

# save tree
write.tree(tree_rescale, here("data/final", "tree_rescale_ulm.new"))

# read in tree as a phylo object class
tree_rescale_ulm <- read.tree(here("data/final", "tree_rescale_ulm.new"))

# remove node labels on phylogenetic tree
tree_nodes_null <- tree_rescale_ulm 
tree_nodes_null$node.label <- NULL

# clean taxa labels name 

tree_nodes_null <- drop.tip(
  tree_nodes_null,
  tip = c(
    "Coelioxys_alternata",
    "Coelioxys_moesta",
    "Coelioxys_sayi",
    "Stelis_verticalis")
  )

tree_nodes_null$tip.label[tree_nodes_null$tip.label == "Megachile_centucularis"] <- "Megachile_centuncularis"
tree_nodes_null$tip.label[tree_nodes_null$tip.label == "Heriades_crucifera"] <- "Heriades_carinata"
tree_nodes_null$tip.label[tree_nodes_null$tip.label == "Osmia_atriventis"] <- "Osmia_atriventris"

# phylogenetic signals: ITD ----------------------------------------------------

# prep
ITD <- trait$itd
names(ITD) <- trait$spp
tree_di2multi <- multi2di(tree_nodes_null) # binary tree

# trait evolution models 
lambda.itd <- fitContinuous(tree_di2multi, ITD, model = "lambda")
star.tree.itd <- rescale(tree_di2multi, "lambda", 0)
lambda.star.itd <- fitContinuous(star.tree.itd, ITD, model = "BM")

# likelihood ratio test
# p < 0.05: infer phylogenetic structure for the given focal trait
LR.itd <-2*(lambda.itd$opt$lnL - lambda.star.itd$opt$lnL)
pchisq(LR.itd, df = 1, lower.tail = F)

# phylogenetic signals: native status ------------------------------------------

# fit model using phylogenetic D statistic
# statistical inference: 0 = BM model, 1 = no signal
native_D <- 
  phylo.d(
    data = as.data.frame(trait), 
    phy = tree_nodes_null, 
    names.col = spp, 
    binvar = native_y_n
    )

# phylogenetic signal: leaf hair -----------------------------------------------

leafhair_D <- 
  phylo.d(
    data      = as.data.frame(trait),
    phy       = tree_nodes_null,
    names.col = spp, 
    binvar    = leaf_hair
  )

# phylogenetic signal: leaf cut -------------------------------------------------

leafcut_D <- 
  phylo.d(
    data      = as.data.frame(trait), 
    phy       = tree_nodes_null, 
    names.col = spp, 
    binvar    = leaf_cut
    )

# phylogenetic signal: leaf pulp -----------------------------------------------

leafpulp_D <- 
  phylo.d(
    data      = as.data.frame(trait), 
    phy       = tree_nodes_null, 
    names.col = spp, 
    binvar    = leaf_pulp
    )

# phylogenetic signal: resin ---------------------------------------------------

resin_D <- 
  phylo.d(
    data      = as.data.frame(trait), 
    phy       = tree_nodes_null, 
    names.col = spp, 
    binvar    = resin
    )

# phylogenetic signal: mud -----------------------------------------------------

mud_D <- 
  phylo.d(
    data = as.data.frame(trait), 
    phy = tree_nodes_null, 
    names.col = spp, 
    binvar = mud
    )

# phylogenetic signal: stone ---------------------------------------------------

stone_D <- 
  phylo.d(
    data = as.data.frame(trait), 
    phy = tree_nodes_null, 
    names.col = spp, 
    binvar = stone
    )

# phylogenetic signal: none ----------------------------------------------------

none_D <-
  phylo.d(
    data = as.data.frame(trait), 
    phy = tree_nodes_null, 
  names.col = spp, 
  binvar = none
  )

# phylogenetic signal: diet breadth --------------------------------------------

diet_D <- phylo.d(
  data = as.data.frame(trait), 
  phy = tree_nodes_null, 
  names.col = spp, 
  binvar = diet
  )

# phylogenetic signal: voltinism -----------------------------------------------

volt_D <- phylo.d(
  data      = as.data.frame(trait), 
  phy       = tree_nodes_null, 
  names.col = spp, 
  binvar    = volt
  )

# phylogenetic signal: emergence time ------------------------------------------

e <- 
  phylo.d(
    data      = as.data.frame(trait), 
    phy       = tree_nodes_null, 
    names.col = spp,
    binvar    = emer_time2
  )

# table: phylo signals for binary traits ---------------------------------------

# summary of phylogenetic signal tests for binary bee traits

bin_table <- tribble(
  
  ~trait,       # trait name
  ~est_D,       # estimated D statistic
  ~N_state_0,   # number of character states 0
  ~N_state_1,   # number of character states 1 
  ~p_random,    # 
  ~p_BM,        #
  
  # diet
  diet_D$binvar, 
  round(as.numeric(diet_D$DEstimate), digits = 2), 
  as.numeric(diet_D$StatesTable[1]), 
  as.numeric(diet_D$StatesTable[2]),
  as.numeric(diet_D$Pval1),
  as.numeric(diet_D$Pval0),
  
  # native
  native_D$binvar, 
  round(as.numeric(native_D$DEstimate), digits = 2), 
  as.numeric(native_D$StatesTable[1]), 
  as.numeric(native_D$StatesTable[2]),
  as.numeric(native_D$Pval1),
  as.numeric(native_D$Pval0),
  
  # emergence_time
  e$binvar, 
  round(as.numeric(e$DEstimate), digits = 2), 
  as.numeric(e$StatesTable[1]), 
  as.numeric(e$StatesTable[2]),
  as.numeric(e$Pval1),
  as.numeric(e$Pval0),
  
  # voltinism
  volt_D$binvar, 
  round(as.numeric(volt_D$DEstimate), digits = 2), 
  as.numeric(volt_D$StatesTable[1]), 
  as.numeric(volt_D$StatesTable[2]),
  as.numeric(volt_D$Pval1),
  as.numeric(volt_D$Pval0),
  
  # leaf cut
  leafcut_D$binvar, 
  round(as.numeric(leafcut_D$DEstimate), digits = 2),
  as.numeric(leafcut_D$StatesTable[1]),
  as.numeric(leafcut_D$StatesTable[2]), 
  as.numeric(leafcut_D$Pval1),
  as.numeric(leafcut_D$Pval0),
  
  # leaf pulp
  leafpulp_D$binvar, 
  round(as.numeric(leafpulp_D$DEstimate), digits = 2), 
  as.numeric(leafpulp_D$StatesTable[1]),
  as.numeric(leafpulp_D$StatesTable[2]),
  as.numeric(leafpulp_D$Pval1),
  as.numeric(leafpulp_D$Pval0),
  
  # leaf hair
  leafhair_D$binvar, 
  round(as.numeric(leafhair_D$DEstimate), digits = 2), 
  as.numeric(leafhair_D$StatesTable[1]),
  as.numeric(leafhair_D$StatesTable[2]),
  as.numeric(leafhair_D$Pval1),
  as.numeric(leafhair_D$Pval0),
  
  # mud
  mud_D$binvar, 
  round(as.numeric(mud_D$DEstimate), digits = 2),
  as.numeric(mud_D$StatesTable[1]),
  as.numeric(mud_D$StatesTable[2]),
  as.numeric(mud_D$Pval1),
  as.numeric(mud_D$Pval0),
  
  # none
  none_D$binvar, 
  round(as.numeric(none_D$DEstimate), digits = 2),
  as.numeric(none_D$StatesTable[1]),
  as.numeric(none_D$StatesTable[2]),
  as.numeric(none_D$Pval1),
  as.numeric(none_D$Pval0),
  
  # resin
  resin_D$binvar, 
  round(as.numeric(resin_D$DEstimate), digits = 2), 
  as.numeric(resin_D$StatesTable[1]),
  as.numeric(resin_D$StatesTable[2]),
  as.numeric(resin_D$Pval1),
  as.numeric(resin_D$Pval0),) %>%
  
  arrange(est_D)

# save to disk -----------------------------------------------------------------

write.csv(x = bin_table, file = here('data/final', "phylo_signal_binary.csv"))
