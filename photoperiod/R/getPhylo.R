#  Phylogentic Analysis
#  - Combine the photoperiod sensitivity data with the phylogenetic data using used the ‘phylosignal’ package
#  - pruning the phylogenetic tree published by Zanne et al. (2014) using the ‘ape’ package
#  - Phylogenetic signal and the significance of the signal was calculated using ‘phylo.d’ from the ‘caper’ package to:
#    - find Frits and Purvis’ D 

## Resources consulted:
##  https://rgriff23.github.io/2017/05/11/primate-phylogeny-ggtree.html
## 
## https://www.bioconductor.org/packages/3.7/bioc/vignettes/ggtree/inst/doc/treeAnnotation.html#plot-tree-with-associated-data
## 
## https://www.stat.berkeley.edu/~nolan/stat133/Fall05/lectures/DataTypes4.pdf
## 
## https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4799788/


getPhylo <- function(phylo_data){

# Load packages
  library(ape)
  library(ggtree)
  library(phytools)
  library(phylobase) # NOTE: What did i use phylobase for again?
  library(phylosignal)
  library(caper)
  library(ggplot2)
  library(tidyverse)

# Read tree
  tree.raw <- read.tree("./Data/ALLMB.tre")

# Make species list 
  species.list <- pp$species

# Prune tree by dropping all tips (species) that are not on our species list
   tree <- drop.tip(tree.raw, tree.raw$tip.label[-na.omit(match(species.list, tree.raw$tip.label))])
   
#Sort the species data into the same order as the tree
   species.order <- tree$tip.label
  data_sorted <- data.frame(Reduce(rbind, species.order))
  names(data_sorted)[1] <- "species"
  data_sorted <- left_join(data_sorted, phylo_data)

  # Fixing up the data: 
  # There is a known error with clade names where un-named nodes are treated as having duplicated names
  # The fix is to numerically name each node so it has a unique name
  # For more info: http://grokbase.com/t/r/r-sig-phylo/11ce1b8stn/doubts-and-problems-bug-when-using-comparative-data-and-pgls-functions-in-caper%E2%80%8F
  
  tree <- makeLabel(tree)   
  
# Visualising the combined data

  # Visualise the tree
  #    http://www.phytools.org/eqg/Exercise_3.2/
  #  Tree with photoperiod and phylogeny*
  
  
  tree_plot <- ggtree(tree, layout = "circular")
#  tree_plot
  
  tree_plot2 <- tree_plot %<+% data_sorted +
                  geom_segment2(aes(subset = isTip, 
                                    yend = y, 
                                    xend = x + 20, 
                                    colour = sensitivity_yn, 
                                    size = 1.0)) +
                  geom_tiplab2(size = 3, offset = 20) +
                  scale_color_manual(values = c("#d9361a", "#063951"))
  
  tree_plot2
  
#  ggsave(tree_plot2, "fig/tree_plot2.png", width = 30, height = 30)

# We can also plot tree2 it in a circle to look cool, making sure to use geom_tiplab2 so the labels go around the circle
#  tree_plot2 <- ggtree(tree, layout = "circular") 
  
#  tree_plot2
  
# Again, save to a painfully large png so we can actually read the species
#  ggsave("fig/tree2_plot2.png", width = 20, height = 20)

# Calculating the phylogenetic signal
# Combine photoperiod data with tree in the phylo4d format
  tree4d <- phylo4d(tree, (data_sorted$sensitivity_yn))

  signalresults <- phyloSignal(tree4d)
  
#  signalresults

# The first table is the signal values and the second table is the significance of each signal.
# ponits.col is generated to colour the points red if significant, black if not

  local.i <- lipaMoran(tree4d, prox.phylo = "nNodes", as.p4d = TRUE)
  points.col <- lipaMoran(tree4d, prox.phylo ="nNodes")$p.value #EXTRACT PVAL
  points.col <- ifelse(points.col < 0.05, "red", "black") #SIGNIFicant p val limit is 0.05
  phylosig.dotplot <- dotplot.phylo4d(local.i, dot.col = points.col)


# can't figure out how to save this nvm but good to know as a basic visualisation
  
# Binary phylogenetic signal with the caper package

# photo_compdat <- comparative.data(tree, phylo_data, species)
  
 
photo_phylod <- phylo.d(data_sorted, tree, names.col = species, binvar = sensitivity_yn)
  
#print(photo_phylod)

  output <- list(signalresults = signalresults,
                 photo_phylod = photo_phylod,
                 tree = tree,
                 tree4d = tree4d,
                 tree_plot = tree_plot,
                 tree_plot2 = tree_plot2
                 )
  
  return(output)
  
}
  
  
  
  