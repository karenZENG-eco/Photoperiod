

getPhyloCor <- function(data) {
  
  require(ape)
  require(dplyr)
  require(tidyr)
  
  data2 <- data %>%
           distinct(species, .keep_all = T)%>%
           filter(!is.na(photoperiod_sensitive))
  
  phyall <- read.tree(file = "./Data/ALLMB.tre") #full phylo tree for plants
  
  #Prune full tree for matches
  pruned_photo <- drop.tip(phyall, phyall$tip.label[na.omit(-match(data$species, phyall$tip.label))])
  
  photo_tree <- data %>%
    rename("pruned_photo$tip.label" = species) %>%
    right_join(as.data.frame(pruned_photo$tip.label), by = "pruned_photo$tip.label") %>%
    rename(species="pruned_photo$tip.label")
  
  
  nSpp <- nrow(photo_tree)
  
  MSBP.tree[, "phylo"] <- MSBP.Species
  
  tree <- compute.brlen(PrunedMSBP)
  
  Vphy <- vcv(tree, cor = T) #correlation matrix

  return(list(MSBP.tree = MSBP.tree, cor = Vphy))
  
}
