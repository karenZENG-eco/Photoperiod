#' This is code to add Photoperiodism and Flowering Time to the master dataset
#' For The Photoperiod Sensitivity Project
#' 
#' @param Tibble of Photoperiod data  
#' @return Data tibble with Flowering Time added to it
#' @authors Karen Zeng, Alex Sentinella ATSentinella.at.gmail.com


#Load data
#- Photoperiod Sensitivity (pp)
#- Flowering Time (ft)
#- Species List (sl)
#
#Returns tibble with the following variables
#- species
#- avg_ffd: Average first flowering day (doy)
#- year
#- source (where the data came from)
#- photo: photoperiod sensitive (T/F)

#
#Issues:
#

joinFloweringData <- function(pp, ft){
  
  data <- pp %>% #make copy of photoperiod data
    mutate(species = as.character(species), cultivar = as.character(cultivar), notes = as.character(notes))#make sure data is the correct data type
  
  ft <- ft %>% 
    mutate(species = as.character(species)) #make sure data is the correct data type

  
  data <- left_join(ft, data, by = "species") %>% #Join photoperiod and flowering time data
          mutate(photoperiod_sensitive = sensitivity_yn,
                 avg_ffd = doy) %>%
          select(c("species", "doy", "year", "source", "photoperiod_sensitive")) %>%
          as_tibble()
  
  data <- distinct(data) %>% # remove duplicates and take only recent samples (1950+)
    filter(year>1949)
  
  data <-data %>% # Remove a few entreies with doy = 365 that are definitely supposed to be null
    filter(doy != 365)
  
  data <-data %>% # Remove species with not enough points of data
    group_by(species) %>%
    filter(n()>= 5)
  
  data$species <- as.factor(as.character(data$species))
  
  data <- na.omit(data)
  
  return(data)
  
  
}