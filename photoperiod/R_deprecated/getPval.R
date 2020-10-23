#' This is code to get Effect of Photoperiod Sensitivity on First Flowering Date over time
#' For The Photoperiod Sensitivity Project
#' 
#' @param Tibble of Photoperiod data  
#' @return Results of analysis 
#' @authors Karen Zeng, Alex Sentinella ATSentinella.at.gmail.com
#' 

#' #Obtaining p-values for comparison between models using bootstrapping methods

getPval <- function(data){
  
  lm.1 <- lmer(doy ~ photoperiod_sensitive*year + (1|species), data = data)
  lm.2 <- lmer(doy ~ photoperiod_sensitive + year + (1|species), data = data)
  
  PBmodcomp(lm.1, lm.2, nsim = 5999, ref = NULL, cl = NULL, details = 0)
  
  output <- list()
  
  return(output)
  
}