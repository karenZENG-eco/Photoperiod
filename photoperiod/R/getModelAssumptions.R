#' This is code to get Effect of Photoperiod Sensitivity on First Flowering Date over time
#' For The Photoperiod Sensitivity Project
#' 
#' @param Tibble of Photoperiod data  
#' @return Results of analysis 
#' @authors Karen Zeng, Alex Sentinella ATSentinella.at.gmail.com
#' 

#' #Additional analyses to do with separating species
#- lm for each species
#- lm using base R to obtain slope
#- magrittr pipes to repeat lm
#- Note that this is not an ideal analysis, each species irl would have different geographically isolated populations 
#- I am concerned that the different block of observation time of different populations affect results, especially in the rmbl alpine group
#- Further analysis could be done that integrates gps coordinates to approximate population?

getModelAssumptions <- function(lm){
  
require(lme4)
require(ggplot2)
  
residual  <- ggplot(lm, aes(.fitted, .resid)) +
  geom_point(alpha = 0.5, shape = 1) + 
  theme_classic() #Homogeneity of residuals variance. The residuals are assumed to have a constant variance (homoscedasticity)

qq <- qqnorm(resid(lm))#Normality of residuals. The residual errors are assumed to be normally distributed.
  
  #Linearity of the data. The relationship between the predictor (x) and the outcome (y) is assumed to be linear.
      #Independence of residuals error terms.
  
  return(list(
    residual = residual,
    qq = qq
  ))
  
}