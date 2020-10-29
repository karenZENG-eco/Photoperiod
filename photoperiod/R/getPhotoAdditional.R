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

getPhotoAdditional <- function(data){
  
data_Models <- data %>% #Make a model for each species
    group_by(species) %>%
    do(Model = lm( doy ~ year, data = .),
       photoperiod_sensitive = first(.$photoperiod_sensitive))
  
# Take gradient of each model (aka the change over time) as output
  data_Models_output <- data_Models %>%
    rowwise() %>%
    mutate(year.effect = coef(Model)[2])  %>% 
    select(-Model)

#See if there is a difference between ppsensitive and ppinsensitive species
  data_Models_output$photoperiod_sensitive <- sapply(data_Models_output$photoperiod_sensitive,unlist)
   data_models_ttest <- t.test(year.effect~photoperiod_sensitive, data_Models_output)
  
  
#Plot the output
  
data_Models_output$photoperiod_sensitive <- ifelse(data_Models_output$photoperiod_sensitive == "TRUE", "Photoperiod Sensitive", "Photoperiod Insensitive")
  
box_plot <- ggplot(data_Models_output, aes(x = photoperiod_sensitive, y = year.effect)) +
    geom_boxplot(width = 0.5, fill = mypalette) +
    xlab("Photoperiod Sensitivity") +
    ylab("Change in First Flowering (Days per Decade)") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 15,
                                  face  = "bold"), plot.margin = margin(10, 10, 10, 10)
  )

violin_plot <- ggplot(data_Models_output, aes(x = photoperiod_sensitive, y = year.effect)) +
  geom_violin(width = 0.5)  +
  scale_fill_manual(values = mypalette)+
  geom_boxplot(width=0.1, fill = mypalette)+
  xlab("Photoperiod Sensitivity") +
  ylab("Change in First Flowering (Days per Decade)") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10)
  )

output <- list(data_models = data_Models,
               data_models_output = data_Models_output,
               data_models_ttest = data_models_ttest,
               box_plot = box_plot,
               violin_plot = violin_plot)
  
return(output)

}

