#' This is code to get Effect of Photoperiod Sensitivity on First Flowering Date over time
#' For The Photoperiod Sensitivity Project
#' 
#' @param Tibble of Photoperiod data  
#' @return Results of analysis 
#' @authors Karen Zeng, Alex Sentinella ATSentinella.at.gmail.com

  

#Effect of Photoperiod Sensitivity on First Flowering Date over time
#- mixed effect generalised linear model
#- photoperiod sensitive vs photoperiod insensitive
#- lmer function in the lme4 package
#- obtained P-values using the lmerTest package
#- calculated the rate of change per decade by multiplying the slope of each line (the annual rate of change) by 10
#- We also fitted linear models to the combined dataset including both photoperiod sensitive and insensitive species 
#to identify whether it made a significant difference over time (Appendix C) and to each species, extracting the 
#gradient of the line to find their individual rates of advancement.


getPhotoFFD <- function(data){

require(lme4)
require(lmerTest)

#lm.1 will be a model comparing flowering time (doy) with photoperiod sensitivity (sensitivity), year 
#AND the interaction between sensitivity and year
#Once again, species is included as random effects
lm.1 <- lmer(doy ~ photoperiod_sensitive*year + (1|species), data = data)

#Check the assumptions
residuals.1 <- ggplot(lm.1, aes(.fitted, .resid)) +
  geom_point(alpha = 0.5, shape = 1) + 
  theme_classic()

qq.1 <- qqnorm(resid(lm.1))
  qqline(resid(lm.1))

aic.1 <- AIC(lm.1)


#graph the datapoints, comparing photoperiod sensitivite and not photoperiod sensitive
write.csv(data, "./outputs/graphdata.csv")


photo_plot <- 
  ggplot(data)+
  geom_point(aes(y=doy, year, colour = photoperiod_sensitive), size = 2, alpha = 0.4)+ #points that are shape 1 (hollow circle) and jittered
  # geom_smooth(aes(y=doy, year, group = photoperiod_sensitive), method = "lm", se=F, size= 1.5, colour = "white")+ #white outline
  geom_smooth(aes(y=doy, year, colour = photoperiod_sensitive), method = "lm", se=F, size = 2)+ #trendline
  xlab("Year")+
  ylim(0, 400)+
  xlim(1950, 2017)+
  ylab("Average First Flowering Day")+
  guides(color = guide_legend(title = "Photoperiod Sensitivity", reverse = TRUE))+
  scale_colour_manual(values=mypalette)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 15, face  = "bold"),
        legend.position = "none")+
  geom_label_repel(x = 2017, y = 110, label = "Insensitive", colour = "#0072B2")+
  geom_label_repel(x = 2017, y = 130, label = "Sensitive", colour = "#E69F00")

source_plot <- ggplot(data)+
  geom_point(aes(y=doy, year, colour = source, alpha = 0.5))+
  scale_shape_manual(values = 1)+
  geom_smooth(aes(y=doy, year, colour = source), method = "glm")+
  xlab("Year")+
  ylab("Average First Flowering Day")

# A polished species plot with photoperiod as colour
#ggplot(data)+
#geom_smooth(aes(y=doy, year, group = species, colour = photoperiod_sensitive), method = "glm", se = FALSE)+
#  xlab("Year")+
#  ylab("Average First Flowering Day")+
#  scale_colour_manual(values= c("#0072B2","#E69F00","#000000"))+ labs(colour = "Photoperiod Sensitive")+ theme_bw()

#
#Part 2: WIP
#

#separate data into photoperiod sensitive and insensitive samples
  
data.t <- filter(data,  photoperiod_sensitive == TRUE)
data.f <- filter(data, photoperiod_sensitive == FALSE)

#make a linear model for each group

lm.t <- lmer(doy ~ year + (1|species), data = data.t, REML = F)
lm.f <- lmer(doy ~ year + (1|species), data = data.f, REML = F)

#Check assumptions: residuals, qq, fit

residuals.t <- ggplot(lm.t, aes(.fitted, .resid)) +
  geom_point(alpha = 0.5, shape = 1) + 
  theme_classic()
residuals.f <- ggplot(lm.f, aes(.fitted, .resid)) +
  geom_point(alpha = 0.5, shape = 1) + 
  theme_classic()

#qq.t <- qqnorm(resid(lm.t))
#qqline(resid(lm.t))

#qq.f <- qqnorm(resid(lm.f))
#qqline(resid(lm.f))

aic.t <- AIC(lm.t)
aic.f <- AIC(lm.f)

#Comparing the two groups

#Because we have more species that are sensitive to photoperiod than not, 
#we will take a random sample of them to be able to properly compare the two
#Check if this is ok statswise

#data.t.sampled <- sample_n(data.t, size = nrow(data.f)) #sample data.t so it has same samplesize as data.f for the ANOVA

#lm.t.sampled <- lmer(doy ~ year + (1|species), data = data.t.sampled, REML = F)

#anova(lm.f, lm.t.sampled)

#Error here, iduno what uhhh
#summary(lm.t)
#summary(lm.f)

##Issue that old records and recent records are more 'extreme' and have higher residuals
##qqplot(resid(lm.1))

output <- list(lm_interaction = lm.1,
               residuals_interaction = residuals.1,
               qq_interaction = qq.1,
               aic_interaction = aic.1,
               lm_t = lm.t,
               lm_f = lm.f,
               photo_plot = photo_plot,
               species_plot = species_plot,
               speciesxphoto_plot = speciesxphoto_plot,
               source_plot = source_plot)

  return(output)


}