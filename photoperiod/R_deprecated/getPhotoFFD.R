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
#- compared AIC/BIC using pbkrtest in pbmodcomp to model comparison


getPhotoFFD <- function(data){

require(lme4)
require(lmerTest)
require(pbkrtest)

#lm.1 will be a model comparing flowering time (doy) with photoperiod sensitivity (sensitivity) AND the interaction between sensitivity and year WITH species as a random effect
lm.1 <- lmer(doy ~ photoperiod_sensitive*year + (1|species), data = data)

#Check the assumptions
residuals.1 <- ggplot(lm.1, aes(.fitted, .resid)) +
  geom_point(alpha = 0.5, shape = 1) + 
  theme_classic()

qq.1 <- qqnorm(resid(lm.1))
  qqline(resid(lm.1))

aic.1 <- AIC(lm.1)


#Graph the linear model
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

#separate data into photoperiod sensitive and insensitive samples
  
data.t <- filter(data,  photoperiod_sensitive == TRUE)
data.f <- filter(data, photoperiod_sensitive == FALSE)

#make a line model for each group
#used to draw the lines in resulting figures

lm.t <- lmer(doy ~ year + (1|species), data = data.t, REML = F)
lm.f <- lmer(doy ~ year + (1|species), data = data.f, REML = F)

#Check assumptions: residuals, qq, fit

residuals.t <- ggplot(lm.t, aes(.fitted, .resid)) +
  geom_point(alpha = 0.5, shape = 1) + 
  theme_classic()
residuals.f <- ggplot(lm.f, aes(.fitted, .resid)) +
  geom_point(alpha = 0.5, shape = 1) + 
  theme_classic()

qq.t <- qqnorm(resid(lm.t))
#qqline(resid(lm.t))

qq.f <- qqnorm(resid(lm.f))
#qqline(resid(lm.f))

aic.t <- AIC(lm.t)
aic.f <- AIC(lm.f)

#Comparing the two groups

#lm.2: a comparison model that doesn't include the photoperion:year interaction

lm.2 <- lmer(doy ~ photoperiod_sensitive+year + (1|species), data = data)

anova <- anova(lm.1, lm.2)

#Now we simulate a load of (5999 simulations, i multiplied the recommended 599 by 10 as the number was recommended in 2010 when tech was older)
#Be warned that this part usually takes a long time

bootstrap <- PBmodcomp(lm.1, lm.2, nsim = 5999)



output <- list(lm_interaction = lm.1,
               residuals_interaction = residuals.1,
               qq_interaction = qq.1,
               aic_interaction = aic.1,
               lm_t = lm.t,
               lm_f = lm.f,
               photo_plot = photo_plot,
               species_plot = species_plot,
               speciesxphoto_plot = speciesxphoto_plot,
               source_plot = source_plot,
               anova = anova,
               bootstrap = bootstrap)

  return(output)


}