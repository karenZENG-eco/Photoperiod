mypalette <- c("#0072B2","#E69F00")

photo_plot <- ggplot(data)+
  geom_point(aes(y=doy, year, colour = photoperiod_sensitive), shape = 19,alpha = 0.7)+ #points that are shape 1 (hollow circle) and jittered
  geom_smooth(aes(y=doy, year, group = photoperiod_sensitive), method = "glm", se=F, size= 1.5, colour = "white")+ #white outline
  geom_smooth(aes(y=doy, year, colour = photoperiod_sensitive), method = "glm", se=F, size = 1)+ #trendline
  xlab("Year")+
  xlim(0, 365)+
  ylab("Average First Flowering Day")+
  guides(color = guide_legend(title = "Photoperiod Sensitivity",reverse = TRUE))+
  scale_colour_manual(values=mypalette)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 15,
                                  face  = "bold"), plot.margin = margin(10, 10, 10, 10)
  )

photo_plot


