
#' This is code to add Life History and Growth Form Data to the master dataset
#' For The Photoperiod Sensitivity Project
#' Life history data (annual/perennial) from the TRY plant database  
#' Growth form data (woody/herbaceous) was adapted from Zanne et al. (2009). 
#' 
#' @param Tibble of Photoperiod data  
#' @return Data tibble with Growth Form and Life History added to it
#' @authors Karen Zeng, Alex Sentinella ATSentinella.at.gmail.com



#- Life history data (annual/perennial) from the TRY plant database  
#- Growth form data (woody/herbaceous) was adapted from Zanne et al. (2009). 
#- Species that could not be readily classified in to these groups were excluded from the relevant analyses


#takes tibble, and adds columns for life history and growth form


addLHGF <- function(traits_data) {
  
  
#Read in woodiness data
wood <- read.csv("./Data/GlobalWoodinessDatabase.csv")

  
#Remove the extra columns and change space to underscore
#Note to Alex: the 2 woodiness datasets were the same dataset from 2 different people, i've used just one
wood <- transmute(wood, species = gs, woodiness = woodiness)
  
wood$species <- gsub(" ", "_", wood$species)

#join to main data table
traits_data <- left_join(traits_data, wood, by = "species")

#Pastinaca sativa, Scrophularia vernalis and Hibiscus moscheutos are all herbaceous.
traits_data$woodiness[traits_data$species == "Pastinaca_sativa"] <- "H"
traits_data$woodiness[traits_data$species == "Scrophularia_vernalis"] <- "H"
traits_data$woodiness[traits_data$species == "Hibiscus_moscheutos"] <- "H"

#There should be 370 species with woodiness data


#Read in longevity data

longevity <- read.csv(file = "./Data/TRYlongevity.csv")

#Identify if annual or perennial through a very long list of qualifiers
#NOTE: the life history section doesn't seem to work yet with the main table, all longevity is NA when added to main dataframe

longevity <- longevity %>% 
  mutate(ann_per = case_when(OrigValueStr == "annual" | 
                               OrigValueStr == "Annual" | 
                               OrigValueStr == "annuals" |
                               OrigValueStr == "summer annuals" |
                               OrigValueStr == "always annual" |
                               OrigValueStr == "winter annuals" |
                               OrigValueStr == "annual-winter annual" |
                               OrigValueStr == "winter annual" |
                               (OriglName == "Life history" & OrigValueStr == "1" ) |
                               (OriglName == "Plant phenology: Annual" & OrigValueStr == "yes" )     ~   "annual",  ######annuals
                             OrigValueStr == "perennial" | 
                               OrigValueStr == "Perennial" | 
                               OrigValueStr == "perennials" |                                   
                               OrigValueStr == "always pluriennial-pollakanthic" | 
                               (OriglName == "Plant phenology: Biennial" & OrigValueStr == "yes" ) | 
                               OrigValueStr == "perennial < 20 years" | 
                               OrigValueStr == "woody" | 
                               OrigValueStr == "perennial/woody" | 
                               OrigValueStr == "perennial > 20 years" | 
                               OrigValueStr == "poly-annuals > 50 years (long-lived perennials)" | 
                               OrigValueStr == "always biennial, always pluriennial-hapaxanthic" | 
                               OrigValueStr == "always biennial, always pluriennial-pollakanthic" | 
                               OrigValueStr == "tree" | 
                               OrigValueStr == "shrub" | 
                               OrigValueStr == "always pluriennial-hapaxanthic, always pluriennial-pollakanthic" | 
                               OrigValueStr == "always pluriennial-hapaxanthic" | 
                               OrigValueStr == "biennial" | 
                               OrigValueStr == "annual/biennial" | 
                               OrigValueStr == "poly-annuals < 5 years (short-lived perennials)" | 
                               OrigValueStr == "Biennial" | 
                               OrigValueStr == "biennial/perennial" | 
                               OrigValueStr == "always biennial" | 
                               OrigValueStr == "biennial-perennial" | 
                               OrigValueStr == "sometimes biennial, always pluriennial-hapaxanthic, sometimes pluriennial-pollakanthic" | 
                               OrigValueStr == "sometimes biennial, sometimes pluriennial-hapaxanthic, always pluriennial-pollakanthic" | 
                               OrigValueStr == "biennial/perennial/woody" | 
                               OrigValueStr == "sometimes biennial, always pluriennial-pollakanthic" | 
                               OrigValueStr == "poly-annuals 5-50 years (medium-lived perennials)" | 
                               (OriglName == "Plant phenology: Perennial" & OrigValueStr == "yes" )| 
                               (OriglName == "Plant phenology: Annual" & OrigValueStr == "no" )    ~   "perennial"  ###perennials
  ))

longevity$species <- gsub(" ", "_", longevity$AccSpeciesName)

#Remove duplicates in the database by taking the first row per species

longevity <- longevity %>%
  group_by(AccSpeciesName) %>% 
  filter(row_number()==1)
  
#remove the parts you won't use


longevity <- transmute(longevity, species = species, life = ann_per)

#Note: keep the longevity data in workspace because it takes a while to remake if you break it

#add the assumption that woody species are perennial

longevity <- left_join(longevity, wood, by = "species")


longevity <- longevity %>%
  mutate(life=replace(life, woodiness=="W", "perennial"))

#remove AccSpeciesName
longevity <-as_tibble(longevity[2:3])
  

#join to main data table
traits_data <- left_join(traits_data, longevity, by = "species")

#Analyse life History
life_history <- traits_data %>% #Tally life histories
  filter(!is.na(life))%>%
  group_by(photoperiod_sensitive, life) %>%
  tally()


life_history_chi <- chisq.test(life_history$n, p = c(0.25, 0.25, 0.25, 0.25)) #Perform chi-square test

life_history_chi
life_history_chi$residuals

#Perennial Pie

life_history_pie_perennial <- life_history %>% #choose our dataframe
  filter(life == "perennial") %>% #Filter so we are only looking at this type of plant
  ggplot(aes(x = "", y = n, fill = photoperiod_sensitive)) + #single graph split by proportion of photoperiod sensitivity
  ggtitle("Perennial")+ #add title
  geom_bar(width = 1, stat = "identity") + #make it a bar graph
  labs(fill = "Photoperiod Sensitive") + #change key label
  scale_fill_manual(values= mypalette) + #use our own colours
  theme_minimal() + #removes most of the background fluff
  theme( #removes the rest of the background fluff
    axis.title.x = element_blank(),
    axis.text.x=element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank()
    ) +
  coord_polar("y", start=0) #turns bar chart into a pie, remove this to more easily see what parts of the graph are

#Annual Pie

life_history_pie_annual <- life_history %>% #choose our dataframe
  filter(life == "annual") %>% #Filter so we are only looking at this type of plant
  ggplot(aes(x = "", y = n, fill = photoperiod_sensitive)) + #single graph split by proportion of photoperiod sensitivity
  ggtitle("Annual")+ #add title
  geom_bar(width = 1, stat = "identity") + #make it a bar graph
  labs(fill = "Photoperiod Sensitive") + #change key label
  scale_fill_manual(values= mypalette) + #use our own colours
  theme_minimal() + #removes most of the background fluff
  theme( #removes the rest of the background fluff
    axis.title.x = element_blank(),
    axis.text.x=element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank()
  ) +
  coord_polar("y", start=0) #turns bar chart into a pie, remove this to more easily see what parts of the graph are

#Analyse Growth Form
woodiness <- traits_data %>%
  filter(!is.na(woodiness), woodiness != "variable")%>% #remove NAs and variable woodiness
  group_by(photoperiod_sensitive, woodiness) %>%
  tally()


woodiness_chi <- chisq.test(woodiness$n, p = c(0.25, 0.25, 0.25, 0.25))

woodiness_chi
woodiness_chi$residuals

#Woody Pie
woodiness_pie_woody <- woodiness %>% #choose our dataframe
  filter(woodiness == "W") %>% #Filter so we are only looking at this type of plant
  ggplot(aes(x = "", y = n, fill = photoperiod_sensitive)) + #single graph split by proportion of photoperiod sensitivity
  ggtitle("Woody")+ #add title
  geom_bar(width = 1, stat = "identity") + #make it a bar graph
  labs(fill = "Photoperiod Sensitive") + #change key label
  scale_fill_manual(values= mypalette) + #use our own colours
  theme_minimal() + #removes most of the background fluff
  theme( #removes the rest of the background fluff
    axis.title.x = element_blank(),
    axis.text.x=element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank()
  ) +
  coord_polar("y", start=0) #turns bar chart into a pie, remove this to more easily see what parts of the graph are

#Herbaceous Pie
woodiness_pie_herbaceous <- woodiness %>% #choose our dataframe
  filter(woodiness == "H") %>% #Filter so we are only looking at this type of plant
  ggplot(aes(x = "", y = n, fill = photoperiod_sensitive)) + #single graph split by proportion of photoperiod sensitivity
  ggtitle("Herbaceous")+ #add title
  geom_bar(width = 1, stat = "identity") + #make it a bar graph
  labs(fill = "Photoperiod Sensitive") + #change key label
  scale_fill_manual(values= mypalette) + #use our own colours
  theme_minimal() + #removes most of the background fluff
  theme( #removes the rest of the background fluff
    axis.title.x = element_blank(),
    axis.text.x=element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank()
  ) +
  coord_polar("y", start=0) #turns bar chart into a pie, remove this to more easily see what parts of the graph are


output <- list(traits_data = as.tibble(traits_data),
               life_history = life_history,
               life_history_chi = life_history_chi,
               life_history_pie_perennial = life_history_pie_perennial,
               life_history_pie_annual = life_history_pie_annual,
               growth_form = woodiness,
               growth_form_chi = woodiness_chi,
               growth_form_pie_woody = woodiness_pie_woody, 
               growth_form_pie_herbaceous = woodiness_pie_herbaceous
               )

return(output)
}