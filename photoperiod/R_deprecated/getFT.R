

getFT <- function(ft){
  craine <- read.csv("Data/raw_ft/craine_2012_FFD.csv")
  pep725 <- read.csv("Data/raw_ft/EU_PEP725_FFD.csv")
  fitter <- read.csv("Data/raw_ft/fitter_2002_FFD.csv")
  uknatc <- read.csv("Data/raw_ft/UK_Natures_Calender_FFD.csv")
  usrmbl <- read.csv("Data/raw_ft/US_Rocky_Mountains_Biology_Lab_FFD.csv")
  ususpn <- read.csv("Data/raw_ft/US_US_Phenology_Network_FFD.csv")
  

  fitter$X <- NULL #fix the bits that don't make sense
  uknatc$X <- NULL
  
  ft <- bind_rows(craine, pep725, fitter, uknatc, usrmbl, ususpn) #join tables
  
  ft$species <- gsub(" ", "_", ft$species)
  
  firstup <- function(x) {
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
  }
  
  ft$species <- firstup(ft$species)
  
  ft <- ft%>% #Take average doy for each species, year, source combination
    group_by(species, year, source)%>% 
    summarise(doy = mean(doy))
  
  
  return(ft)
  
  
}