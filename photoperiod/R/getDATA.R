#load in required libraries
library(tidyverse)

#read in single-source data

mones2001 <- read.csv("./Data/raw/Mones2001/mones_2001_doy.csv")
fitter2002 <- read.csv("./Data/raw/Fitter2002/fitter_2002.csv")
craine2012 <- read.csv("./Data/raw/Craine2012/craine_2012.csv")
rmbl <- read.csv("./Data/raw/RMBL phenology/RMBL_Phenology_1974-2017_Zeng.csv")

#gather by year for mones2001 and fitter2002

mones2001 <- gather(mones2001, "year", "n", 2:30)
fitter2002 <- gather(fitter2002, "year", "n", 2:48)

#Rename and reorder columns, add source and location if missing



