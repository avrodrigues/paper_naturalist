# Richness Map
library(naturaList)
library(here)
library(raster)
source("https://raw.githubusercontent.com/avrodrigues/avr_functions/master/map_rich_occ.R")


myrt.data <- read.csv(here("data", "myrteae_to_naturaList.csv"), fileEncoding = "latin1")
myrt.spec <- read.csv2(here("data","especialistas_Myrtaceae.csv"), fileEncoding = "latin1")

set.seed(1985)
smpl.data <- sample(1:nrow(myrt.data), 1000)

myrt.cl.occ <- classify_occ(myrt.data[smpl.data,], 
                            myrt.spec,
                            spec.ambiguity = "is.spec") # not.spec or manual.check

map_rich_occ(myrt.cl.occ, 
             resolution = 1.5,
             species = "species",
             latitude = "decimalLatitude",
             longitude = "decimalLongitude",
             color.palette = c("yellow", "orange", "red", "darkred")
             )                            
