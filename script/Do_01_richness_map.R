# Richness Map
library(naturaList)
library(here)
library(raster)
source("https://raw.githubusercontent.com/avrodrigues/avr_functions/master/map_rich_occ.R")


myrt.data <- read.csv(here::here("data", "myrteae_to_naturaList.csv"))
myrt.data.CC <- read.csv(here::here("data","myrteae_to_naturaList_CoordClean.csv"))
myrt.spec <- read.csv2(here::here("data","especialistas_Myrtaceae.csv"))


myrt.cl.occ.VGS <- classify_occ(myrt.data.CC, 
                            myrt.spec,
                            spec.ambiguity = "is.spec") # not.spec or manual.check

myrt.cl.occ.all <- classify_occ(myrt.data.CC, 
                                myrt.spec.all,
                                spec.ambiguity = "is.spec") # not.spec or manual.check





dim(myrt.cl.occ[!is.na(myrt.cl.occ$year),])

length(unique(myrt.cl.occ[!is.na(myrt.cl.occ$year), "species"]))

myrt.cl.occ[!is.na(myrt.cl.occ$year),] %>% 
  group_by(species) %>% 
  summarise(n = n()) %>% 
  filter(n >= 3)
              

r.rich <- map_rich_occ(myrt.data.CC[!is.na(myrt.data.CC$year),], 
             resolution = 0.5,
             species = "species",
             latitude = "decimalLatitude",
             longitude = "decimalLongitude",
             color.palette = c("yellow", "orange", "red", "darkred"),
             raster = T,
             xlim = c(-120,-30),
             ylim = c(-60,35)
             )                            
