# Richness Map
library(naturaList)
library(here)
library(raster)
library(sp)
library(letsR)
source("https://raw.githubusercontent.com/avrodrigues/avr_functions/master/map_rich_occ.R")
source(here::here("functions", "map_rich_occ_modf.R")) # modified version of plot richness function 


myrt.data <- read.csv(here::here("data", "myrteae_to_naturaList.csv"), fileEncoding = "latin1")
myrt.spec <- read.csv2(here::here("data","especialistas_Myrtaceae.csv"), fileEncoding = "latin1")

set.seed(1985)
smpl.data <- sample(1:nrow(myrt.data), 1000)

myrt.cl.occ <- classify_occ(myrt.data[smpl.data,], 
                            myrt.spec,
                            spec.ambiguity = "is.spec") # not.spec or manual.check

quartz()

layout.show(layout(matrix(c(1, 2, 3, 4), nrow= 2, ncol= 2, byrow = T)))
             
rich_all<- map_rich_occ_modif(myrt.cl.occ, 
                   resolution = 1.5,
                   species = "species",
                   latitude = "decimalLatitude",
                   longitude = "decimalLongitude",
                   color.palette = c("yellow", "orange", "red", "darkred"),
                   filter.by = NULL, main = "All criterions"
)  
rich_spec.taxon<- map_rich_occ_modif(x = myrt.cl.occ, 
                   resolution = 1.5, 
                   species = "species", 
                   latitude = "decimalLatitude",
                   longitude = "decimalLongitude", 
                   color.palette = c("yellow", "orange", "red", "darkred"),
                   filter.by = c("det_by_spec", "taxonomist"), main = "Spec/taxo")
rich_spec<- map_rich_occ_modif(x = myrt.cl.occ, 
                   resolution = 1.5, 
                   species = "species", 
                   latitude = "decimalLatitude",
                   longitude = "decimalLongitude", 
                   color.palette = c("yellow", "orange", "red", "darkred"),
                   filter.by = c("det_by_spec"), main = "Spec")
rich_taxon<- map_rich_occ_modif(x = myrt.cl.occ, 
                   resolution = 1.5, 
                   species = "species", 
                   latitude = "decimalLatitude",
                   longitude = "decimalLongitude", 
                   color.palette = c("yellow", "orange", "red", "darkred"),
                   filter.by = c("taxonomist"), main = "Taxono")
rich_dataframe<- cbind(rich_all, rich_spec, rich_spec.taxon, rich_taxon)

