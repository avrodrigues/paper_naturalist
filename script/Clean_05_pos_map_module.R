library(naturaList)
library(here)
library(tidyverse)
library(reshape2)
library(raster)
library(dismo)
library(maps)
library(sp)
library(sf)
library(rgdal)
library(rgeos)
source("https://raw.githubusercontent.com/avrodrigues/avr_functions/master/map_rich_occ.R")

america <- readOGR(here("data","Continentes"), "level1")[7:8,]
map.mod.output <- list.files(here("data", "map_module_output"), full.names = T)

list.occ <- lapply(map.mod.output, read.table, header = T)
# file.remove(map.mod.output[sapply(list.occ, nrow)==0])

occ.df <- do.call(rbind, list.occ)

save(occ.df, file = here("data", "occ_df.RData"))

map_rich_occ(occ.df[occ.df$year >= 1979,], 
             resolution = 1,
             species = "species",
             latitude = "decimalLatitude",
             longitude = "decimalLongitude",
             color.palette = viridisLite::viridis(5, alpha = 0.3),
             xlim = c(-120,-30),
             ylim = c(-55,35)
)     


map_rich_occ(occ.df[occ.df$year >= 1979 & 
                      occ.df$naturaList_levels == "1_det_by_spec",], 
             resolution = 1,
             species = "species",
             latitude = "decimalLatitude",
             longitude = "decimalLongitude",
             color.palette = viridisLite::viridis(5, alpha = 0.3),
             xlim = c(-120,-30),
             ylim = c(-55,35)
) 


levels.occ.df <- occ.df[occ.df$year >= 1979,] %>% 
  group_by(species) %>% 
  count(naturaList_levels) %>% 
  ungroup()

levels.melt <- melt(levels.occ.df)
sp_levels <- dcast(levels.melt, species ~ naturaList_levels)

sp_levels[is.na(sp_levels)] <- 0

sp_levels <- sp_levels[,order(names(sp_levels))]
sp_levels$total <- rowSums(sp_levels[,1:4])

sp_levels_prop <- sp_levels[,1:4]/sp_levels$total
sp_levels_prop$total <- sp_levels$total

ggplot(sp_levels_prop, 
       aes(`1_det_by_spec`)) +
  geom_histogram() 


ggplot(sp_levels_prop, 
       aes(x = total, y = `1_det_by_spec`)) +
  geom_point() 


