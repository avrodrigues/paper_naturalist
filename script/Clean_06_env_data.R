library(raster)
library(sp)
library(sf)
library(here)
library(dismo)
library(ggplot2)
library(tmap)
# defining neotropical region extent
NEO <- list(x = c(-120,-30),
            y = c(-60,35))

l.files <- list.files("../Doutorado/data/PaleoClim/A_CHELSA_cur_0ka_V1_2B_r10m", 
                      full.names = T)

bioclim.files <- grep(".tif$", l.files, value = T)

r.bioclim <- stack(bioclim.files)
neo.bioclim <- crop(r.bioclim, NEO)

neo.bioclim.df <- na.omit(as.data.frame(neo.bioclim))

cor(neo.bioclim.df[, "bio_17"],
    neo.bioclim.df[, "bio_1"])

ggplot(neo.bioclim.df, aes(x = bio_1, y = sqrt(bio_17)))+
  geom_point(col = alpha("blue", 0.1)) +
  ylab("Dry Quarter") +
  xlab("Annual Mean Temperature")


temp.mean <- neo.bioclim$bio_1
dry.quart <- neo.bioclim$bio_17


temp.mean.1dd <- raster::aggregate(temp.mean, fact = 3)
dry.quart.1dd <- raster::aggregate(dry.quart, fact = 3)
dry.quart.1dd <- setValues(dry.quart.1dd, sqrt(values(dry.quart.1dd)))

temp_map <- tm_shape(temp.mean.1dd) + 
  tm_raster(col =  "bio_1", 
            alpha = 0.9,
            n = 15,
            title = "Temperature * 100", 
            palette = "-Spectral",
            midpoint = 180) +
  tm_layout(panel.show = T, 
            panel.labels = "Annual Mean Temperature",
            panel.label.fontface = 2,
            panel.label.bg.color = "grey90",
            legend.position = c("left", "bottom"))
  

dry_map <- tm_shape(dry.quart.1dd) + 
  tm_raster(col =  "bio_17", 
            alpha = 0.9,
            n = 15,
            title = "Square Root of Precipitation", 
            palette = "Spectral",
            midpoint = 10) +
  tm_layout(panel.show = T, 
            panel.labels = "Precipitation of the Driest Quarter",
            panel.label.fontface = 2,
            panel.label.bg.color = "grey90",
            legend.position = c("left", "bottom"))


windows(12,6)
tmap_arrange(temp_map, dry_map)

writeRaster(temp.mean.1dd, here("data", "env", "bio1_chelsa_0_5dd.tif"))
writeRaster(dry.quart.1dd, here("data", "env", "bio17_chelsa_0_5dd_sqrt.tif"))

