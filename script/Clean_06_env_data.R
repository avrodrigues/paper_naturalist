library(raster)
library(sp)
library(sf)
library(stars)
library(here)
library(dismo)
library(ggplot2)
library(tmap)
library(stringr)


# Climatic layers ---------------------------------------------------------

# defining neotropical region extent
NEO <- list(x = c(-120,-30),
            y = c(-60,35))

# climatic layers
l.files <- list.files("../Doutorado/data/PaleoClim/A_CHELSA_cur_0ka_V1_2B_r10m", 
                      full.names = T)

bioclim.files <- grep(".tif$", l.files, value = T)

#load layers
r.bioclim <- stack(bioclim.files[c(1,4,7,9,14)])
# crop by neotropical region extent
neo.bioclim <- crop(r.bioclim, NEO)

# individual rasters
temp.mean <- neo.bioclim$bio_1
temp.sazo <- neo.bioclim$bio_4
prec.year <- neo.bioclim$bio_12
prec.sazo <- neo.bioclim$bio_15
dry.quart <- neo.bioclim$bio_17

# aggregating climatic layers by mean (default) in 0.5 decimal degrees
temp.mean.05dd <- raster::aggregate(temp.mean, fact = 3)
temp.sazo.05dd <- raster::aggregate(temp.sazo, fact = 3)
prec.year.05dd <- raster::aggregate(prec.year, fact = 3)
prec.sazo.05dd <- raster::aggregate(prec.sazo, fact = 3)
dry.quart.05dd <- raster::aggregate(dry.quart, fact = 3)

#save 
writeRaster(temp.mean.05dd, here("data", "env", "bio_1_chelsa_05dd.tif") , overwrite=TRUE)
writeRaster(temp.sazo.05dd, here("data", "env", "bio_4_chelsa_05dd.tif") , overwrite=TRUE)
writeRaster(prec.year.05dd, here("data", "env", "bio_12_chelsa_05dd.tif"), overwrite=TRUE)
writeRaster(prec.sazo.05dd, here("data", "env", "bio_15_chelsa_05dd.tif"), overwrite=TRUE)
writeRaster(dry.quart.05dd, here("data", "env", "bio_17_chelsa_05dd.tif"), overwrite=TRUE)

env.stack <- stack(temp.mean.05dd,
                   temp.sazo.05dd,
                   prec.year.05dd,
                   prec.sazo.05dd,
                   dry.quart.05dd)


# Altitude layers - merge and resamplig -----------------------------------

## altitude raster SRTM
srtm.files <-  list.files("../Doutorado/data/SRTM_Americas", 
                          full.names = T)
srtm.tif <- grep(".tif$", srtm.files, value = T)

# load - using stars pkg for speed
r.srtm <- lapply(srtm.tif,read_stars)

# joint SRTM rasters
alt <- st_mosaic(r.srtm[[1]],
                 r.srtm[[2]],
                 r.srtm[[3]],
                 r.srtm[[4]],
                 r.srtm[[5]],
                 r.srtm[[6]],
                 r.srtm[[7]],
                 r.srtm[[8]],
                 r.srtm[[9]],
                 r.srtm[[10]]
                 )

# grid size reference for resampling
grid.reference <- st_as_stars(env.stack[[1]])
bbox <- st_bbox(grid.reference)

# setting crs
st_crs(alt) <- st_crs(bbox)
alt.neo <- st_crop(alt, bbox)

# resampling using average method
alt.neo.05dd <- st_warp(alt.neo, 
                        grid.reference, 
                        use_gdal=TRUE, 
                        method = "average") # this method allows to work better 
                                            # with continent and island borders
                                            # bc it decrease the chance of a land
                                            # area has no raster value

## save 
stars::write_stars(adrop(alt.neo.05dd),
                  "../Doutorado/data/SRTM_Americas/Altitude_neotropico.tif",
                  driver = "GTiff")

stars::write_stars(adrop(alt.neo.05dd),
                   here("data", "env", "Altitude_neotropico.tif"),
                   driver = "GTiff")
