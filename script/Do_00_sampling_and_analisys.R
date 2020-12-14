library(naturaList)
library(here)
library(tidyverse)
library(raster)
library(sp)
library(sf)
library(rnaturalearth)
library(vegan)
library(fasterize)
source(here::here("function", "niche_area_richness.R"))
source(here::here("function", "class_filter_samp_spec.R"))
source("https://raw.githubusercontent.com/avrodrigues/avr_functions/master/map_rich_occ.R")


# Load data ---------------------------------------------------------------


# species occurrence data
# occ.df
load(here("data", "occ_df.RData"))
occ.df <- filter(occ.df, year >= 1979)

# specialist files

spec.files <- grep("especialistas", 
                   list.files(here("data"), full.names = T), 
                   value = T)
l.spec.df <- lapply(spec.files, read.csv2)
names(l.spec.df) <- substr(spec.files, 99, nchar(spec.files)-4)

# enviromental data
temp <- raster(here("data", "env", "bio1_chelsa_0_5dd.tif"))
dry <- raster(here("data", "env", "bio17_chelsa_0_5dd_sqrt.tif"))

env.df <- na.omit(data.frame(temp = values(temp),
                              dry = values(dry)))

# standardized by range
env.stand <- decostand(env.df, "range")


# Define limits of eniromental and geographical space ---------------------

# define the possible eviromental space
env.point <-  st_multipoint(as.matrix(env.stand))

box <- c(0,1,0,1)
r <- raster(extent(box), resolution = 0.025)
r.cell <- unique(cellFromXY(r, env.stand))
xy.cell <- xyFromCell(r, r.cell)
ch.point <-  st_multipoint(as.matrix(xy.cell))
ch.buffer <- st_buffer(ch.point, 0.05)

box2 <- c(xmin = 0, ymin = 0, xmax = 1, ymax = 1)
env.space <- st_crop(st_geometry(ch.buffer), box2)
plot(env.space)

# defining geographic region
neo_coastline <- ne_coastline(scale = 50, returnclass = "sf")
ext <- extent(temp)[c(1:4)]
names(ext) <- c("xmin", "xmax", "ymin", "ymax")
neo_coastline <- st_crop(st_geometry(neo_coastline), ext)
neo_coastline <- st_cast(neo_coastline, "MULTIPOLYGON")

plot(temp)
plot(neo_coastline,  add = T)




# Classify and filter from complete lists of specialists ------------------

# All occurrence data
# No filtering
occ.test <- occ.df %>% 
  dplyr::select(decimalLongitude, decimalLatitude, species) 

all.occ <- niche_area_richness(occ.test,
                               neo_coastline,
                               env.space)
saveRDS(all.occ, here("output", "all_occ_classified.rds"))


# Specialist list: 1% most frequent strings
# Filter: only identified by specialists
occ.df.spec.1 <- classify_occ(occ.df,
                             l.spec.df$especialistas_1porcento,
                             spec.ambiguity = "not.spec")

occ.spec.1 <- occ.df.spec.1 %>% 
  filter(naturaList_levels == "1_det_by_spec") %>% 
  dplyr::select(decimalLongitude, decimalLatitude, species) 

spec.1.occ <- niche_area_richness(occ.spec.1,
                                  neo_coastline,
                                  env.space)

names.sp <- names(all.occ[[1]]) %in% names(spec.1.occ[[1]])
mtx.env.area <- matrix(0, length(all.occ[[1]]), 1)
mtx.geo.area <- matrix(0, length(all.occ[[1]]), 1)
mtx.env.area[names.sp, 1] <- spec.1.occ$env.area
mtx.geo.area[names.sp, 1] <- spec.1.occ$geo.area
spec.1.occ$env.area <- mtx.env.area
spec.1.occ$geo.area <- mtx.geo.area


saveRDS(spec.1.occ, here("output", "spec_1_occ_classified.rds"))

# Specialist list: 5% most frequent strings
# Filter: only identified by specialists
occ.df.spec.5 <- classify_occ(occ.df,
                             l.spec.df$especialistas_5porcento,
                             spec.ambiguity = "not.spec")

occ.spec.5 <- occ.df.spec.5 %>% 
  filter(naturaList_levels == "1_det_by_spec") %>% 
  dplyr::select(decimalLongitude, decimalLatitude, species) 

spec.5.occ <- niche_area_richness(occ.spec.5,
                                  neo_coastline,
                                  env.space)

names.sp <- names(all.occ[[1]]) %in% names(spec.5.occ[[1]])
mtx.env.area <- matrix(0, length(all.occ[[1]]), 1)
mtx.geo.area <- matrix(0, length(all.occ[[1]]), 1)
mtx.env.area[names.sp, 1] <- spec.5.occ$env.area
mtx.geo.area[names.sp, 1] <- spec.5.occ$geo.area
spec.5.occ$env.area <- mtx.env.area
spec.5.occ$geo.area <- mtx.geo.area

saveRDS(spec.5.occ, here("output", "spec_5_occ_classified.rds"))

# Specialist list: 10% most frequent strings
# Filter: only identified by specialists
occ.df.spec.10 <- classify_occ(occ.df,
                             l.spec.df$especialistas_10porcento,
                             spec.ambiguity = "not.spec")

occ.spec.10 <- occ.df.spec.10 %>% 
  filter(naturaList_levels == "1_det_by_spec") %>% 
  dplyr::select(decimalLongitude, decimalLatitude, species) 

spec.10.occ <- niche_area_richness(occ.spec.10,
                                  neo_coastline,
                                  env.space)

names.sp <- names(all.occ[[1]]) %in% names(spec.10.occ[[1]])
mtx.env.area <- matrix(0, length(all.occ[[1]]), 1)
mtx.geo.area <- matrix(0, length(all.occ[[1]]), 1)
mtx.env.area[names.sp, 1] <- spec.10.occ$env.area
mtx.geo.area[names.sp, 1] <- spec.10.occ$geo.area
spec.10.occ$env.area <- mtx.env.area
spec.10.occ$geo.area <- mtx.geo.area

saveRDS(spec.10.occ, here("output", "spec_10_occ_classified.rds"))

# Specialist list: provided by a specialist
# Filter: only identified by specialists
occ.df.spec.Myrt <- classify_occ(occ.df,
                             l.spec.df$especialistas_Myrtaceae,
                             spec.ambiguity = "not.spec")

occ.spec.Myrt <- occ.df.spec.Myrt %>% 
  filter(naturaList_levels == "1_det_by_spec") %>% 
  dplyr::select(decimalLongitude, decimalLatitude, species) 

spec.Myrt.occ <- niche_area_richness(occ.spec.Myrt,
                                   neo_coastline,
                                   env.space)

names.sp <- names(all.occ[[1]]) %in% names(spec.Myrt.occ[[1]])
mtx.env.area <- matrix(0, length(all.occ[[1]]), 1)
mtx.geo.area <- matrix(0, length(all.occ[[1]]), 1)
mtx.env.area[names.sp, 1] <- spec.Myrt.occ$env.area
mtx.geo.area[names.sp, 1] <- spec.Myrt.occ$geo.area
spec.Myrt.occ$env.area <- mtx.env.area
spec.Myrt.occ$geo.area <- mtx.geo.area

saveRDS(spec.Myrt.occ, here("output", "spec_Myrt_occ_classified.rds"))

# Specialist list: jointed the lists of 10% most freq. and provided by a specialist
# Filter: only identified by specialists
occ.df.spec.all <- classify_occ(occ.df,
                             l.spec.df$especialistas_todos,
                             spec.ambiguity = "not.spec")

occ.spec.all <- occ.df.spec.all %>% 
  filter(naturaList_levels == "1_det_by_spec") %>% 
  dplyr::select(decimalLongitude, decimalLatitude, species) 

spec.all.occ <- niche_area_richness(occ.spec.all,
                                     neo_coastline,
                                     env.space)

names.sp <- names(all.occ[[1]]) %in% names(spec.all.occ[[1]])
mtx.env.area <- matrix(0, length(all.occ[[1]]), 1)
mtx.geo.area <- matrix(0, length(all.occ[[1]]), 1)
mtx.env.area[names.sp, 1] <- spec.all.occ$env.area
mtx.geo.area[names.sp, 1] <- spec.all.occ$geo.area
spec.all.occ$env.area <- mtx.env.area
spec.all.occ$geo.area <- mtx.geo.area

saveRDS(spec.all.occ, here("output", "spec_all_occ_classified.rds"))

# Sampling the list of specialists and filter occurrence data -------------

# each list of specialists
for(k in 1:length(l.spec.df)){
  
  idx <- 1
  list.env.area.std <- vector("list", 5)
  
  # each proportional sampling of a list of specialist
  for(prop in c(0.1,0.3,0.5,0.7,0.9)){
    
    # number of repetitions
    n.run <- 100
    mtx.env.area <- matrix(0, length(all.occ[[1]]), n.run)
    mtx.geo.area <- matrix(0, length(all.occ[[1]]), n.run)
    mtx.pol.richness <- matrix(0, length(all.occ[[3]]), n.run)
    mtx.pt.richness <- matrix(0, length(all.occ[[4]]), n.run)
    
    # classification, filter only idetified by specialists
    # calcule geographical and environmental range and
    # richness based only on points and polygons
    pb <- txtProgressBar(min = 0, max = n.run, style = 3)
    for(i in 1:n.run){
      occ.run <- class_filter_samp_spec(occ.df = occ.df, 
                                        spec.list = l.spec.df[[k]],
                                        sample.spec = prop)
      
      occ.run.ft <- occ.run %>% 
        dplyr::select(decimalLongitude, decimalLatitude, species)
      
      smp.occ <- niche_area_richness(occ.run.ft,
                                     geo.space = neo_coastline,
                                     env.space)
      
      run.sp <- names(all.occ[[1]]) %in% names(smp.occ[[1]])
      
      mtx.env.area[run.sp, i] <- smp.occ$env.area
      mtx.geo.area[run.sp, i] <- smp.occ$geo.area
      mtx.pol.richness[,i] <- smp.occ$pol.richness
      mtx.pt.richness[,i] <- smp.occ$pt.richness
      
      setTxtProgressBar(pb, i)
      
    }
    
    list.env.area.std[[idx]] <- list(mtx.env.area = mtx.env.area,
                                     mtx.geo.area = mtx.geo.area,
                                     mtx.pol.richness = mtx.pol.richness,
                                     mtx.pt.richness = mtx.pt.richness)
    cat(paste("prop", idx), fill = T)
    idx <- idx + 1
  }
  save(list.env.area.std, file = here("output", 
                                      paste0("sample_",
                                             names(l.spec.df)[k],
                                             ".RData")))
  cat(paste(k, "finalizado"))
}

