library(naturaList)
library(tidyverse)
library(raster)
library(sp)
library(sf)
library(rnaturalearth)
library(vegan)
library(fasterize)
library(here)

# Load data ---------------------------------------------------------------


# clasified occurrence data
# occ.cl
occ.cl.files <- list.files(here("output"), pattern = "occ_cl_", full.names = T)
occ.cl.list <- lapply(occ.cl.files, readRDS)
names(occ.cl.list) <- substr(occ.cl.files,101, nchar(occ.cl.files)-4)



# environmental data
env.files <- list.files(here("data", "env"), full.names = T)
env.stack <- stack(env.files)

# Define limits of environmental and geographical space ---------------------

# defining geographic region
neo_coastline <- ne_coastline(scale = 50, returnclass = "sf")
ext <- extent(env.stack)[c(1:4)]
names(ext) <- c("xmin", "xmax", "ymin", "ymax")
neo_coastline <- st_crop(st_geometry(neo_coastline), ext)
neo_coastline <- st_cast(neo_coastline, "MULTIPOLYGON")

# Classify, filter and compute metrics ------------------

# Specialist list: 1% most frequent strings
occ.cl.spec.1 <- occ.cl.list$occ_cl_spec_1

prop1 <- 
occ.cl.spec.1 %>% 
  group_by(naturaList_levels) %>% 
  summarise(prop_1 = n()/nrow(occ.cl.spec.1))


l.eval.spec.1 <- clean_eval(occ.cl.spec.1, 
                     neo_coastline, 
                     r = env.stack)

saveRDS(l.eval.spec.1, here("output", "spec_1_clean_eval_results.rds"))


# Specialist list: 5% most frequent strings
occ.cl.spec.5 <- occ.cl.list$occ_cl_spec_5

prop5 <- 
occ.cl.spec.5 %>% 
  group_by(naturaList_levels) %>% 
  summarise(prop_5 = n()/nrow(occ.cl.spec.5))


l.eval.spec.5 <- clean_eval(occ.cl.spec.5, 
                            neo_coastline,
                            r = env.stack)

saveRDS(l.eval.spec.5, here("output", "spec_5_clean_eval_results.rds"))


# Specialist list: 10% most frequent strings
occ.cl.spec.10 <- occ.cl.list$occ_cl_spec_10

prop10 <- 
occ.cl.spec.10 %>% 
  group_by(naturaList_levels) %>% 
  summarise(prop_10 = n()/nrow(occ.cl.spec.10))

l.eval.spec.10 <- clean_eval(occ.cl.spec.10, 
                            neo_coastline,
                            r = env.stack)

saveRDS(l.eval.spec.10, here("output", "spec_10_clean_eval_results.rds"))


# Specialist list: provided by a specialist in Myrtaceae
occ.cl.spec.myrt <- occ.cl.list$occ_cl_spec_myrt

propMyrt <- 
occ.cl.spec.myrt %>% 
  group_by(naturaList_levels) %>% 
  summarise(prop_myrt = n()/nrow(occ.cl.spec.myrt))

l.eval.spec.myrt <- clean_eval(occ.cl.spec.myrt, 
                             neo_coastline,
                             r = env.stack)

saveRDS(l.eval.spec.myrt, here("output", "spec_myrt_clean_eval_results.rds"))


# Specialist list: jointed the lists of 10% most freq. and provided by a specialist
occ.cl.spec.all <- occ.cl.list$occ_cl_spec_all

propAll <- 
occ.cl.spec.all %>% 
  group_by(naturaList_levels) %>% 
  summarise(prop_all = n()/nrow(occ.cl.spec.all))

l.eval.spec.all <- clean_eval(occ.cl.spec.all, 
                               neo_coastline,
                              r = env.stack)

saveRDS(l.eval.spec.all, here("output", "spec_all_clean_eval_results.rds"))



# summary of classification -------------------------------------------------


bind_cols(propMyrt, prop1[,2], prop5[,2], prop10[,2], propAll[,2])


