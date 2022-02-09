
# load packages -----------------------------------------------------------

library(here)
library(raster)
library(tidyverse)

# load data ---------------------------------------------------------------

# occurence data
occ.cl.files <- list.files(here("output"), pattern = "occ", full.names = T)

occ.cl.list <- lapply(occ.cl.files, readRDS)
names(occ.cl.list) <- substr(occ.cl.files,101, nchar(occ.cl.files)-4)

# raster layer
# environmental data
env.files <- list.files(here("data", "env"), full.names = T)
env.stack <- stack(env.files)


# how many occurences in each site of the cleaned dataset -----------------

occ.cl <- occ.cl.list$occ_cl_spec_all

xy.coords.level1 <- 
occ.cl %>% 
  filter(naturaList_levels == "1_det_by_spec") %>% 
  dplyr::select(decimalLongitude, decimalLatitude) %>% 
  as.matrix 
  
cell.n.ref <- 
  data.frame(cell = cellFromXY(env.stack[[1]], xy.coords.level1)) %>% 
  group_by(cell) %>% 
  summarise(n = n())


# sampling uncleaned dataset with same size of the cleaned datset --------

xy.coords.occ <- 
  occ.cl %>% 
  dplyr::select(decimalLongitude, decimalLatitude) %>% 
  as.matrix 

occ.cl.cell <- occ.cl %>% 
  mutate(cell = cellFromXY(env.stack[[1]], xy.coords.occ)) %>% 
  filter(cell %in% cell.n.ref$cell)

set.seed(06022022)
resampled.occ <- lapply(1:100, function(i){
  cat(paste0(i, " of 100"), fill = T)
  occ.cl.cell %>% 
    split(.$cell) %>% 
    map_dfr(function(x) {
      
      n.size <- cell.n.ref %>% filter(cell %in% x$cell) %>% 
        dplyr::select(n) %>% 
        as.integer()
      
      slice_sample(x, n = n.size)
    })
  
})


# save resampled data -----------------------------------------------------

saveRDS(resampled.occ, here("output", "resampled_occ_df.rds"))

  