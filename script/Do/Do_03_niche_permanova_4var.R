
# load packages -----------------------------------------------------------

library(tidyverse)
library(here)
library(raster)
library(vegan)
library(ape)


# load data ---------------------------------------------------------------

# classification based on each list of specialists
occ.cl.files <- list.files(here("output"), pattern = "occ_cl_", full.names = T)

occ.cl.list <- lapply(occ.cl.files, readRDS)
names(occ.cl.list) <- substr(occ.cl.files,101, nchar(occ.cl.files)-4)


# raster layer
# environmental data
env.files <- list.files(here("data", "env"), full.names = T)
env.stack <- stack(env.files[-c(1,5)])



# Permanova test ----------------------------------------------------------

names((occ.cl.list$occ_cl_spec_all))
species.names <- unique(occ.cl.list$occ_cl_spec_all$species)

permanova.list <- vector("list", length(species.names))
for(i in seq_along(species.names)){
  
  
  sp <- species.names[i] #sample(species.names, 1)
  # select species and split the data 
  occ.spec <- occ.cl.list$occ_cl_spec_all %>%
    filter(species == sp) %>%
    mutate(
      type = if_else(naturaList_levels == "1_det_by_spec", "spec", "not.spec")
    ) %>% 
    dplyr::select(decimalLongitude, decimalLatitude, species, type) %>% 
    arrange(desc(type))
  
  test.1 <- table(occ.spec$type)
  if(length(test.1) < 2 | any(test.1 < 10)) {
    permanova.list[[i]] <- NULL
    cat(i,  fill = T)
    next()
  }
  
  points.spec <- occ.spec %>% 
    filter(type == "spec") %>% 
    dplyr::select(decimalLongitude, decimalLatitude) %>% 
    SpatialPoints()
    
  points.not.spec <- occ.spec %>% 
    filter(type == "not.spec") %>% 
    dplyr::select(decimalLongitude, decimalLatitude) %>% 
    SpatialPoints()
  
  # extract envriomental variables
  env.df.spec <- as.data.frame(raster::extract(env.stack, points.spec)) %>% 
    setNames(c("temp_mean", "prec_annual",
               "prec_season", "temp_season")) 
  
  env.df.not.spec <- as.data.frame(raster::extract(env.stack, points.not.spec)) %>% 
    setNames(c("temp_mean", "prec_annual",
               "prec_season", "temp_season")) 
  
  env.df <- bind_rows(env.df.spec, env.df.not.spec) 
  
  # Identify duplicates in env data
  dup.spec <- duplicated(env.df.spec)
  dup.not.spec <- duplicated(env.df.not.spec)
  
  dup <- c(dup.spec, dup.not.spec)
  
  # standardize env data
  env.df.std <- decostand(env.df[!dup, ], "stand")
  data_dist <-vegdist(env.df.std, method="euclidean", na.rm = FALSE)
  
  # is there data able to comparsion?
  type <- occ.spec$type[!dup]
  
  if(any(table(type) < 10)) {
    permanova.list[[i]] <- NULL
    cat(i,  fill = T)
    next()
  } else {
    # compute permanova
    permanova <- adonis2(data_dist ~ type, method = "euclidean") %>% 
      broom::tidy() %>% 
      mutate(species = sp)
    
    #save results
    permanova.list[[i]] <- permanova
    cat(i,  fill = T)
  }
  
}


saveRDS(permanova.list, "output/permanova_list_4var.rds")


