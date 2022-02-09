# load packages -----------------------------------------------------------

library(fasterize)
library(naturaList)
library(here)
library(raster)
library(sp)
library(sf)
library(rnaturalearth)
library(tidyverse)


# load data ---------------------------------------------------------------

resampled.occ <- readRDS(here("output", "resampled_occ_df.rds"))

# occurence data
occ.cl.files <- list.files(here("output"), pattern = "occ_cl", full.names = T)

occ.cl.list <- lapply(occ.cl.files, readRDS)
names(occ.cl.list) <- substr(occ.cl.files,101, nchar(occ.cl.files)-4)

# raster layer
# environmental data
env.files <- list.files(here("data", "env"), full.names = T)
env.stack <- stack(env.files)

# defining geographic region
neo_coastline <- ne_coastline(scale = 50, returnclass = "sf")
ext <- c(-120, -30, -60, 35)
names(ext) <- c("xmin", "xmax", "ymin", "ymax")
neo_coastline <- st_crop(st_geometry(neo_coastline), ext)
neo_coastline <- st_cast(neo_coastline, "MULTIPOLYGON")


# convex-hull and composition matrix function --------------------------------

#' occ.df data frame. Occurence data with at least three colluns:
#'   decimalLongitude, decimalLatitude and species.
#' r a raster object from which the cell will become sites in the composition matrix
#' geo.space an sf object with the bondaries of the geographical area of study,
#'   e.g continental bondaries
#'
#' return a composition matrix of presence-absence

comp.mtx.chull <- function(occ.df, r, geo.space){
  
  ## preparing data
  occ <- occ.df %>%
    dplyr::select(.data$decimalLongitude, .data$decimalLatitude, .data$species) %>%
    dplyr::arrange(species)
  
  names.to.mtx <- unique(as.character(occ$species))
  
  v <- ifelse(is.na(raster::values(r[[1]])),
              NA, 0)
  
  sitexsp <- matrix(rep(v, length(names.to.mtx)),
                    nrow = raster::ncell(r),
                    ncol = length(names.to.mtx))
  
  colnames(sitexsp) <- names.to.mtx
  
  # Convex hull
  
  geo.polygon <- lapply(unique(occ$species), function(i){
    x <- dplyr::filter(occ, .data$species == i)
    pt <- sf::st_multipoint(as.matrix(x[,1:2]))
    
    if(nrow(x) <=3){
      sp.pol <- sf::st_buffer(pt, 0.5)
    }
    
    if(nrow(x) > 3){
      sp.pol <- sf::st_convex_hull(pt)
      sp.pol <- sf::st_buffer(sp.pol, 0.5)
    }
    geo <- sf::st_geometry(sp.pol)
    sf::st_crs(geo) <- 4326
    suppressMessages(sf::st_intersection(geo, geo.space))
  })
  
  
  # rasters for composition 
  geo.raster <- lapply(geo.polygon, function(x){
    x <- sf::st_cast(x, "MULTIPOLYGON")
    
    
    fasterize::fasterize(sf::st_sf(a = 1, x),
                         r[[1]],
                         background = 0)
  })
  
  stk <- raster::stack(geo.raster)
  msk <- fasterize::fasterize(sf::st_sf(a = 1, geo.space),
                              r[[1]])
  stk <- raster::mask(stk, msk)
  
  names.current <- as.character(unique(occ$species))
  names.sp <- names.to.mtx %in% names.current
  sitexsp[,names.sp] <-  raster::values(stk)
  
  
  return(sitexsp)
  
}



# composition matrix for each occurrence dataset --------------------------

occ.cleaned <- occ.cl.list$occ_cl_spec_all %>% 
  filter(naturaList_levels ==  "1_det_by_spec")

comp.occ.cleaned <- comp.mtx.chull(occ.cleaned, 
                                   env.stack[[1]],
                                   neo_coastline)

saveRDS(comp.occ.cleaned, here("output", "comp_occ_cleaned.rds"))

#dir.create(here("output", "resampled_occ_comp"))

for(i in 1:length(resampled.occ)){
  
  comp.mtx <- 
    comp.mtx.chull(resampled.occ[[i]], 
                   env.stack[[1]],
                   neo_coastline)
  
  i_char <- as.character(i)
  if(nchar(i_char) == 1) num_file <- paste0("00", i)
  else if (nchar(i_char) == 2) num_file <- paste0("0", i)
  else if (nchar(i_char) == 3) num_file <- i_char
  
  file.name <- paste0("comp_mtx_", num_file, ".rds")
  
  saveRDS(comp.mtx, here("output", "resampled_occ_comp", file.name))
}


