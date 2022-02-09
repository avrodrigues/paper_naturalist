niche_area_richness <- function(occ,
                                geo.space,
                                env.space
                                ){
  require(raster)
  require(sf)
  
  # Geographical space
  geo.polygon <- lapply(unique(occ$species), function(i){
    x <- filter(occ, species == i)
    pt <-st_multipoint(as.matrix(x[,1:2]))
    
    if(nrow(x) <=3){
      sp.pol <- st_buffer(pt, 0.5)
    }
    
    if(nrow(x) > 3){
      sp.pol <- st_convex_hull(pt)
      sp.pol <- st_buffer(sp.pol, 0.5)
    }
    geo <- st_geometry(sp.pol)
    st_crs(geo) <- 4326
    suppressMessages(st_intersection(geo, geo.space))
  })
  
  geo.area <- sapply(geo.polygon, function(x) sum(st_area(x)))

  
  # richness
  geo.raster <- lapply(geo.polygon, function(x){
    x <- st_cast(x, "MULTIPOLYGON")
    
    
    fasterize(st_sf(a = 1, x), temp)
  })
  
  st.geo.raster <- stack(geo.raster)
  richness <- values(sum(st.geo.raster, na.rm = T))
  
  # point richness
  pt.richness <- map_rich_occ(occ, 
               resolution = 0.5,
               species = "species",
               latitude = "decimalLatitude",
               longitude = "decimalLongitude",
               plot = F,
               raster = T,
               xlim = c(-120,-30),
               ylim = c(-60,35)
  )  
  
  pt.richness.val <- values(pt.richness)
  
  # Environmental space
  env.polygon <- lapply(unique(occ$species), function(i){
    x <- filter(occ, species == i)
    
    sp.cell <- unique(cellFromXY(temp, x[, 1:2]))
    env.row <- row.names(env.stand) %in% sp.cell
    env.xy <- env.stand[env.row,]
    
    pt <-st_multipoint(as.matrix(env.xy))
    
    if(nrow(x) <=3){
      sp.pol <- st_buffer(pt, 0.025)
    }
    
    if(nrow(x) > 3){
      sp.pol <- st_convex_hull(pt)
      sp.pol <- st_buffer(sp.pol, 0.025)
    }
    
    suppressMessages(st_intersection(st_geometry(sp.pol), env.space))
  })
  
  env.area <- sapply(env.polygon, function(x) sum(st_area(x)))
  
  names(geo.area) <- unique(occ$species)
  names(env.area) <- unique(occ$species)
  results <- list(env.area = env.area, 
                  geo.area = geo.area, 
                  pol.richness = richness,
                  pt.richness = pt.richness.val)
  
  return(results)
}
