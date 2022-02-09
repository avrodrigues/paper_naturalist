clean_eval_0 <- function(occ.cl, 
                       geo.space,
                       env.space,
                       level.filter = c("1_det_by_spec"),
                       r){

  
  
# Tests for arguments rules -----------------------------------------------

  
  # Include TEST FOR classified occurence data set
  
  
  # Include TEST FOR SF OBJECT!!!!
  #class(geo.space)
  
  if(nlayers(r) != 2) errorCondition("raster objetct must have two layers")
  
  

# Inicial data ------------------------------------------------------------


  occ.full <- occ.cl %>% 
    dplyr::select(decimalLongitude, decimalLatitude, species) 
  
  occ.cleaned <- occ.cl %>% 
    filter(naturaList_levels %in% level.filter) %>% 
    dplyr::select(decimalLongitude, decimalLatitude, species) 
  
  occ.list <- list(occ.full = occ.full, 
                   occ.cleaned = occ.cleaned)
  
  names.sp.full <- as.character(unique(occ.full$species))
  
  v <- ifelse(is.na(values(r[[1]])),
              NA, 0)
  
  sitexsp <- matrix(rep(v, length(names.sp.full)), 
                    nrow = ncell(r),
                    ncol = length(names.sp.full))
  
  colnames(sitexsp) <- names.sp.full
  
  
  

# Metrics computaion ------------------------------------------------------

 
  msg <- c("Calculating metrics before cleannig",
           "Calculating metrics after cleannig")
  
  res.list <- vector("list", 2)
  
  for(i in seq_along(occ.list)){
    occ <- occ.list[[i]]
    message(msg[i])
    
    ## vector for area output
    
    geo.area <- rep(0, length(names.sp.full))
    names(geo.area) <- names.sp.full
    env.area <- rep(0, length(names.sp.full))
    names(env.area) <- names.sp.full
    
    names.current <- as.character(unique(occ$species))
    names.sp <- names.sp.full %in% names.current
    
    # Geographical space
    message("..Step 1 - Geographical space")
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
    
    res.geo.area <- sapply(geo.polygon, function(x) sum(st_area(x)))
    geo.area[names.sp] <- res.geo.area
    
    # rasters for composition and richness
    geo.raster <- lapply(geo.polygon, function(x){
      x <- st_cast(x, "MULTIPOLYGON")
      
      
      fasterize(st_sf(a = 1, x), 
                r[[1]],
                background = 0)
    })
    
    stk <- stack(geo.raster)
    msk <- fasterize(st_sf(a = 1, geo.space), 
                     r[[1]])
    stk <- mask(stk, msk)
    sitexsp[,names.sp] <-  values(stk)
    
    # Enviromental space
    message("..Step 2 - Enviromental space")
    
    env.std <- decostand(as.data.frame(r), 
                         "range", 
                         na.rm = T)
    
    
    env.polygon <- lapply(unique(occ$species), function(i){
      x <- filter(occ, species == i)
      
      sp.cell <- unique(cellFromXY(r[[1]], x[, 1:2]))
      env.row <- row.names(env.std) %in% sp.cell
      env.xy <- env.std[sp.cell,]
      
      if(any(is.na(env.xy))){
        warningCondition("There are occurrence points in raster cells without values (NA)")
        }
      
      pt <- st_multipoint(na.omit(as.matrix(env.xy)))
      
      if(nrow(x) <=3){
        sp.pol <- st_buffer(pt, 0.025)
      }
      
      if(nrow(x) > 3){
        sp.pol <- st_convex_hull(pt)
        sp.pol <- st_buffer(sp.pol, 0.025)
      }
      
      suppressMessages(st_intersection(st_geometry(sp.pol), env.space))
    })
    
    res.env.area <- sapply(env.polygon, function(x) sum(st_area(x)))
    env.area[names.sp] <- res.env.area
    
    # results list for the loop
    res.list[[i]] <- list(geo.area = geo.area,
                          env.area = env.area,
                          sitexsp = sitexsp)
    
  }
  
  message("Preparing outputs")
  site.coords <- coordinates(r)
  
  remmain.geo.area <- round(res.list[[2]]$geo.area/res.list[[1]]$geo.area, 2)
  remmain.env.area <- round(res.list[[2]]$env.area/res.list[[1]]$env.area, 2)
  
  area <- list(r.geo.area = remmain.geo.area, 
               r.env.area = remmain.env.area)
  
  #composition before cleaning (BC) and after cleaning(AC)
  comp <- list(comp.BC = res.list[[1]]$sitexsp,
               comp.AC = res.list[[2]]$sitexsp)
  
  rich <- list(rich.BC = rowSums(res.list[[1]]$sitexsp),
               rich.AC = rowSums(res.list[[2]]$sitexsp))
  
  results <- list(area = area, 
                  comp = comp,
                  rich = rich, 
                  site.coords = site.coords)
  
  message("DONE!")
  return(results)
  
  
}

