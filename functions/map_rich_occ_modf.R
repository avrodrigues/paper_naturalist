
map_rich_occ_modif<- function(x,
         resolution = 1,
         species = "species",
         latitude = "latitude",
         longitude = "longitude",
         color.palette = c("white","red"),
         xlim = NULL,
         ylim = NULL,
         filter.by= NULL
){
  
  if(!is.null(filter.by) == TRUE){
    filter_level<- c("det_by_spec", "taxonomist", "image", "sci_colection", "field_obs",
                     "no_criteria_met") # levels allowed for filtering
    
    pattern<- filter_level[match(filter.by, filter_level)] # pattern to match with occ table
    
    filter<- unlist(lapply(pattern, function(y){
      unique(x$naturaList_levels)[grep(pattern = y, x = unique(x$naturaList_levels))]
    })) # occ.table level to be filtered
  }
  
  # crie um raster
  r <- raster::raster(resolution = resolution, # 1 grau decimal
                      ext = extent(c(-180,180,-90,90)))
  
  # cria uma lista de celulas do raster em que cada espécie ocorre
  species_data <- split(as.data.frame(x), x[,species])
  
  cells_occ <- list()

  for (i in seq_along(species_data)){
    #i= 327
    if(is.null(filter.by) == TRUE){
      cells_occ[[i]] <- unique(raster::cellFromXY(r, species_data[[i]][ ,c("decimalLongitude", "decimalLatitude")]))
    } else{
      cells_occ[[i]]<- unique(raster::cellFromXY(r, species_data[[i]][which(species_data[[i]][, "naturaList_levels"] == filter), 
                                                                      c("decimalLongitude", "decimalLatitude")]))
    }
  }
  

  # Gera valores de riqueza para cada pixel do raster
  riq_cell <- table(unlist(cells_occ))
  
  values_cell <- rep(NA, ncell(r))
  names(values_cell) <- 1:ncell(r)
  valid_cells <- names(values_cell) %in% names(riq_cell)
  values_cell[valid_cells] <- riq_cell
  
  richness_raster <- raster::setValues(r, values = values_cell)
  
  
  lims <- c(xlim,ylim)
  if(is.null(lims)){
    lims <- extent(xyFromCell(r, as.numeric(names(riq_cell)), spatial=T))
  }
  
  
  # Mapa
  countries <- rnaturalearth::ne_countries() # paises
  pal <- grDevices::colorRampPalette(color.palette) # Paleta de cores do raster
  
  # gerando mapa
  plot(richness_raster,
       xlim = lims[1:2]+c(-1,1),
       ylim = lims[3:4]+c(-1,1), # limites longitude/latitude
       col = pal(50))
  plot(countries, add = T)
}

