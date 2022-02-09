#' @param env matrix or data frame with two columns representing a bidimensional 
#'   envrionmental space
#' @param buffer.size numeric value indicating a buffer size arround each point
#'  which will delimit the enviromental space border
#' @param plot logical. whether to plot the polygon
#' @return sfc_POLYGON 
#' 
define_env_space <- function(env, 
                             buffer.size, 
                             plot = TRUE){
  env.range <- decostand(env, "range")
  env.point <-  st_multipoint(as.matrix(env.range))
  
  box <- c(0,1,0,1)
  r <- raster(extent(box), resolution = 0.025)
  r.cell <- unique(cellFromXY(r, env.range))
  xy.cell <- xyFromCell(r, r.cell)
  ch.point <-  st_multipoint(as.matrix(xy.cell))
  ch.buffer <- st_buffer(ch.point, buffer.size)
  
  box2 <- c(xmin = 0, ymin = 0, xmax = 1, ymax = 1)
  env.space <- st_crop(st_geometry(ch.buffer), box2)
  
  if(plot) plot(env.space)
  return(env.space)
}
