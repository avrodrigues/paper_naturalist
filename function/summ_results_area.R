summ_results_area <- function(list.res, 
                              ref.res){
  
  # area in enviromental space
  envlist <- sapply(list.res, "[", 1)
  
  env.list.std <- lapply(envlist, function(z){
    apply(z, 2, function(x){
      as.numeric(x)/as.numeric(ref.res$env.area)
    })
  })
  
  names(env.list.std) <- c(0.1,0.3,0.5,0.7,0.9)
  
  mean.envlist <- lapply(env.list.std, rowMeans)
  mean.env <- do.call(c, mean.envlist)
  sd.envlist <- lapply(env.list.std, apply, 1, sd)
  sd.env <- do.call(c, sd.envlist)
  prop.col <- rep(c(0.1,0.3,0.5,0.7,0.9), each = 1180)
  sp.names <- rep(names(all.occ$env.area), 5)
  
  data.env <- data.frame(prop = prop.col, 
                         env.area = mean.env,
                         env.area.sd = sd.env,
                         sp = sp.names)
  
  # area in geographical space
  geolist <- sapply(list.res, "[", 2)
 
  geo.list.std <- lapply(geolist, function(z){
    apply(z, 2, function(x){
      as.numeric(x)/as.numeric(ref.res$geo.area)
    })
  })
  
  names(geo.list.std) <- c(0.1,0.3,0.5,0.7,0.9)
  
  mean.geolist <- lapply(geo.list.std, rowMeans)
  mean.geo <- do.call(c, mean.geolist)
  sd.geolist <- lapply(geo.list.std, apply, 1, sd)
  sd.geo <- do.call(c, sd.geolist)
 
  data.geo <- data.frame(prop = prop.col, 
                         geo.area = mean.geo,
                         geo.area.sd = sd.geo,
                         sp = sp.names)
  
  return(list(data.env = data.env,
              data.geo = data.geo))
  
}
