summ_results_richness <- function(list.res, 
                                  ref.res){
  ## richness from polygons
  richlist.pol <- sapply(list.res, "[", 3)
  
  cor.rich.pol <- sapply(richlist.pol, function(x){
    sapply(1:ncol(x), 
                function(i) cor(ref.res[[3]], x[, i]))
  })
  
  mean.rich.pol <- colMeans(cor.rich.pol)
  sd.rich.pol <- apply(cor.rich.pol, 2, sd)
  
  
  data.rich.pol <- data.frame(prop = c(0.1,0.3,0.5,0.7,0.9), 
                              mean.rich = mean.rich.pol, 
                              lower.rich = mean.rich.pol-sd.rich.pol,
                              upper.rich = mean.rich.pol+sd.rich.pol)
  
  ## richness from points
  richlist.pt <- sapply(list.res, "[", 4)
  
  cor.rich.pt <- sapply(richlist.pt, function(x){
    sapply(1:ncol(x), 
           function(i) cor(ref.res[[4]], x[, i], use = "na.or.complete"))
  })
  
  mean.rich.pt <- colMeans(cor.rich.pt, na.rm = T)
  sd.rich.pt <- apply(cor.rich.pt, 2, sd, na.rm = T)
  
  
  data.rich.pt <- data.frame(prop = c(0.1,0.3,0.5,0.7,0.9), 
                              mean.rich = mean.rich.pt, 
                              lower.rich = mean.rich.pt-sd.rich.pt,
                              upper.rich = mean.rich.pt+sd.rich.pt)
  
  list(data.rich.pol = data.rich.pol,
       data.rich.pt = data.rich.pt)
  
}
