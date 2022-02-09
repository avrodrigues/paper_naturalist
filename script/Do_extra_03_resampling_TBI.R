# load packages -----------------------------------------------------------

library(fasterize)
library(naturaList)
library(here)
library(raster)
library(sp)
library(sf)
library(rnaturalearth)
library(tidyverse)
library(tidymodels)
library(adespatial)


# load data ---------------------------------------------------------------

comp.occ.cleaned <- readRDS(here("output", "comp_occ_cleaned.rds"))

l.resampled.comp <-  list.files(here("output", "resampled_occ_comp"), full.names = T)

# prepare data ------------------------------------------------------------

sp.names.cleaned <- colnames(comp.occ.cleaned)

top10.genus <- c("Calycolpus", "Campomanesia", "Eugenia", "Myrceugenia",
                 "Myrcia", "Myrcianthes", "Myrciaria", "Neomitranthes", 
                 "Plinia", "Psidium")

tbi.genus.mtx <- matrix(nrow = nrow(comp.occ.cleaned), 
                        ncol = 100, 
                        dimnames = list(1:nrow(comp.occ.cleaned), 
                                        paste0("s", 1:100))
                        )

tbi.resampled <- lapply(1:10, function(x) {tbi.genus.mtx})
names(tbi.resampled) <- top10.genus

# Calculating the TBI index for the resampled uncleaned datasets ------------

for(k in 1:100){ ## each resampled dataset
  
  #resampled dataset
  message(
    paste0("Loading data. Run ", k)
  )
  comp.mtx.resampled <- readRDS(l.resampled.comp[k])
  sp.names.resampled <- colnames(comp.mtx.resampled)
  
  # species in any dataset
  j.sp.names <- unique(c(sp.names.cleaned, sp.names.resampled))
  
  
  # prepare matrices for TBI
  l.mtx.ori <-list(resampled = comp.mtx.resampled, cleaned = comp.occ.cleaned)
  
  l.mtx.tbi <- list(resampled = NULL, cleaned = NULL)
  
  for(i in seq_along(l.mtx.tbi)){
    
    sitexsp <- matrix(0, 
                      nrow = nrow(comp.occ.cleaned),
                      ncol = length(j.sp.names), 
                      dimnames = list(1:nrow(comp.occ.cleaned), j.sp.names)
    )
    
    sp.in.mtx <- j.sp.names %in% colnames(l.mtx.ori[[i]]) 
    
    sitexsp[,sp.in.mtx] <- l.mtx.ori[[i]]
    
    l.mtx.tbi[[i]] <- sitexsp
  }
  
  comp.bc <- as_tibble(l.mtx.tbi$resampled)
  comp.ac <- as_tibble(l.mtx.tbi$cleaned)
  
  # composition by genus
  genus.comp <- 
    lapply(top10.genus, function(genus) {
      #before cleaning
      bc <- comp.bc %>% 
        dplyr::select(starts_with(genus)) 
      #after cleaning
      ac <- comp.ac %>% 
        dplyr::select(starts_with(genus))
      
      list(bc = bc, ac = ac)
    })
  
  names(genus.comp) <- top10.genus
  
  message(
    paste0("Calculating TBI. Run ", k)
  )
  # calculate Temporal beta-diversity Index (% change)
  tbi.list <- lapply(genus.comp, function(x){
    keep.site <- rowSums(x$ac, na.rm = T) > 0
    
    mtx_bc <- as.matrix(x$bc[keep.site,])
    mtx_ac <- as.matrix(x$ac[keep.site,])
    
    tbi.genus <- TBI(mtx_bc, mtx_ac)  
    
    tbi.all.sites <- rep(NA, length(keep.site))
    tbi.all.sites[keep.site] <- tbi.genus$TBI
    
    tbi.all.sites
  })
  
  # Organize TBI results
  
  for(genus in seq_along(tbi.list)){
    tbi.resampled[[genus]][,k] <- tbi.list[[genus]]
  }
}

saveRDS(tbi.resampled, here("output", "tbi_resampling_results.rds"))

# summarizing results -----------------------------------------------------

length(tbi.resampled)


res.mean.sd <- vector('list', 10)
names(res.mean.sd) <- top10.genus


for(i in seq_along(tbi.resampled)){
  
  mean.vec <- rowMeans(tbi.resampled[[i]], na.rm = T)
  sd.vec <- apply(tbi.resampled[[i]], 1, sd,  na.rm = T)
  
  res.mean.sd[[i]] <- data.frame(mean = mean.vec, sd = sd.vec)
}

  hist(res.mean.sd$Psidium$mean)
