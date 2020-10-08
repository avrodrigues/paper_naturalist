
####### All species and coordinates
occ_spp<- split(as.data.frame(myrt.cl.occ), myrt.cl.occ[,"species"])
names(rich_all$XY) <- names(split(as.data.frame(myrt.cl.occ), myrt.cl.occ[,"species"]))
names(rich_spec.taxon$XY) <- names(split(as.data.frame(myrt.cl.occ), myrt.cl.occ[,"species"]))
names(rich_spec$XY) <- names(split(as.data.frame(myrt.cl.occ), myrt.cl.occ[,"species"]))
names(rich_taxon$XY) <- names(split(as.data.frame(myrt.cl.occ), myrt.cl.occ[,"species"]))

## convex hull for all species

chull_spp<- lapply(rich_all$XY, function(x){
  ch <- chull(x)
  coords <- x[c(ch, ch[1]), ]
  coords
})

one_occ<- names(which(is.na(lapply(chull_spp, function(x){
  ifelse(dim(x)[1] >= 4, x, NA)
})) == T)) # species with less than 3 occurrences

chull_spp_noNA<- chull_spp[- match(one_occ, names(chull_spp))] # removing species with less than 3 occurreces


## convex hull for specialist and taxonomist
names_spp_Spec.tax <- names(unlist(lapply(rich_spec.taxon$XY, function(x){
  which(dim(x)[1] == 0 | any(is.na(x)) == TRUE)
})))
rich_spec.taxon_coords<- rich_spec.taxon$XY[- match(names_spp_Spec.tax, names(rich_spec.taxon$XY))]

chull_spp_spec.tax<- lapply(rich_spec.taxon_coords, 
                   function(x){ 
                     ch <- chull(x) 
                     coords <- x[c(ch, ch[1]), ]
                     coords
})

# convex hull for specialist
names_spp_Spec <- names(unlist(lapply(rich_spec$XY, function(x){
  which(dim(x)[1] == 0 | any(is.na(x)) == TRUE)
})))
rich_spec_coords<- rich_spec$XY[- match(names_spp_Spec, names(rich_spec$XY))]

chull_spp_spec<- lapply(rich_spec_coords, 
                            function(x){ 
                              ch <- chull(x) 
                              coords <- x[c(ch, ch[1]), ]
                              coords
                            })

# convex hull for taxonomist
names_spp_taxon <- names(unlist(lapply(rich_taxon$XY, function(x){
  which(dim(x)[1] == 0 | any(is.na(x)) == TRUE)
})))
rich_taxon_coords<- rich_taxon$XY[- match(names_spp_taxon, names(rich_taxon$XY))]

chull_spp_taxon<- lapply(rich_taxon_coords, 
                        function(x){ 
                          ch <- chull(x) 
                          coords <- x[c(ch, ch[1]), ]
                          coords
                        })

## converting convex hull in spatial polygons
spp_polygons<- lapply(chull_spp, function(x){
  SpatialPolygons(list(Polygons(list(Polygon(x)), ID=1)))
})

# specialists and taxonomists
spp_polygons_spec.tax<- lapply(chull_spp_spec.tax, function(x){
  SpatialPolygons(list(Polygons(list(Polygon(x)), ID=1)))
})

# specialist
spp_polygons_spec<- lapply(chull_spp_spec, function(x){
  SpatialPolygons(list(Polygons(list(Polygon(x)), ID=1)))
})

# taxonomist
spp_polygons_taxon<- lapply(chull_spp_taxon, function(x){
  SpatialPolygons(list(Polygons(list(Polygon(x)), ID=1)))
})


## spatial polygons only with species containing more than 3 occurrece records
spp_polygons_noNA<- lapply(chull_spp_noNA, function(x){
  SpatialPolygons(list(Polygons(list(Polygon(x)), ID=1)))
})


## converting spatial polygons to spatial polygons data frame
sp <- bind(spp_polygons)
spdf <- SpatialPolygonsDataFrame(sp, data.frame(id=1:length(sp)))   
spdf$SCINAME <- names(rich_all$XY) # adding a column with species names

# converting spatial polygons using species with more than 3 occurrence records
sp_noNA <- bind(spp_polygons_noNA)
spdf_noNA <- SpatialPolygonsDataFrame(sp_noNA, data.frame(id=1:length(sp_noNA)))   
spdf_noNA$SCINAME <- names(chull_spp_noNA) # adding a column with species names

# converting spatial polygons for specialist and taxonomist
sp_spec.tax <- bind(spp_polygons_spec.tax)
spdf_spec.tax <- SpatialPolygonsDataFrame(sp_spec.tax, data.frame(id=1:length(sp_spec.tax)))   
spdf_spec.tax$SCINAME <- names(rich_spec.taxon_coords) # adding a column with species names

# converting spatial polygons for specialist 
sp_spec <- bind(spp_polygons_spec)
spdf_spec <- SpatialPolygonsDataFrame(sp_spec, data.frame(id=1:length(sp_spec)))   
spdf_spec$SCINAME <- names(rich_spec_coords) # adding a column with species names

# converting spatial polygons for taxonomist 
sp_taxon <- bind(spp_polygons_taxon)
spdf_taxon <- SpatialPolygonsDataFrame(sp_taxon, data.frame(id=1:length(sp_taxon)))   
spdf_taxon$SCINAME <- names(rich_taxon_coords) # adding a column with species names



## transforming the spatial polygon data frame in a occurrence matrix
PAM <- lets.presab(spdf, xmn = -99.5, xmx = -35.0019,
            ymn = -42.69972, ymx = 24.83, res = 1)

PAM_spec.tax <- lets.presab(spdf_spec.tax, xmn = -99.5, xmx = -35.0019,
                            ymn = -42.69972, ymx = 24.83, res = 1)

PAM_spec <- lets.presab(spdf_spec, xmn = -100, xmx = -35,
                            ymn = -30.59314, ymx = 20, res = 1)

PAM_taxon <- lets.presab(spdf_taxon, xmn = -100, xmx = -35,
                        ymn = -30.59314, ymx = 20, res = 1)


quartz()
layout.show(layout(matrix(c(1, 2, 3, 4), nrow= 2, ncol= 2, byrow = T)))
plot(PAM, main= "all crit")
plot(PAM_spec.tax, main= "specialist/taxonomist")
plot(PAM_spec, main= "specialist")
plot(PAM_taxon, main= "taxonomist")
