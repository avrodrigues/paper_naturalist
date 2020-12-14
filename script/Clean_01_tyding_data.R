library(dplyr)
library(lubridate)

myrt.file <- list.files("../Doutorado/selected_points", full.names = T)

length(myrt.file)

l.occ <- lapply(myrt.file, read.table, header = T, stringsAsFactors = F)

occ.raw <- do.call(rbind, l.occ)

# organizando tabela para o naturaList ------------------------------------

institutionCode  <- occ.raw$database.source
collectionCode   <- occ.raw$herb
catalogNumber    <- occ.raw$herb.code
year             <- year(as.Date(occ.raw$date.colected))
dateIdentified   <- occ.raw$date.determined
species          <- occ.raw$new.scientific.name
identifiedBy     <- occ.raw$determined.by
decimalLongitude <- occ.raw$longitude
decimalLatitude  <- occ.raw$latitude
basisOfRecord    <- ifelse(!is.na(occ.raw$herb), "PRESERVED_SPECIMEN", NA)
mediaType        <- rep(NA, nrow(occ.raw))
occurrenceID     <- occ.raw$database


new.df <- data.frame(institutionCode,
                     collectionCode, 
                     catalogNumber ,  
                     year ,           
                     dateIdentified  ,
                     species         ,
                     identifiedBy    ,
                     decimalLongitude,
                     decimalLatitude ,
                     basisOfRecord   ,
                     mediaType       ,
                     occurrenceID )

write.csv(occ.raw, "data/myrteae_raw.csv", row.names = F)
write.csv(new.df, "data/myrteae_to_naturaList.csv", row.names = F)



# data filtered only with coordinate cleaner ------------------------------
load("../Doutorado/data/Bases de dados ocorrencia/jt_dataset_final.RData")

str(jt.dataset.final)

# organizando tabela para o naturalist ------------------------------------


institutionCode  <- jt.dataset.final$database.source
collectionCode   <- jt.dataset.final$herb
catalogNumber    <- jt.dataset.final$herb.code
year             <- year(as.Date(jt.dataset.final$date.colected))
dateIdentified   <- jt.dataset.final$date.determined
species          <- jt.dataset.final$new.scientific.name
identifiedBy     <- jt.dataset.final$determined.by
decimalLongitude <- jt.dataset.final$longitude
decimalLatitude  <- jt.dataset.final$latitude
basisOfRecord    <- ifelse(!is.na(jt.dataset.final$herb), "PRESERVED_SPECIMEN", NA)
mediaType        <- rep(NA, nrow(jt.dataset.final))
occurrenceID     <- jt.dataset.final$database

new.df <- data.frame(institutionCode,
                     collectionCode, 
                     catalogNumber ,  
                     year ,           
                     dateIdentified  ,
                     species         ,
                     identifiedBy    ,
                     decimalLongitude,
                     decimalLatitude ,
                     basisOfRecord   ,
                     mediaType       ,
                     occurrenceID )

write.csv(jt.dataset.final, "data/myrteae_raw_CoordClean.csv", row.names = F)
write.csv(new.df, "data/myrteae_to_naturaList_CoordClean.csv", row.names = F)
