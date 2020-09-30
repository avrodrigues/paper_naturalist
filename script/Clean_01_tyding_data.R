library(dplyr)
library(lubridate)

myrt.file <- list.files("../Doutorado/selected_points", full.names = T)

length(myrt.file)

l.occ <- lapply(myrt.file, read.table, header = T, stringsAsFactors = F)

occ.raw <- do.call(rbind, l.occ)
tbl.occ.raw <- as_tibble(occ.raw)


# organizando tabela para o naturalist ------------------------------------


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
occurrenceID     <- rep(NA, nrow(occ.raw))

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