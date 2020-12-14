library(naturaList)
library(here)

source(here("function", "map_module2.R"))

load(here("data", "classify_occ_data.RData"))
myrt.spec.all <- read.csv2(here::here("data","especialistas_todos.csv"))


# map_module --------------------------------------------------------------


for (i in sort(unique(myrt.cl.occ.all$species))[1581:1609]){
  
  sp.filtered <- map_module2(myrt.cl.occ.all[myrt.cl.occ.all$species == i,])
  
  i_ <- gsub(" ", "_", i)
  write.table(sp.filtered, 
              file = paste0("data/map_module_output/", i_, ".txt"),
              row.names = F)
  
}



  

