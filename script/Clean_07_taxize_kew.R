
# Standardize taxonomic names ---------------------------------------------


# load packages -----------------------------------------------------------

library(taxize)
library(tidyverse)
library(here)



# load data ---------------------------------------------------------------

load(here("data", "occ_df.RData"))

occ.df <-
occ.df %>% 
  filter(year >= 1979)

sp.names.raw <-
occ.df %>% 
  select(species) %>% 
  distinct() %>% 
  arrange(species)


# standardize names -------------------------------------------------------


spdata <- as.character(sp.names.raw$species)
pow.list <- vector("list", length(spdata))
for(i in 599:length(spdata)){
  x <-spdata[i]
  
  g <- get_pow(x, ask = F, messages = F)
  pow_id <- as.character(g)
  if(is.na(g)){
    found <- attr(g,"match")
    multi <- ifelse(attr(g,"multiple_matches"), "multiple_matches", NA)
    
    pow.list[[i]] <- data.frame(
      name_searched = x,
      taxonomic_status = found,
      nomenclatural_status = multi,
      name_accepted = NA
    ) 
    
    next()
  }
  
  lk <- pow_lookup(pow_id)
  
  tax_status <- lk$meta$taxonomicStatus
  nom_status <- lk$meta$nomenclaturalStatus
  
  if(tax_status != "Accepted"){
    if(tax_status == "Unplaced"){
      accepted.name <- NA
    }else{
      accepted.name <- lk$meta$accepted$name
    }
    
  } else {
    accepted.name <- x
  }
  
  pow.list[[i]] <- data.frame(
    name_searched = x,
    taxonomic_status = tax_status,
    nomenclatural_status = nom_status,
    name_accepted = accepted.name
  ) 
  
}

pow.df <- as_tibble(do.call(rbind, pow.list))

pow.resolved <- 
pow.df %>% 
  filter(nomenclatural_status != "multiple_matches",
         taxonomic_status != "not found")

pow.not.m.matches<- 
pow.df %>% 
  filter(nomenclatural_status == "multiple_matches")

pow.not.m.matches$name_accepted <-
c("Calycolpus roraimensis",
  "Myrcia carioca", #synonym
  "Myrcia legrandii", #synonym
  "Myrcia megapaniculata", #synonym
  "Myrcia zuzygium", #synonym
  "Eugenia axillaris",
  "Eugenia bimarginata",
  "Eugenia astringens", #synonym
  "Eugenia cerasiflora",
  "Eugenia cordata",
  NA, # doubius
  "Eugenia crenulata",
  "Eugenia emarginata",
  "Eugenia expansa",
  "Eugenia flavescens",
  "Eugenia glandulosa",
  "Eugenia grandiflora",
  "Eugenia herbacea",
  NA, # doubius
  "Eugenia linearis",
  "Eugenia livida",
  "Eugenia macrocarpa",
  "Eugenia magnifica",
  "Eugenia maritima",
  "Eugenia modesta",
  "Eugenia mucronata",
  "Eugenia nutans",
  "Eugenia disperma", # synonym
  "Eugenia pauciflora",
  "Eugenia platyphylla",
  NA, #dubious
  "Eugenia ramiflora",
  "Eugenia rosea",
  "Eugenia trinervia",
  "Eugenia trunciflora",
  "Eugenia umbrosa",
  "Eugenia uniflora",
  NA, #dubious
  "Myrcia clusiifolia",
  NA, #dubious
  "Myrcia lanuginosa",
  "Myrcia nitida",
  "Myrcia polyantha",
  "Myrcia pubescens",
  "Myrcia reticulata",
  "Myrcia myrtillifolia", # synonym
  "Psidium cattleyanum", #mispelled
  NA, #dubious
  NA #duboius
  )


pow.not.found <- 
pow.df %>% 
  filter(taxonomic_status == "not found") 

pow.not.found$name_searched

#manual search in INPI (https://www.ipni.org/)
pow.not.found$name_accepted <- 
c("Myrcia estremerae", # mispelled and synonym
  NA,
  "Eugenia riosiae", # mispelled
  NA, 
  "Myrcia abbotiana", # mispelled
  "Plinia spirito-sanctensis" # mispelled
  ) 
pow.not.found$nomenclatural_status <- 
c("mispelled and synonym", NA, "mispelled", NA, "mispelled", "mispelled")


sp.names.pow <- 
rbind(pow.resolved, pow.not.m.matches, pow.not.found)

occ.df.renamed <- occ.df
for(i in 1:nrow(sp.names.pow)){
  pttrn <- paste0("^", sp.names.pow$name_searched[i], "$")
  str_new <- sp.names.pow$name_accepted[i]
  occ.df.renamed$species <- 
    str_replace_all(occ.df.renamed$species, pttrn, str_new)
}

occ.df.kew <- 
occ.df.renamed %>% 
  drop_na(species) %>% 
  mutate(genus = word(occ.df.kew$species))

length(unique(occ.df.kew$species))

save(occ.df.kew, file = here("data", "occ_df_kew.RData"))
