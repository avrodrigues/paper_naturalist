library(naturaList)
library(here)
library(stringr)
library(dplyr)


myrt.data <- read.csv(here::here("data", "myrteae_to_naturaList.csv"))
myrt.data.CC <- read.csv(here::here("data","myrteae_to_naturaList_CoordClean.csv"))
myrt.spec.all <- read.csv2(here::here("data","especialistas_todos.csv"))


# Atualizar nomes cientificos (The Plant List) ----------------------------

tax_list <- Taxonstand::TPL(levels(myrt.data.CC$species))


# Filtar apenas nomes aceitos --------------------------------------------
tax_list_accepted <- tax_list %>% 
  mutate(new.binomial =  paste(New.Genus, New.Species)) %>% 
  filter(New.Taxonomic.status == "Accepted") 

tax.aceitos <- myrt.data.CC$species %in% tax_list_accepted$Taxon

myrt.data.tax <- myrt.data.CC[tax.aceitos, ]


# Atualizar sinonimos ---------------------------------------------

tax_list_synon <- tax_list_accepted %>% 
  mutate(change.name = Taxon != new.binomial) %>% 
  filter(change.name == T)

sp.binom <- tax_list_synon$new.binomial
names(sp.binom) <- tax_list_synon$Taxon

myrt.data.tax$species <- str_replace_all(myrt.data.tax$species, sp.binom)

write.csv(myrt.data.tax, "myrt.data.tax.csv", row.names = F)


# Classificação naturaList ------------------------------------------------

myrt.data.tax <- read.csv("myrt.data.tax.csv")

myrt.cl.occ.all <- classify_occ(myrt.data.tax, 
                                myrt.spec.all,
                                spec.ambiguity = "is.spec") # not.spec or manual.check


save(myrt.cl.occ.all, myrt.data.tax, tax_list, 
     file = here("data", "classify_occ_data.RData"))
