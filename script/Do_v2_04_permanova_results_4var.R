
# load packages -----------------------------------------------------------

library(tidyverse)
library(here)
library(raster)
library(vegan)
library(ape)

# classification based on each list of specialists
occ.cl.files <- list.files(here("output"), pattern = "occ", full.names = T)

occ.cl.list <- lapply(occ.cl.files, readRDS)
names(occ.cl.list) <- substr(occ.cl.files,101, nchar(occ.cl.files)-4)

permanova.list <- readRDS("output/permanova_list.rds")

permanova.res <- bind_rows(permanova.list)

perm.res.reduced <- 
permanova.res %>% 
  filter(term == "type") %>% 
  mutate(
    bias = if_else(p.value <= 0.05, TRUE, FALSE),
    n.size = permanova.res %>% filter(term == "Total") %>% dplyr::select(df)) %>% 
  dplyr::select(!term:SumOfSqs)


# species with bias in niche
perm.res.reduced %>% 
  filter(bias == T) 
  


