library(naturaList)
library(tidyverse)
library(raster)
library(sp)
library(sf)
library(rnaturalearth)
library(vegan)
library(fasterize)
library(here)
source(here("function", "define_env_space.R"))
source(here("function", "clean_eval.R"))

# Load data ---------------------------------------------------------------


# species occurrence data
# occ.df
load(here("data", "occ_df.RData"))
occ.df <- filter(occ.df, year >= 1979)

# specialist files

spec.files <- grep("especialistas", 
                   list.files(here("data"), full.names = T), 
                   value = T)
l.spec.df <- lapply(spec.files, read.csv2)
names(l.spec.df) <- substr(spec.files, 99, nchar(spec.files)-4)

# environmental data
env.files <- list.files(here("data", "env"), full.names = T)
env.stack <- stack(env.files)

# standardized 
values(env.stack) <- scale(as.data.frame(env.stack))

env.stand <- na.omit(as.data.frame(env.stack))

env.pca <- prcomp(env.stand)
res.env.pca <- summary(env.pca)

#biplot(env.pca)
# Sumary statistics of PCA
# explanation
res.env.pca
#loadings
res.env.pca$rotation[,1:2]

# Select the first two axes
raster.pca <- predict(env.stack, env.pca, index = 1:2)


# Define limits of environmental and geographical space ---------------------

# define the possible environmental space
pca.df <- na.omit(as.data.frame(raster.pca))
env.space <- define_env_space(pca.df, buffer.size = 0.05)

# defining geographic region
neo_coastline <- ne_coastline(scale = 50, returnclass = "sf")
ext <- extent(env.stack)[c(1:4)]
names(ext) <- c("xmin", "xmax", "ymin", "ymax")
neo_coastline <- st_crop(st_geometry(neo_coastline), ext)
neo_coastline <- st_cast(neo_coastline, "MULTIPOLYGON")

# Classify, filter and compute metrics ------------------

# Specialist list: 1% most frequent strings
occ.cl.spec.1 <- classify_occ(occ.df,
                          l.spec.df$especialistas_1porcento,
                          spec.ambiguity = "not.spec")

occ.cl.spec.1 %>% 
  group_by(naturaList_levels) %>% 
  summarise(count = n()/nrow(occ.cl.spec.1))


l.eval.spec.1 <- clean_eval(occ.cl.spec.1, 
                     neo_coastline, 
                     env.space,
                     r = raster.pca)

saveRDS(l.eval.spec.1, here("output", "spec_1_clean_eval_results.rds"))


# Specialist list: 5% most frequent strings
occ.cl.spec.5 <- classify_occ(occ.df,
                              l.spec.df$especialistas_5porcento,
                              spec.ambiguity = "not.spec")

occ.cl.spec.5 %>% 
  group_by(naturaList_levels) %>% 
  summarise(count = n()/nrow(occ.cl.spec.5))


l.eval.spec.5 <- clean_eval(occ.cl.spec.5, 
                            neo_coastline, 
                            env.space,
                            r = raster.pca)

saveRDS(l.eval.spec.5, here("output", "spec_5_clean_eval_results.rds"))


# Specialist list: 10% most frequent strings
occ.cl.spec.10 <- classify_occ(occ.df,
                              l.spec.df$especialistas_10porcento,
                              spec.ambiguity = "not.spec")

occ.cl.spec.10 %>% 
  group_by(naturaList_levels) %>% 
  summarise(count = n()/nrow(occ.cl.spec.10))

l.eval.spec.10 <- clean_eval(occ.cl.spec.10, 
                            neo_coastline, 
                            env.space,
                            r = raster.pca)

saveRDS(l.eval.spec.10, here("output", "spec_10_clean_eval_results.rds"))


# Specialist list: provided by a specialist in Myrtaceae
occ.cl.spec.myrt <- classify_occ(occ.df,
                               l.spec.df$especialistas_Myrtaceae,
                               spec.ambiguity = "not.spec")

occ.cl.spec.myrt %>% 
  group_by(naturaList_levels) %>% 
  summarise(count = n()/nrow(occ.cl.spec.myrt))

l.eval.spec.myrt <- clean_eval(occ.cl.spec.myrt, 
                             neo_coastline, 
                             env.space,
                             r = raster.pca)

saveRDS(l.eval.spec.myrt, here("output", "spec_myrt_clean_eval_results.rds"))


# Specialist list: jointed the lists of 10% most freq. and provided by a specialist
occ.cl.spec.all <- classify_occ(occ.df,
                                 l.spec.df$especialistas_todos,
                                 spec.ambiguity = "not.spec")

occ.cl.spec.all %>% 
  group_by(naturaList_levels) %>% 
  summarise(count = n()/nrow(occ.cl.spec.all))

l.eval.spec.all <- clean_eval(occ.cl.spec.all, 
                               neo_coastline, 
                               env.space,
                               r = raster.pca)

saveRDS(l.eval.spec.all, here("output", "spec_all_clean_eval_results.rds"))


##############################
#### GRANDE RASCUNHO FINAL E COISAS ANTIGAS
############

cor(l.eval$area$r.geo.area, l.eval$area$r.env.area, use = "na.or.complete")


comp.BC <- l.eval$comp$comp.BC
comp.AC <- l.eval$comp$comp.AC

empty.sites <- rowSums(comp.BC) == 0 & rowSums(comp.AC) == 0
empty.sites[is.na(empty.sites)] <- TRUE

tbi.null <- ifelse(empty.sites, 0, 0)
tbi.all.sites <- rep(NA, nrow(comp.BC))

tbi <- TBI(comp.BC[!empty.sites,], 
           comp.AC[!empty.sites,])

tbi.all.sites[!empty.sites] <- tbi$TBI

r.tbi <- rasterFromXYZ(cbind(l.eval$site.coords,tbi.all.sites))
plot(r.tbi)


