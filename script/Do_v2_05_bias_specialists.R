
# load pakages ------------------------------------------------------------

library(naturaList)
library(raster)
library(rnaturalearth)
library(tidyverse)
library(here)
library(viridis)
library(MetBrewer)

# load data -----------------------------------------------------------

# classified occ
occ.cl.spec.all <- readRDS("output/occ_cl_spec_all.rds")

# number of occ with string in 'determinedBy'
sum(table(occ.cl.spec.all$naturaList_levels)[1:2])

# percentage of all occ
sum(table(occ.cl.spec.all$naturaList_levels)[1:2])/nrow(occ.cl.spec.all)

# defining region extent
NEO <- list(x = c(-120,-30),
            y = c(-60,35))

# empty raster, 1 degree resolution
r.1deg <- raster(resolution = 1, ext = extent(unlist(NEO)) )

# cell which each occ fall
cells.num <-
cellFromXY(
  r.1deg, 
  as.matrix(occ.cl.spec.all[, c("decimalLongitude", "decimalLatitude")])
)

# summary: percentage of occ determined by specialists from the occs with the
# name of who identified
perc_spec_cell <- 
occ.cl.spec.all %>% 
  mutate(cell = cells.num) %>% 
  filter(
    naturaList_levels %in% c("1_det_by_spec", "2_taxonomist")
  ) %>% 
  group_by(cell, naturaList_levels) %>% 
  summarise(n = n()) %>% 
  pivot_wider(id_cols = cell, names_from = naturaList_levels, values_from = n) %>% 
  janitor::clean_names() %>%  
  mutate(
    x2_taxonomist = ifelse(is.na(x2_taxonomist), 0, x2_taxonomist),
    x1_det_by_spec = ifelse(is.na(x1_det_by_spec), 0, x1_det_by_spec),
    total = sum(x2_taxonomist, x1_det_by_spec, na.rm = T),
    percentage_spec = (x1_det_by_spec/total)*100) %>% 
  dplyr::select(cell, percentage_spec) %>% 
  ungroup()

# values to raster
perc_values <- rep(NA, ncell(r.1deg))                                  

for(i in seq_along(perc_values)) {
  if(!i %in% perc_spec_cell$cell) next()
  
  perc_values[i] <- 
    unlist(perc_spec_cell[perc_spec_cell$cell == i, "percentage_spec"])
}          

r.perc.spec <- setValues(r.1deg, perc_values)

# preview raster
plot(r.perc.spec)


# preparing data to map
perc.spec.df <- as.data.frame(r.perc.spec, xy = T)
names(perc.spec.df) <- c("x", "y", "perc_specialists")


# defining geographic region
neo_coastline <- ne_coastline(scale = 50, returnclass = "sf")


map.settings <- 
  list(
    geom_sf(data = neo_coastline, size = 0.5, color = "grey50"),
    coord_sf(xlim = NEO$x, ylim = NEO$y, expand = FALSE),
    theme_bw(),
    theme(
      axis.title = element_blank())
  )

# creating the map
map.spec.bias <- 
ggplot() +  
  geom_tile(data=perc.spec.df, aes(x=x, y=y, fill=perc_specialists)) + 
  scale_fill_gradientn(
    colors = MetBrewer::met.brewer("Hiroshige"), 
    name = "%",
    limits = c(0,100), 
    na.value = "grey99") +
  coord_equal() +
  map.settings 

#save map

# Save in png
ggsave(here("output", "fig", "S3_spec_bias.png"),
       map.spec.bias,
       width = 5,
       height = 5,
       dpi = 300,
       units = "in")


# Save in pdf
ggsave(here("output", "fig", "S3_spec_bias.pdf"),
       map.spec.bias,
       width = 5,
       height = 5,
       dpi = 300,
       units = "in")

