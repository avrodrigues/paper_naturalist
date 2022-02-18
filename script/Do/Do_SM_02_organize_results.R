library(tidyverse)
library(ggplot2)
library(gridExtra)
library(ggridges)
library(paletteer)
library(dplyr)
library(cowplot)
library(FD)
library(adespatial)
library(here)
library(raster)


# Loading data ------------------------------------------------------------

# list of matrix results from sampling specialist table before classify 
# and filter occurrences which was identified by a specialist

output.files <- list.files(here("output"), full.names = T)
file.names <- grep("clean_eval", output.files, value = T)

# analyses for each specialist data set
spec_1 <- readRDS(file.names[[1]])
spec_10 <-  readRDS(file.names[[2]])
spec_5 <- readRDS(file.names[[3]])
spec_all <- readRDS(file.names[[4]])
spec_myrt <- readRDS(file.names[[5]])

# Plot results -------------------------------------------------------

# Change in Area in geographical space

spec_1_area    <- as_tibble(sapply(spec_1$area, function(x) 1 - x))
spec_5_area    <- as_tibble(sapply(spec_5$area, function(x) 1 - x))
spec_10_area   <- as_tibble(sapply(spec_10$area, function(x) 1 - x))
spec_all_area  <- as_tibble(sapply(spec_all$area, function(x) 1 - x))
spec_myrt_area <- as_tibble(sapply(spec_myrt$area, function(x) 1 - x))

group <- rep(c("spec_1", "spec_5", "spec_10", "spec_myrt", "spec_all"), 
              each = nrow(spec_1_area))
group <- factor(group,
                 levels = c("spec_myrt", "spec_1", "spec_5", "spec_10", "spec_all"))
sp.names <- rep(rownames(spec_1$area), 5)

area.df <- bind_rows(spec_1_area,    
                     spec_5_area,   
                     spec_10_area,
                     spec_myrt_area,
                     spec_all_area)

area.df <- tibble(area.df, group = group, sp.names)

# area as percentage
area.df <-
area.df %>% 
  mutate(r.geo.area = r.geo.area*100)


# Summary results ---------------------------------------------------------

area.df %>% 
  group_by(group) %>% 
  summarise(n.spp = n(),
            total.change = sum(r.geo.area == 100),
            low.change = sum(r.geo.area <= 20)/n.spp,
            high.change = sum(r.geo.area == 100)/n.spp,)


# plot: comparing the quality of lists of specialists ---------------------

col.set <- c("#e40064",
          "#01a238",
          "#832e88",
          "#0ddec9",
          "#ff7436")

col.set2 <- c("#f4eee5",
              "#deccb2",
              "#c9aa7f",
              "#78664c",
              "#282219")

leg.labels <- c("Provided by a specialist",
                "1% most frequent",
                "5% most frequent",
                "10% most frequent",
                "Lists united")


bin.geo.area.df <-
mutate(area.df, 
       bin.geo = as.character(cut(r.geo.area, 
                 breaks = seq(-1, 99, by = 20), 
                 labels =  c("0-19", "20-39", "40-59",
                             "60-79", "80-99"),
                 right = T)),
       bin.geo = ifelse(is.na(bin.geo), "100", bin.geo),
       )  %>% 
  group_by(group, bin.geo) %>% 
  summarise(count = n()) %>% 
  mutate(bin.geo = factor(bin.geo, levels = c("0-19", "20-39", "40-59",
                                              "60-79", "80-99", "100")))

(bin.geo.plot <- 
ggplot(bin.geo.area.df, aes(fill=group, y=count, x=bin.geo)) +
  geom_bar( position="dodge", stat="identity") +
  scale_fill_brewer(palette = "Paired", #values = col.set2,
                    name =  "Type of list of specialists used to clean the dataset",
                    labels = leg.labels) +
  
  scale_y_continuous(breaks = seq(0, 600, 100),
                     minor_breaks = seq(0, 650, 50),
                     limits = c(0,650)) +
  theme_bw() +
  labs(
    y = "Number of species",
    x = "Percentage of change in species area after cleanning
records not identified by specialists") +
  theme(legend.position = "top") +
    guides(fill=guide_legend(
      title.position = "top", 
      title.theme = element_text(hjust = 0.5, size = 9, face = "italic"),
      nrow=2, byrow=T))
)


# Save in png
ggsave(here("output", "fig", "S1_change_in_species_area.png"),
       bin.geo.plot,
       width = 6,
       height = 6,
       units = "in")
  
# Save in pdf
ggsave(here("output", "fig", "S1_change_in_species_area.pdf"),
       bin.geo.plot,
       width = 6,
       height = 6,
       units = "in")


# results based on composition --------------------------------------------




# CWM ---------------------------------------------------------------------

## preparing data

# bind composition and geographical coordinates and omit rows with NA

# spec.1
comp.xy.spec.1    <- na.omit(cbind(spec_1$comp$comp.BC, spec_1$site.coords))
comp.xy.spec.5    <- na.omit(cbind(spec_5$comp$comp.BC, spec_5$site.coords))
comp.xy.spec.10   <- na.omit(cbind(spec_10$comp$comp.BC, spec_10$site.coords))
comp.xy.spec.myrt <- na.omit(cbind(spec_myrt$comp$comp.BC, spec_myrt$site.coords))
comp.xy.spec.all  <- na.omit(cbind(spec_all$comp$comp.BC, spec_all$site.coords))


# get number of columns (all data has the same)
ncol.comp.xy <- ncol(comp.xy.spec.1)

# separate composition and coordinate data set
# composition
comp.spec.1    <- comp.xy.spec.1[,1:(ncol.comp.xy-2)]
comp.spec.5    <- comp.xy.spec.5[,1:(ncol.comp.xy-2)]
comp.spec.10   <- comp.xy.spec.10[,1:(ncol.comp.xy-2)]
comp.spec.myrt <- comp.xy.spec.myrt[,1:(ncol.comp.xy-2)]
comp.spec.all  <- comp.xy.spec.all[,1:(ncol.comp.xy-2)]

# coordinates
xy.spec.1 <- comp.xy.spec.1[,(ncol.comp.xy-1):ncol.comp.xy]
xy.spec.5 <- comp.xy.spec.5[,(ncol.comp.xy-1):ncol.comp.xy]
xy.spec.10 <- comp.xy.spec.10[,(ncol.comp.xy-1):ncol.comp.xy]
xy.spec.myrt <- comp.xy.spec.myrt[,(ncol.comp.xy-1):ncol.comp.xy]
xy.spec.all <- comp.xy.spec.all[,(ncol.comp.xy-1):ncol.comp.xy]


# trait data: area of species after cleaning (geographical space)
area.AC.spec.1    <- data.frame(area.AC = spec_1$area$r.geo.area)
area.AC.spec.5    <- data.frame(area.AC = spec_5$area$r.geo.area)
area.AC.spec.10   <- data.frame(area.AC = spec_10$area$r.geo.area)
area.AC.spec.myrt <- data.frame(area.AC = spec_myrt$area$r.geo.area)
area.AC.spec.all  <- data.frame(area.AC = spec_all$area$r.geo.area)

# set names for columns in composition data set
colnames(comp.spec.1    ) <- rownames(area.AC.spec.1)
colnames(comp.spec.5    ) <- rownames(area.AC.spec.5)
colnames(comp.spec.10   ) <- rownames(area.AC.spec.10)
colnames(comp.spec.myrt ) <- rownames(area.AC.spec.myrt)
colnames(comp.spec.all  ) <- rownames(area.AC.spec.all)

## Compute CWM
#      only in site with more than 3 species
more.3.spec.1 <- rowSums(comp.spec.1) > 3
cwm.spec.1    <- functcomp(area.AC.spec.1, comp.spec.1[more.3.spec.1,])

more.3.spec.5 <- rowSums(comp.spec.5) > 3
cwm.spec.5    <- functcomp(area.AC.spec.5, comp.spec.5[more.3.spec.5,])

more.3.spec.10 <- rowSums(comp.spec.10) > 3
cwm.spec.10   <- functcomp(area.AC.spec.10, comp.spec.10[more.3.spec.10,])

more.3.spec.myrt <- rowSums(comp.spec.myrt) > 3
cwm.spec.myrt <- functcomp(area.AC.spec.myrt, comp.spec.myrt[more.3.spec.myrt,])

more.3.spec.all <- rowSums(comp.spec.all) > 3
cwm.spec.all  <- functcomp(area.AC.spec.all, comp.spec.all[more.3.spec.all,])

# CWM in geographical space
r.cwm.spec.1    <- rasterFromXYZ(cbind(xy.spec.1[more.3.spec.1, ], (1-cwm.spec.1)*100))
r.cwm.spec.5    <- rasterFromXYZ(cbind(xy.spec.5[more.3.spec.5, ], (1-cwm.spec.5)*100))
r.cwm.spec.10   <- rasterFromXYZ(cbind(xy.spec.10[more.3.spec.10, ], (1-cwm.spec.10)*100))
r.cwm.spec.myrt <- rasterFromXYZ(cbind(xy.spec.myrt[more.3.spec.myrt, ], (1-cwm.spec.myrt)*100))
r.cwm.spec.all  <- rasterFromXYZ(cbind(xy.spec.all[more.3.spec.all,] , (1-cwm.spec.all)*100 ))

plot(r.cwm.spec.1)
plot(r.cwm.spec.5)
plot(r.cwm.spec.10)
plot(r.cwm.spec.myrt)
plot(r.cwm.spec.all)



# plot CWM and TBI ----------------------------------------------------------

library(viridis)
library(ggthemes)
library(rnaturalearth)
library(sf)

# defining neotropical region extent
NEO <- list(x = c(-120,-30),
            y = c(-60,35))

# defining geographic region
neo_coastline <- ne_coastline(scale = 50, returnclass = "sf")


map.label = c("1% most frequent",
              "5% most frequent",
              "10% most frequent",
              "Provided by a specialist",
              "Lists united")

map.settings <- 
  list(
    geom_sf(data = neo_coastline, size = 0.5),
    coord_sf(xlim = NEO$x, ylim = NEO$y, expand = FALSE),
    theme_bw(),
    theme(axis.title = element_blank())
  )

#### raster plot CWM ####
## spec.1

viridis.col <- "plasma"

cwm.df.spec.1 <- as.data.frame(as(r.cwm.spec.1, "SpatialPixelsDataFrame"))
colnames(cwm.df.spec.1) <- c("value", "x", "y")
# map
(r.map.cwm.spec.1 <- 
ggplot() +  
  geom_tile(data=cwm.df.spec.1, aes(x=x, y=y, fill=value)) + 
  scale_fill_viridis(option = viridis.col, name = "%", limits = c(0,100)) +
  labs(title = map.label[1]) +
  coord_equal() +
  map.settings
)
## spec.5
cwm.df.spec.5 <- as.data.frame(as(r.cwm.spec.5, "SpatialPixelsDataFrame"))
colnames(cwm.df.spec.5) <- c("value", "x", "y")
# map
r.map.cwm.spec.5 <- 
  ggplot() +  
  geom_tile(data=cwm.df.spec.5, aes(x=x, y=y, fill=value)) + 
  scale_fill_viridis(option = viridis.col, name = "%", limits = c(0,100)) +
  labs(title = map.label[2]) +
  coord_equal() +
  map.settings

## spec.10
cwm.df.spec.10 <- as.data.frame(as(r.cwm.spec.10, "SpatialPixelsDataFrame"))
colnames(cwm.df.spec.10) <- c("value", "x", "y")
# map
r.map.cwm.spec.10 <- 
  ggplot() +  
  geom_tile(data=cwm.df.spec.10, aes(x=x, y=y, fill=value)) + 
  scale_fill_viridis(option = viridis.col, name = "%", limits = c(0,100)) +
  labs(title = map.label[3]) +
  coord_equal() +
  map.settings

## spec.myrt
cwm.df.spec.myrt <- as.data.frame(as(r.cwm.spec.myrt, "SpatialPixelsDataFrame"))
colnames(cwm.df.spec.myrt) <- c("value", "x", "y")
# map
r.map.cwm.spec.myrt <- 
  ggplot() +  
  geom_tile(data=cwm.df.spec.myrt, aes(x=x, y=y, fill=value)) + 
  scale_fill_viridis(option = viridis.col, name = "%", limits = c(0,100)) +
  labs(title = map.label[4]) +
  coord_equal() +
  map.settings


## spec.all
cwm.df.spec.all <- as.data.frame(as(r.cwm.spec.all, "SpatialPixelsDataFrame"))
colnames(cwm.df.spec.all) <- c("value", "x", "y")
# map
r.map.cwm.spec.all <- 
  ggplot() +  
  geom_tile(data=cwm.df.spec.all, aes(x=x, y=y, fill=value)) + 
  scale_fill_viridis(
    option = viridis.col, 
    name = 
"Averange change (%) in the species
area of distribution after cleaning 
the occurrences not identified by 
specialists",
    limits = c(0,100)) +
  labs(title = map.label[5]) +
  coord_equal() +
  map.settings +
  theme(legend.position = "bottom", 
        legend.title = element_text(size = 9)) +
  guides(fill = guide_colourbar(
    title.position = "top", 
    #title.hjust = 0.5, 
    barwidth = 10,
    barheight = 1.5
    
  ))

# Map graph ---------------------------------------------------------------

no.legend <- list(theme_few(),
                  theme(legend.position = 'none', 
                        axis.title = element_blank(),
                        plot.margin = margin(0,0,20,0),
                        axis.text = element_blank(),
                        axis.ticks = element_blank()))

leg <- get_legend(r.map.cwm.spec.all)
  

plot_grid(leg)
cwm.5 <-
  plot_grid(r.map.cwm.spec.myrt + no.legend,
            r.map.cwm.spec.1    + no.legend,
            r.map.cwm.spec.5    + no.legend,
            r.map.cwm.spec.10   + no.legend, 
            r.map.cwm.spec.all  + no.legend,
            labels= "AUTO",
            ncol = 3) 
cwm.5.leg <-
ggdraw(cwm.5)  + 
  draw_plot(
    leg,
    scale = 0.5, 
    x = 0.32,
    y = -0.3
    )


# Save in png
ggsave(here("output", "fig", "S2_cwm.png"),
       cwm.5.leg,
       width = 9,
       height = 6,
       units = "in")




