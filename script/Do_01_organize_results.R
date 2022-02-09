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



# Loading data ------------------------------------------------------------

# list of matrix results from sampling specialist table before classify 
# and filter occurrences which was identified by a specialist

output.files <- list.files(here("output"), full.names = T)
file.names <- grep(".rds", output.files, value = T)

# analyses for each specialist data set
spec_1 <- readRDS(file.names[[1]])
spec_10 <-  readRDS(file.names[[2]])
spec_5 <- readRDS(file.names[[3]])
spec_all <- readRDS(file.names[[4]])
spec_myrt <- readRDS(file.names[[5]])

# Plot results -------------------------------------------------------

# Area in geographical and environmental space

spec_1_area    <- as_tibble(sapply(spec_1$area, function(x) x))
spec_5_area    <- as_tibble(sapply(spec_5$area, function(x) x))
spec_10_area   <- as_tibble(sapply(spec_10$area, function(x) x))
spec_all_area  <- as_tibble(sapply(spec_all$area, function(x) x))
spec_myrt_area <- as_tibble(sapply(spec_myrt$area, function(x) x))

group <- rep(c("spec_1", "spec_5", "spec_10", "spec_myrt", "spec_all"), 
              each = nrow(spec_1_area))
group <- factor(group,
                 levels = c("spec_myrt", "spec_1", "spec_5", "spec_10", "spec_all"))
sp.names <- rep(names(spec_1$area$r.geo.area), 5)

area.df <- bind_rows(spec_1_area,    
                     spec_5_area,   
                     spec_10_area,
                     spec_myrt_area,
                     spec_all_area)

area.df <- tibble(area.df, group = group, sp.names)

# area as percentage
area.df <-
area.df %>% 
  mutate(r.geo.area = r.geo.area*100,
         r.env.area = r.env.area*100)


# Summary results ---------------------------------------------------------

area.df %>% 
  group_by(group) %>% 
  summarise(n.spp = n(),
            zero.sum = sum(r.geo.area == 0),
            geo.n.spp.0 = sum(r.geo.area == 0)/n.spp,
            geo.n.spp.80 = sum(r.geo.area > 80)/n.spp,
            env.n.spp.0 = sum(r.env.area == 0)/n.spp,
            env.n.spp.80 = sum(r.env.area > 80)/n.spp)


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
                 breaks = seq(0, 100, by = 20), 
                 labels =  c("1-20", "21-40", "41-60",
                             "61-80", "81-100"),
                 right = T)),
       bin.geo = ifelse(is.na(bin.geo), "0", bin.geo),
       ) %>% 
  group_by(group, bin.geo) %>% 
  summarise(count = n()) 

(bin.geo.plot <- 
ggplot(bin.geo.area.df, aes(fill=group, y=count, x=bin.geo)) +
  geom_bar( position="dodge", stat="identity") +
  scale_fill_brewer(palette = "Paired", #values = col.set2,
                    name =  "Type of List of Specialist",
                    labels = leg.labels) +
  
  scale_y_continuous(breaks = seq(0, 600, 100),
                     minor_breaks = seq(0, 650, 50),
                     limits = c(0,650)) +
  theme_bw() +
  labs(title = "Geograpical space",
       y = "Number of species",
       x = "Remaning area after cleaning occurrences\n(classes)") +
  theme(#axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")
)

bin.env.area.df <-
  mutate(area.df, 
         bin.env = as.character(cut(r.env.area, 
                                    breaks = seq(0, 100, by = 20), 
                                    labels =  c("1-20", "21-40", "41-60",
                                                "61-80", "81-100"),
                                    right = T)),
         bin.env = ifelse(is.na(bin.env), "0", bin.env),
  ) %>% 
  group_by(group, bin.env) %>% 
  summarise(count = n()) 

bin.env.plot <-
ggplot(bin.env.area.df, aes(fill=group, y=count, x=bin.env)) +
  geom_bar( position="dodge", stat="identity") +
  scale_fill_brewer(palette = "Paired",
                    name =  "Type of List of Specialist",
                    labels = leg.labels) +
  scale_y_continuous(breaks = seq(0, 600, 100),
                     minor_breaks = seq(0, 650, 50),
                     limits = c(0,650)) +
  theme_bw() +
  labs(title = "Environmental space",
       y = "",
       x = "Remaning area after cleaning occurrences\n(classes)") +
  theme(#axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")


# get legend as a single plot
legend <- get_legend(bin.geo.plot)
# delete legend in geo plot
bin.geo.plot.2 <- bin.geo.plot + theme(legend.position='none')

main.fig <- plot_grid(bin.geo.plot.2, bin.env.plot, ncol = 2,
                      labels = c("A", "B"))

# Figure
binned.plot <- 
  plot_grid(main.fig,  legend, 
    nrow = 2, rel_heights = c(1,  0.2))

# Save in png
ggsave(here("output", "fig", "02_bin_plot2.png"),
       binned.plot,
       width = 10,
       height = 6,
       units = "in")
  
# Save in png
ggsave(here("output", "fig", "02_bin_plot2.pdf"),
       binned.plot,
       width = 10,
       height = 6,
       units = "in")

# results from richness ---------------------------------------------------
#correlation
cor.rich.1    <-  cor(spec_1$rich$rich.AC,
                      spec_1$rich$rich.BC, use = "na.or.complete")

cor.rich.5    <-  cor(spec_5$rich$rich.AC,
                      spec_5$rich$rich.BC, use = "na.or.complete")

cor.rich.10   <-  cor(spec_10$rich$rich.AC,
                      spec_10$rich$rich.BC, use = "na.or.complete")

cor.rich.myrt <-  cor(spec_myrt$rich$rich.AC,
                      spec_myrt$rich$rich.BC, use = "na.or.complete")

cor.rich.all  <-  cor(spec_all$rich$rich.AC,
                      spec_all$rich$rich.BC, use = "na.or.complete")

cor.results <- c(cor.rich.1,
                 cor.rich.5 ,
                 cor.rich.10,
                 cor.rich.myrt,
                 cor.rich.all )


# results based on composition --------------------------------------------

library(FD)
library(raster)


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
r.cwm.spec.1    <- rasterFromXYZ(cbind(xy.spec.1[more.3.spec.1, ], cwm.spec.1*100))
r.cwm.spec.5    <- rasterFromXYZ(cbind(xy.spec.5[more.3.spec.5, ], cwm.spec.5*100))
r.cwm.spec.10   <- rasterFromXYZ(cbind(xy.spec.10[more.3.spec.10, ], cwm.spec.10*100))
r.cwm.spec.myrt <- rasterFromXYZ(cbind(xy.spec.myrt[more.3.spec.myrt, ], cwm.spec.myrt*100))
r.cwm.spec.all  <- rasterFromXYZ(cbind(xy.spec.all[more.3.spec.all,] , cwm.spec.all*100 ))

plot(r.cwm.spec.1)
plot(r.cwm.spec.5)
plot(r.cwm.spec.10)
plot(r.cwm.spec.myrt)
plot(r.cwm.spec.all)


# Temporal Beta-diversity Index (TBI) -------------------------------------
library(adespatial)

## this method measure composition change in a site (raster cell) between to
## times: t1 and t2. We consider, the t1 as the composition before the cleaning
## of occurrences not identified by specialists and the t2 as the composition after 
## the cleaning. 

# composition after cleaning
comp.AC.spec.1    <- na.omit(spec_1$comp$comp.AC   )
comp.AC.spec.5    <- na.omit(spec_5$comp$comp.AC   )
comp.AC.spec.10   <- na.omit(spec_10$comp$comp.AC  )
comp.AC.spec.myrt <- na.omit(spec_myrt$comp$comp.AC)
comp.AC.spec.all  <- na.omit(spec_all$comp$comp.AC )

### TBI spec.1
# exclude site with less than 3 species in before cleaning
less.3 <- rowSums(comp.spec.1) > 3
# TBI
tbi.spec.1 <- TBI(comp.spec.1[less.3,], comp.AC.spec.1[less.3,])
# TBI in geographical space
r.tbi.spec.1 <- rasterFromXYZ(cbind(xy.spec.1[less.3,], tbi.spec.1$TBI*100))


### TBI spec.5
# exclude site with less than 3 species in before cleaning
less.3 <- rowSums(comp.spec.5) > 3
# TBI
tbi.spec.5 <- TBI(comp.spec.5[less.3,], comp.AC.spec.5[less.3,])
# TBI in geographical space
r.tbi.spec.5 <- rasterFromXYZ(cbind(xy.spec.5[less.3,], tbi.spec.5$TBI*100))


### TBI spec.10
# exclude site with less than 3 species in before cleaning
less.3 <- rowSums(comp.spec.10) > 3
# TBI
tbi.spec.10 <- TBI(comp.spec.10[less.3,], comp.AC.spec.10[less.3,])
# TBI in geographical space
r.tbi.spec.10 <- rasterFromXYZ(cbind(xy.spec.10[less.3,], tbi.spec.10$TBI*100))


### TBI spec.myrt
# exclude site with less than 3 species in before cleaning
less.3 <- rowSums(comp.spec.myrt) > 3
# TBI
tbi.spec.myrt <- TBI(comp.spec.myrt[less.3,], comp.AC.spec.myrt[less.3,])
# TBI in geographical space
r.tbi.spec.myrt <- rasterFromXYZ(cbind(xy.spec.myrt[less.3,], tbi.spec.myrt$TBI*100))


### TBI spec.all
# exclude site with less than 3 species in before cleaning
less.3 <- rowSums(comp.spec.all) > 3
# TBI
tbi.spec.all <- TBI(comp.spec.all[less.3,], comp.AC.spec.all[less.3,])
# TBI in geographical space
r.tbi.spec.all <- rasterFromXYZ(cbind(xy.spec.all[less.3,], tbi.spec.all$TBI*100))


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
cwm.df.spec.1 <- as.data.frame(as(r.cwm.spec.1, "SpatialPixelsDataFrame"))
colnames(cwm.df.spec.1) <- c("value", "x", "y")
# map
r.map.cwm.spec.1 <- 
ggplot() +  
  geom_tile(data=cwm.df.spec.1, aes(x=x, y=y, fill=value)) + 
  scale_fill_viridis(name = "%", limits = c(0,100)) +
  labs(title = map.label[1]) +
  coord_equal() +
  map.settings

## spec.5
cwm.df.spec.5 <- as.data.frame(as(r.cwm.spec.5, "SpatialPixelsDataFrame"))
colnames(cwm.df.spec.5) <- c("value", "x", "y")
# map
r.map.cwm.spec.5 <- 
  ggplot() +  
  geom_tile(data=cwm.df.spec.5, aes(x=x, y=y, fill=value)) + 
  scale_fill_viridis(name = "%", limits = c(0,100)) +
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
  scale_fill_viridis(name = "%", limits = c(0,100)) +
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
  scale_fill_viridis(name = "%", limits = c(0,100)) +
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
  scale_fill_viridis(name = "%", limits = c(0,100)) +
  labs(title = map.label[5]) +
  coord_equal() +
  map.settings


#### raster plot TBI ####
## spec.1
tbi.df.spec.1 <- as.data.frame(as(r.tbi.spec.1, "SpatialPixelsDataFrame"))
colnames(tbi.df.spec.1) <- c("value", "x", "y")
# map
r.map.tbi.spec.1 <- 
  ggplot() +  
  geom_tile(data=tbi.df.spec.1, aes(x=x, y=y, fill=value)) + 
  scale_fill_viridis(name = "%", limits = c(0,100)) +
  labs(title = map.label[1]) +
  coord_equal() +
  map.settings


## spec.5
tbi.df.spec.5 <- as.data.frame(as(r.tbi.spec.5, "SpatialPixelsDataFrame"))
colnames(tbi.df.spec.5) <- c("value", "x", "y")
# map
r.map.tbi.spec.5 <- 
  ggplot() +  
  geom_tile(data=tbi.df.spec.5, aes(x=x, y=y, fill=value)) + 
  scale_fill_viridis(name = "%", limits = c(0,100)) +
  labs(title = map.label[2]) +
  coord_equal() +
  map.settings

## spec.10
tbi.df.spec.10 <- as.data.frame(as(r.tbi.spec.10, "SpatialPixelsDataFrame"))
colnames(tbi.df.spec.10) <- c("value", "x", "y")
# map
r.map.tbi.spec.10 <- 
  ggplot() +  
  geom_tile(data=tbi.df.spec.10, aes(x=x, y=y, fill=value)) + 
  scale_fill_viridis(name = "%", limits = c(0,100)) +
  labs(title = map.label[3]) +
  coord_equal() +
  map.settings

## spec.myrt
tbi.df.spec.myrt <- as.data.frame(as(r.tbi.spec.myrt, "SpatialPixelsDataFrame"))
colnames(tbi.df.spec.myrt) <- c("value", "x", "y")
# map
r.map.tbi.spec.myrt <- 
  ggplot() +  
  geom_tile(data=tbi.df.spec.myrt, aes(x=x, y=y, fill=value)) + 
  scale_fill_viridis(name = "%", limits = c(0,100)) +
  labs(title = map.label[4]) +
  coord_equal() +
  map.settings

## spec.all
tbi.df.spec.all <- as.data.frame(as(r.tbi.spec.all, "SpatialPixelsDataFrame"))
colnames(tbi.df.spec.all) <- c("value", "x", "y")
# map
r.map.tbi.spec.all <- 
  ggplot() +  
  geom_tile(data=tbi.df.spec.all, aes(x=x, y=y, fill=value)) + 
  scale_fill_viridis(name = "%", limits = c(0,100)) +
  labs(title = map.label[5]) +
  coord_equal() +
  map.settings



# Correlation between CWM and TBI -----------------------------------------

# round coordinates
cwm.df.spec.1[,2:3] <- round(cwm.df.spec.1[,2:3]   ,2)
tbi.df.spec.1[,2:3] <- round(tbi.df.spec.1[,2:3]   ,2)

cwm.df.spec.5[,2:3] <- round(cwm.df.spec.5[,2:3]   ,2)
tbi.df.spec.5[,2:3] <- round(tbi.df.spec.5[,2:3]   ,2)

cwm.df.spec.10[,2:3] <- round(cwm.df.spec.10[,2:3]   ,2)
tbi.df.spec.10[,2:3] <- round(tbi.df.spec.10[,2:3]   ,2)

cwm.df.spec.myrt[,2:3] <- round(cwm.df.spec.myrt[,2:3]   ,2)
tbi.df.spec.myrt[,2:3] <- round(tbi.df.spec.myrt[,2:3]   ,2)

cwm.df.spec.all[,2:3] <- round(cwm.df.spec.all[,2:3]   ,2)
tbi.df.spec.all[,2:3] <- round(tbi.df.spec.all[,2:3]   ,2)


df.cor.spec.1    <- right_join(cwm.df.spec.1, tbi.df.spec.1, by = c("x", "y"))
df.cor.spec.5    <- right_join(cwm.df.spec.5, tbi.df.spec.5, by = c("x", "y"))
df.cor.spec.10   <- right_join(cwm.df.spec.10, tbi.df.spec.10, by = c("x", "y"))
df.cor.spec.myrt <- right_join(cwm.df.spec.myrt, tbi.df.spec.myrt, by = c("x", "y"))
df.cor.spec.all  <- right_join(cwm.df.spec.all, tbi.df.spec.all, by = c("x", "y"))

cor.spec.1    <- cor.test(df.cor.spec.1$value.x, df.cor.spec.1$value.y, use = "na.or.complete")
cor.spec.5    <- cor.test(df.cor.spec.5$value.x, df.cor.spec.5$value.y, use = "na.or.complete")
cor.spec.10   <- cor.test(df.cor.spec.10$value.x, df.cor.spec.10$value.y, use = "na.or.complete")
cor.spec.myrt <- cor.test(df.cor.spec.myrt$value.x, df.cor.spec.myrt$value.y, use = "na.or.complete")
cor.spec.all  <- cor.test(df.cor.spec.all$value.x, df.cor.spec.all$value.y, use = "na.or.complete")

cor.vec <-  
  c(cor.spec.myrt$estimate ,
    cor.spec.1$estimate    ,
    cor.spec.5$estimate,
    cor.spec.10$estimate   ,
    cor.spec.all$estimate  )
# Map graph ---------------------------------------------------------------
library(grid)

no.legend <- list(theme_few(),
                  theme(legend.position = 'none', 
                        title = element_blank(),
                        plot.margin = margin(0,0,20,0),
                        axis.text = element_blank(),
                        axis.ticks = element_blank()))

tbi.5 <- 
  plot_grid(r.map.tbi.spec.myrt + no.legend,
            r.map.tbi.spec.1    + no.legend,
            r.map.tbi.spec.5    + no.legend,
            r.map.tbi.spec.10   + no.legend, 
            r.map.tbi.spec.all  + no.legend,
            ncol = 1)

cwm.5 <- 
  plot_grid(r.map.cwm.spec.myrt + no.legend,
            r.map.cwm.spec.1    + no.legend,
            r.map.cwm.spec.5    + no.legend,
            r.map.cwm.spec.10   + no.legend,
            r.map.cwm.spec.all  + no.legend,
            ncol = 1)

legend.cwm <- plot_grid(NA,
                        NA,
                        NA,
                        NA, 
                        get_legend(r.map.cwm.spec.myrt +
                                     theme_bw() +
                                     theme(legend.text = element_blank())),
                        ncol = 1)


legend.tbi <-  plot_grid(get_legend(r.map.tbi.spec.myrt),
                         NA,
                         NA,
                         NA,
                         NA,
                         ncol = 1)

cwm_tbi <- 
  plot_grid(cwm.5, tbi.5, 
            NA, legend.tbi, 
            ncol = 4, rel_widths = c(1, 1, 0.2, 0.2))

## texts for plot

leg.labels
txt.AE <- LETTERS[1:5]
correlation <- paste0("r = ", round(cor.vec, 2))
## labels position
position.ref <- function(ref, n.pos, dir = "neg", dist){
  d <- ifelse(dir == "neg", -1, 1)
  
  res <- vector("numeric", n.pos)
  res[1] <- ref
  
  for(i in 2:length(res)){
    res[i] <- res[i-1] + d*dist
  }
  res
}

lab.x <- rep(0.14, 5)
lab.y <- position.ref(0.873, 5, dist = 0.19)

letters.x <- rep(0.10, 5)
letters.y <- position.ref(0.95, 5, dist = 0.19)

cor.x <- rep(0.185, 5)
cor.y <- position.ref(0.805, 5, dist = 0.19)

map_graph <- 
ggdraw() +
  # main plot
  draw_plot(cwm_tbi,
            x = 0.55, y = 0.01, 
            width = 0.8, height = 0.95,
            hjust = 0.5, vjust = 0) +
  # X labels
  draw_text(c("Average Remaining Area", "Change in composition"),
            x = c(0.32, 0.65), 
            y = rep(0.975, 2),
            size = 14,
            fontface = 'bold') +
  # Y labels
  draw_text(leg.labels, 
            x = lab.x,
            y = lab.y,
            size = 14,
            fontface = 'bold',
            angle = 90) +
  # plot letters
  draw_text(txt.AE, 
            x = letters.x,
            y = letters.y,
            size = 20,
            fontface = 'bold') +
  # correlation
  draw_text(correlation, 
            x = cor.x, 
            y = cor.y,
            size = 10,
            hjust = 0)

  
# Save in pdf
ggsave(here("output", "fig", "03_cwm_tbi2.pdf"),
       map_graph,
       width = 9,
       height = 17,
       units = "in")


# Save in png
ggsave(here("output", "fig", "03_cwm_tbi2.png"),
       map_graph,
       width = 9,
       height = 17,
       units = "in")


