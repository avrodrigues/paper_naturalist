
# load packages -----------------------------------------------------------

library(fasterize)
library(naturaList)
library(here)
library(raster)
library(sp)
library(sf)
library(rnaturalearth)
library(tidyverse)
library(tidymodels)
library(adespatial)
library(patchwork)
library(ragg)

# load data ---------------------------------------------------------------

tbi.resampled <- readRDS(here("output", "tbi_resampling_results.rds"))

occ.cl.files <- list.files(here("output"), pattern = "occ_cl", full.names = T)

occ.cl.list <- lapply(occ.cl.files, readRDS)
names(occ.cl.list) <- substr(occ.cl.files,101, nchar(occ.cl.files)-4)

# raster layer
# environmental data
env.files <- list.files(here("data", "env"), full.names = T)
env.stack <- stack(env.files)

env.df <- as.data.frame(env.stack)

# defining geographic region
neo_coastline <- ne_coastline(scale = 50, returnclass = "sf")
ext <- c(-120, -30, -60, 35)
names(ext) <- c("xmin", "xmax", "ymin", "ymax")
neo_coastline <- st_crop(st_geometry(neo_coastline), ext)
neo_coastline <- st_cast(neo_coastline, "MULTIPOLYGON")

top10.genus <- c("Calycolpus", "Campomanesia", "Eugenia", "Myrceugenia",
                 "Myrcia", "Myrcianthes", "Myrciaria", "Neomitranthes", 
                 "Plinia", "Psidium")

# TBI - observed values ------------------------------------------------------


l.eval.spec.all <- clean_eval(occ.cl.list$occ_cl_spec_all, 
                              neo_coastline, 
                              r = env.stack[[1]])

comp.bc <- as_tibble(l.eval.spec.all$comp$comp.BC)
comp.ac <- as_tibble(l.eval.spec.all$comp$comp.AC)

site.coords <- l.eval.spec.all$site.coords

# composition by genus
genus.comp <- 
  lapply(top10.genus, function(genus) {
    #before cleaning
    bc <- comp.bc %>% 
      dplyr::select(starts_with(genus)) 
    #after cleaning
    ac <- comp.ac %>% 
      dplyr::select(starts_with(genus))
    
    list(bc = bc, ac = ac)
  })

names(genus.comp) <- top10.genus

# calculate Temporal beta-diversity Index (% change)
tbi.list <- lapply(genus.comp, function(x){
  keep.site <- rowSums(x$bc, na.rm = T) > 0
  
  mtx_bc <- as.matrix(x$bc[keep.site,])
  mtx_ac <- as.matrix(x$ac[keep.site,])
  
  tbi.genus <- TBI(mtx_bc, mtx_ac)  
  
  tbi.all.sites <- rep(NA, length(keep.site))
  tbi.all.sites[keep.site] <- tbi.genus$TBI
  
  tbi.all.sites
})


# summary of TBI resampling results ---------------------------------------


res.mean.sd <- vector('list', 10)
names(res.mean.sd) <- top10.genus


for(i in seq_along(tbi.resampled)){
  
  mean.vec <- rowMeans(tbi.resampled[[i]], na.rm = T)
  sd.vec <- apply(tbi.resampled[[i]], 1, sd,  na.rm = T)
  
  res.mean.sd[[i]] <- data.frame(mean = mean.vec, sd = sd.vec)
}


# comparing observed vs resampled values ----------------------------------


z.score.genus <- vector("list", 10)
names(z.score.genus) <- top10.genus

for(i in 1:10){
  
  z.score.genus[[i]] <- 
    (tbi.list[[i]] - res.mean.sd[[i]]$mean) / res.mean.sd[[i]]$sd
}


hist(z.score.genus$Psidium)

z.df <-
bind_rows(z.score.genus) %>% 
  pivot_longer(cols = Calycolpus:Psidium , names_to = "genus", values_to = "z") %>% 
  mutate(genus = as.factor(genus)) %>% 
  drop_na()


ggplot(z.df, aes(group = genus)) +
  geom_histogram(aes(x = z)) +
  facet_wrap("genus", scales = "free",  ncol = 2)

ggsave(
  here("output", "z-score_TBI.png")
)


# modeling TBI ~ altitude -------------------------------------------------

# Organize TBI results
tbi.list2 <-
  lapply(res.mean.sd, function(x){
    bind_cols(altitude = env.df$Altitude_neotropico, tbi = x$mean)
  })


tbi.resampled.df <- 
  bind_rows(tbi.list2) %>% 
  mutate(genus = rep(names(tbi.list2), each = nrow(tbi.list2[[1]]))) %>% 
  filter(!is.na(tbi))

# preparing model fit 
lm_mod <- 
  linear_reg() %>% 
  set_engine("lm")

tbi_fitting <- function(data){
  lm_mod %>% 
    fit(tbi ~ altitude, 
        data = data) 
}

tbi.nested <- 
  tbi.resampled.df %>% 
  group_by(genus) %>% 
  nest() 

# model fit
tbi.lm <- 
  tbi.nested %>% 
  mutate(lm = map(data, tbi_fitting)) 

# organize results of model fit
tbi.lm.df <- 
  bind_rows(lapply(tbi.lm$lm, tidy)) %>% 
  mutate(genus = rep(tbi.nested$genus, each = 2),
         ci = std.error*1.96) 

# Figures of change in Beta Diversity (resampling) ----------------------------
beta.col <- "#7f226f"

clean.cols <- c(bc = "#e1b74c", ac = "#198c8c")
label.cols <- c(bc = "Before cleaning", ac ="After cleaning")
facet_label <- c("Intercept", "Slope")
names(facet_label) <- unique(tbi.lm.df$term)

(
  fig_a_tbi <- 
    ggplot(tbi.lm.df, 
           aes(
             x = estimate,
             y = factor(genus, 
                        levels =  c("Calycolpus", "Campomanesia", "Eugenia", 
                                    "Myrceugenia", "Myrcia", "Myrcianthes", 
                                    "Myrciaria", "Neomitranthes", "Plinia", 
                                    "Psidium" )[10:1])
           )
    ) +
    geom_errorbar(aes(xmin=estimate-ci, xmax=estimate+ci, 
                      width = 0.3), color = beta.col, size = 1) +
    geom_vline(xintercept = 0, linetype = 3) +
    geom_point(size = 4, alpha = 0.5, color = beta.col) +
    scale_color_manual(
      values = beta.col,
      name = "", 
      labels = label.cols) +
    facet_wrap(
      "term", 
      scales = "free_x", 
      labeller = labeller(term = facet_label)) +
    scale_x_continuous(n.breaks = 4) +
    theme_bw() +
    labs(title = "Coefficients of regression") +
    theme(
      plot.title = element_text(family = "Inter", size = 16),
      axis.title = element_blank(),
      legend.position = "bottom", 
      strip.background = element_rect(fill = "grey95"),
      strip.text = element_text(family = "Inter", size = 10),
      axis.text.y = element_text(family = "Inter", size = 12, face = "italic"),
      axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
      text = element_text(family = "Inter"), 
      plot.margin = margin(r = 10)
    )
)

fig_b_tbi <- 
  ggplot(tbi.resampled.df, aes(x = altitude, y = tbi)) +
  geom_smooth(method = "lm", fill = beta.col, color = beta.col) +
  scale_y_continuous(n.breaks = 4) + #, limits = c(0,1)
  facet_wrap("genus", scales = "free_y", ncol = 2) +
  theme_bw() +
  labs(
    title = "Linear trend", 
    x = "Altitude", 
    y = "Change in sites' Beta-diversity") +
  theme(
    plot.title = element_text(family = "Inter", size = 16),
    legend.position = "bottom",
    strip.background = element_rect(fill = "grey95"),
    strip.text = element_text(family = "Inter", size = 10, face = "italic"),
    axis.text.y = element_text(family = "Inter", size = 8),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    text = element_text(family = "Inter", size = 12), 
    panel.grid.minor = element_blank()
  )


### Final figure beta diversity

fig_tbi <- fig_a_tbi + fig_b_tbi +
  plot_layout(widths = c(0.8, 1 )) +
  plot_annotation(tag_levels = c("A", "B"))

agg_png(
  here("output", "fig", "ms_v5", "extra_03_beta_div_resampled.png"),
  width = 10,
  height = 7.5,
  units = "in",
  res = 300
)
fig_tbi
dev.off()


