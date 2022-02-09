
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


# load data ---------------------------------------------------------------

occ.cl.files <- list.files(here("output"), pattern = "occ_cl", full.names = T)

occ.cl.list <- lapply(occ.cl.files, readRDS)
names(occ.cl.list) <- substr(occ.cl.files,101, nchar(occ.cl.files)-4)


# summary of classification -----------------------------------------------

occ.cl.list$occ_cl_spec_all %>% 
  group_by(naturaList_levels) %>% 
  summarise(count = n()) %>% 
  mutate(
    total = sum(count),
    percent = count/total*100)

occ.cl.list$occ_cl_spec_myrt %>% 
  group_by(naturaList_levels) %>% 
  summarise(count = n()) %>% 
  mutate(
    total = sum(count),
    percent = count/total*100)

occ.cl.list$occ_cl_spec_1 %>% 
  group_by(naturaList_levels) %>% 
  summarise(count = n()) %>% 
  mutate(
    total = sum(count),
    percent = count/total*100)


# raster and geographic region -----------------------------------------------

# raster layer
# environmental data
env.files <- list.files(here("data", "env"), full.names = T)
env.stack <- stack(env.files)

# defining geographic region
neo_coastline <- ne_coastline(scale = 50, returnclass = "sf")
ext <- c(-120, -30, -60, 35)
names(ext) <- c("xmin", "xmax", "ymin", "ymax")
neo_coastline <- st_crop(st_geometry(neo_coastline), ext)
neo_coastline <- st_cast(neo_coastline, "MULTIPOLYGON")



# clean_eval --------------------------------------------------------------


l.eval.spec.all <- clean_eval(occ.cl.list$occ_cl_spec_all, 
                            neo_coastline, 
                            r = env.stack)


# composition by genus ----------------------------------------------------

species.names.df <- 
tibble(species = row.names(l.eval.spec.all$area),
       genus = word(species))

top.genus <- 
  species.names.df %>% 
  group_by(genus) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

top10.genus <- 
  top.genus %>% 
  slice_head(n = 10) %>% 
  dplyr::select(genus) 

comp.bc <- as_tibble(l.eval.spec.all$comp$comp.BC)
comp.ac <- as_tibble(l.eval.spec.all$comp$comp.AC)

site.coords <- l.eval.spec.all$site.coords

# composition by genus
genus.comp <- 
  lapply(top10.genus$genus, function(genus) {
    #before cleaning
    bc <- comp.bc %>% 
      dplyr::select(starts_with(genus)) 
    #after cleaning
    ac <- comp.ac %>% 
      dplyr::select(starts_with(genus))
    
    list(bc = bc, ac = ac)
})

names(genus.comp) <- top10.genus$genus



# evaluate effects on richness --------------------------------------------

# enviromental gradient
grad.df <- as.data.frame(env.stack) %>% 
  setNames(c("altitude", "temp_mean", "prec_annual",
             "prec_season", "prec_dry_quarter", "temp_season")) 

# data frame of richness and gradient for each genus
l.rich.df <- lapply(genus.comp, function(x){
  rich.bc <- rowSums(x$bc)
  rich.ac <- rowSums(x$ac)
  
  rich.df <- bind_cols(
    grad.df, rich.bc = rich.bc, rich.ac = rich.ac
  )
  
  rich.df
})


# model fit 
lm_mod <- 
  linear_reg() %>% 
  set_engine("lm")

lm_rich_genus <- lapply(l.rich.df, function(genus){
  lm_fit_bc <- 
    lm_mod %>% 
    fit(rich.bc ~ altitude, 
        data = genus %>% 
          filter(rich.bc > 0))
  
  lm_fit_ac <- 
    lm_mod %>% 
    fit(rich.ac ~ altitude, 
        data = genus %>% 
          filter(rich.ac > 0))
  
  bind_rows(
    mutate(tidy(lm_fit_bc), cleaning = "bc"),
    mutate(tidy(lm_fit_ac), cleaning = "ac")
  )
})

coef_df <- 
bind_rows(lm_rich_genus) %>% 
  mutate(
    cleaning = factor(cleaning, levels = c("bc", "ac")),
    genus = factor(rep(names(lm_rich_genus), 
                       each = nrow(lm_rich_genus$Eugenia))),
    ci = std.error*1.96)


# Figures Richness --------------------------------------------------------
library(ragg)
library(patchwork)

clean.cols <- c(bc = "#e1b74c", ac = "#198c8c")
label.cols <- c(bc = "Before cleaning", ac ="After cleaning")


# Figure A: Difference between coefficients
facet_label <- c("Intercept", "Slope")
names(facet_label) <- unique(coef_df$term)

fig_a_rich <- 
ggplot(coef_df, aes(y = estimate, 
                    x = factor(genus, levels(genus)[10:1]), 
                    color = cleaning)) +
  geom_errorbar(position=position_dodge(0.7), 
                aes(ymin=estimate-ci, ymax=estimate+ci, 
                    width = 0.65),  size = 1) +
  geom_point(position=position_dodge(0.7), size = 4, alpha = 0.5) +
  scale_color_manual(
    values = clean.cols,
    name = "", 
    labels = label.cols) +
  facet_wrap(
    "term", 
    scales = "free_x", 
    labeller = labeller(term = facet_label)) +
  scale_y_continuous(n.breaks = 5) +
  theme_bw() +
  labs(title = "Coefficients of regression") +
  coord_flip() +
  theme(
    plot.title = element_text(family = "Inter", size = 16),
    axis.title = element_blank(),
    legend.position = "bottom", 
    legend.text = element_text(family = "Inter", size = 10),
    strip.background = element_rect(fill = "grey95"),
    strip.text = element_text(family = "Inter", size = 10),
    axis.text.y = element_text(family = "Inter", size = 12, face = "italic"),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1)
  )



# Figure B: Difference between regression lines trend
rich.df.long <- bind_rows(l.rich.df) %>% 
  mutate(
    genus = rep(names(l.rich.df), each = nrow(l.rich.df[[1]]))
  ) %>% 
  pivot_longer(rich.bc:rich.ac, names_to = "cleaning", values_to = "richness") %>% 
  mutate(
    cleaning = str_remove_all(cleaning, "rich."),
    cleaning = factor(cleaning, levels = c("bc", "ac")))

fig_b_rich <- 
rich.df.long %>% 
  filter(richness > 0) %>% 
  ggplot(
    aes(x = altitude, y = richness, 
        color = cleaning, fill = cleaning)
  ) +
  geom_smooth(method = "lm") +
  scale_color_manual(
    values = clean.cols,
    name = "", 
    labels = label.cols) +
  scale_fill_manual(
    values = clean.cols,
    name = "", 
    labels = label.cols) +
    scale_y_continuous(n.breaks = 4) +
  facet_wrap("genus", scales = "free_y", ncol = 2) +
  theme_bw() +
    labs(title = "Linear trend", x = "Altitude", y = "Richness") +
  theme(
    plot.title = element_text(family = "Inter", size = 16),
    legend.position = "bottom",
    legend.text = element_text(family = "Inter", size = 10),
    strip.background = element_rect(fill = "grey95"),
    strip.text = element_text(family = "Inter", size = 10, face = "italic"),
    axis.text.y = element_text(family = "Inter", size = 8),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    text = element_text(family = "Inter", size = 12), 
    panel.grid.minor = element_blank()
  )


### Final figure richness

fig_rich <- fig_a_rich + fig_b_rich +
  plot_layout(widths = c(0.8, 1 )) +
  plot_annotation(tag_levels = c("A", "B"))

agg_png(
  here("output", "fig", "ms_v5", "02_richness.png"),
  width = 10,
  height = 8,
  units = "in",
  res = 300
)
fig_rich
dev.off()



# evaluate effects on beta diversity ---------------------------------------

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

# Organize TBI results
tbi.list2 <-
lapply(tbi.list, function(x){
  bind_cols(grad.df, tbi = x)
})


tbi.df <- 
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
  tbi.df %>% 
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


# Figures of change in Beta Diversity -------------------------------------
beta.col <- "#7f226f"

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
    values = clean.cols,
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
ggplot(tbi.df, aes(x = altitude, y = tbi)) +
  geom_smooth(method = "lm", fill = beta.col, color = beta.col) +
  scale_y_continuous(n.breaks = 4) +
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
  here("output", "fig", "ms_v5", "03_beta_div.png"),
  width = 10,
  height = 7.5,
  units = "in",
  res = 300
)
fig_tbi
dev.off()





