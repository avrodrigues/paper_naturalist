library(here)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(ggridges)
library(paletteer)
library(dplyr)
source(here("function", "summ_results_area.R"))
source(here("function", "summ_results_richness.R"))

output.files <- list.files(here("output"), full.names = T)

file.names <- grep("RData", output.files, value = T)

# Loading data ------------------------------------------------------------

# list of matrix results from sampling specilist table before classify 
# and filter occurrences which was identified by a specialist

load(file.names[[1]])
spec_10 <- list.env.area.std

load(file.names[[2]])
spec_1 <- list.env.area.std

load(file.names[[3]])
spec_5 <- list.env.area.std

load(file.names[[4]])
spec_Myrt <- list.env.area.std

load(file.names[[5]])
spec_all <- list.env.area.std

# area and richness of full occurrences dataset
all.occ <- readRDS(here("output", "all_occ_classified.rds"))

rm(list.env.area.std)
# Summarize results -------------------------------------------------------

# results from area
# spec_1
area.res.spec_1 <- summ_results_area(spec_1, all.occ)

# spec_5
area.res.spec_5 <- summ_results_area(spec_5, all.occ)

# spec_10
area.res.spec_10 <- summ_results_area(spec_10, all.occ)

# spec_Myrt
area.res.spec_Myrt <- summ_results_area(spec_Myrt, all.occ)

# spec_all
area.res.spec_all <- summ_results_area(spec_all, all.occ)

# Plot area results -------------------------------------------------------

## plots with title text

sz <- 4

txt.spec.1 <- ggplot(data.frame(x = 0, y = 1)) +
  geom_text(aes(x,y, 
                label = "           1% most frequent string"), size = sz) +
  theme_void()

txt.spec.5 <- ggplot(data.frame(x = 0, y = 1)) +
  geom_text(aes(x,y, 
                label = "           5% most frequent string"), size = sz) +
  theme_void()

txt.spec.10 <- ggplot(data.frame(x = 0, y = 1)) +
  geom_text(aes(x,y, 
                label = "           10% most frequent string"), size = sz) +
  theme_void()

txt.spec.Myrt <- ggplot(data.frame(x = 1, y = 1)) +
  geom_text(aes(x,y, 
                label = "           Provided by a specialist"), size = sz) +
  theme_void()

txt.spec.all <- ggplot(data.frame(x = 1, y = 1)) +
  geom_text(aes(x,y, 
                label = "           Lists united"), size = sz) +
  theme_void()

lout.mtx <- matrix(c(1,2,3,4,5,
                     6,7,8,9,10,
                     6,7,8,9,10,
                     6,7,8,9,10,
                     6,7,8,9,10,
                     6,7,8,9,10,
                     11,12,13,14,15,
                     11,12,13,14,15,
                     11,12,13,14,15,
                     11,12,13,14,15,
                     11,12,13,14,15),11,5, byrow = T)


#area.res.spec_1
plot.env.spec.1 <- ggplot(area.res.spec_1$data.env, 
                            aes(prop, env.area, group = sp)) +
  geom_line(colour = "red",  alpha = 0.05, size = 1.2) +
  scale_x_continuous(breaks = c(0.1, 0.3, 0.5, 0.7, 0.9)) +
  ylab("Environmental space") +
  xlab("")



plot.geo.spec.1 <- ggplot(area.res.spec_1$data.geo, 
                            aes(prop, geo.area, group = sp)) +
  geom_line(colour = "blue",  alpha = 0.05, size = 1.2) +
  scale_x_continuous(breaks = c(0.1, 0.3, 0.5, 0.7, 0.9)) +
  ylab("Geographical space") +
  xlab("")

#area.res.spec_5
plot.env.spec.5 <- ggplot(area.res.spec_5$data.env, 
                          aes(prop, env.area, group = sp)) +
  geom_line(colour = "red",  alpha = 0.05, size = 1.2) +
  scale_x_continuous(breaks = c(0.1, 0.3, 0.5, 0.7, 0.9)) +
  ylab("") +
  xlab("")

plot.geo.spec.5 <- ggplot(area.res.spec_5$data.geo, 
                          aes(prop, geo.area, group = sp)) +
  geom_line(colour = "blue",  alpha = 0.05, size = 1.2) +
  scale_x_continuous(breaks = c(0.1, 0.3, 0.5, 0.7, 0.9)) +
  ylab("") +
  xlab("")

#area.res.spec_10
plot.env.spec.10 <- ggplot(area.res.spec_10$data.env, 
                          aes(prop, env.area, group = sp)) +
  geom_line(colour = "red",  alpha = 0.05, size = 1.2) +
  scale_x_continuous(breaks = c(0.1, 0.3, 0.5, 0.7, 0.9)) +
  ylab("") +
  xlab("")

plot.geo.spec.10 <- ggplot(area.res.spec_10$data.geo, 
                          aes(prop, geo.area, group = sp)) +
  geom_line(colour = "blue",  alpha = 0.05, size = 1.2) +
  scale_x_continuous(breaks = c(0.1, 0.3, 0.5, 0.7, 0.9)) +
  ylab("") +
  xlab("")

#area.res.spec_Myrt
plot.env.spec.Myrt <- ggplot(area.res.spec_Myrt$data.env, 
                           aes(prop, env.area, group = sp)) +
  geom_line(colour = "red",  alpha = 0.05, size = 1.2) +
  scale_x_continuous(breaks = c(0.1, 0.3, 0.5, 0.7, 0.9)) +
  ylab("") +
  xlab("")

plot.geo.spec.Myrt <- ggplot(area.res.spec_Myrt$data.geo, 
                           aes(prop, geo.area, group = sp)) +
  geom_line(colour = "blue",  alpha = 0.05, size = 1.2) +
  scale_x_continuous(breaks = c(0.1, 0.3, 0.5, 0.7, 0.9)) +
  ylab("") +
  xlab("")


#area.res.spec_all
plot.env.spec.all <- ggplot(area.res.spec_all$data.env, 
                        aes(prop, env.area, group = sp)) +
  geom_line(colour = "red",  alpha = 0.05, size = 1.2) +
  scale_x_continuous(breaks = c(0.1, 0.3, 0.5, 0.7, 0.9)) +
  ylab("") +
  xlab("")

plot.geo.spec.all <- ggplot(area.res.spec_all$data.geo, 
                            aes(prop, geo.area, group = sp)) +
  geom_line(colour = "blue",  alpha = 0.05, size = 1.2) +
  scale_x_continuous(breaks = c(0.1, 0.3, 0.5, 0.7, 0.9)) +
  ylab("") +
  xlab("")


windows(15,7)
grid.arrange(txt.spec.1,
             txt.spec.5,
             txt.spec.10,
             txt.spec.Myrt,
             txt.spec.all,
             plot.env.spec.1,
             plot.env.spec.5,
             plot.env.spec.10,
             plot.env.spec.all, 
             plot.env.spec.Myrt,
             plot.geo.spec.1,
             plot.geo.spec.5,
             plot.geo.spec.10,
             plot.geo.spec.all,
             plot.geo.spec.Myrt,
             ncol = 5,
             layout_matrix = lout.mtx,
             bottom = "Proportion sampled from the list of specialists",
             left = "Remaning area [0-1]")

# plots standand deviation of area ----------------------------------------

# standard deviation
# sd area.res.spec_1
plot.env.spec.1.sd <- ggplot(area.res.spec_1$data.env, 
                             aes(prop, env.area.sd, group = sp)) +
  geom_line(colour = "red",  alpha = 0.05, size = 1.2) +
  scale_x_continuous(breaks = c(0.1, 0.3, 0.5, 0.7, 0.9)) +
  ylab("Environmental space") +
  xlab("")

plot.geo.spec.1.sd <- ggplot(area.res.spec_1$data.geo, 
                             aes(prop, geo.area.sd, group = sp)) +
  geom_line(colour = "blue",  alpha = 0.05, size = 1.2) +
  scale_x_continuous(breaks = c(0.1, 0.3, 0.5, 0.7, 0.9)) +
  ylab("Geographical space") +
  xlab("")

# sd area.res.spec_5
plot.env.spec.5.sd <- ggplot(area.res.spec_5$data.env, 
                             aes(prop, env.area.sd, group = sp)) +
  geom_line(colour = "red",  alpha = 0.05, size = 1.2) +
  scale_x_continuous(breaks = c(0.1, 0.3, 0.5, 0.7, 0.9)) +
  ylab("Environmental space") +
  xlab("")

plot.geo.spec.5.sd <- ggplot(area.res.spec_5$data.geo, 
                             aes(prop, geo.area.sd, group = sp)) +
  geom_line(colour = "blue",  alpha = 0.05, size = 1.2) +
  scale_x_continuous(breaks = c(0.1, 0.3, 0.5, 0.7, 0.9)) +
  ylab("Geographical space") +
  xlab("")

# sd area.res.spec_10
plot.env.spec.10.sd <- ggplot(area.res.spec_10$data.env, 
                             aes(prop, env.area.sd, group = sp)) +
  geom_line(colour = "red",  alpha = 0.05, size = 1.2) +
  scale_x_continuous(breaks = c(0.1, 0.3, 0.5, 0.7, 0.9)) +
  ylab("Environmental space") +
  xlab("")

plot.geo.spec.10.sd <- ggplot(area.res.spec_10$data.geo, 
                             aes(prop, geo.area.sd, group = sp)) +
  geom_line(colour = "blue",  alpha = 0.05, size = 1.2) +
  scale_x_continuous(breaks = c(0.1, 0.3, 0.5, 0.7, 0.9)) +
  ylab("Geographical space") +
  xlab("")

# sd area.res.spec_Myrt
plot.env.spec.myrt.sd <- ggplot(area.res.spec_Myrt$data.env, 
                              aes(prop, env.area.sd, group = sp)) +
  geom_line(colour = "red",  alpha = 0.05, size = 1.2) +
  scale_x_continuous(breaks = c(0.1, 0.3, 0.5, 0.7, 0.9)) +
  ylab("Environmental space") +
  xlab("")

plot.geo.spec.myrt.sd <- ggplot(area.res.spec_Myrt$data.geo, 
                              aes(prop, geo.area.sd, group = sp)) +
  geom_line(colour = "blue",  alpha = 0.05, size = 1.2) +
  scale_x_continuous(breaks = c(0.1, 0.3, 0.5, 0.7, 0.9)) +
  ylab("Geographical space") +
  xlab("")

# sd area.res.spec_all
plot.env.spec.all.sd <- ggplot(area.res.spec_all$data.env, 
                                aes(prop, env.area.sd, group = sp)) +
  geom_line(colour = "red",  alpha = 0.05, size = 1.2) +
  scale_x_continuous(breaks = c(0.1, 0.3, 0.5, 0.7, 0.9)) +
  ylab("Environmental space") +
  xlab("")

plot.geo.spec.all.sd <- ggplot(area.res.spec_all$data.geo, 
                                aes(prop, geo.area.sd, group = sp)) +
  geom_line(colour = "blue",  alpha = 0.05, size = 1.2) +
  scale_x_continuous(breaks = c(0.1, 0.3, 0.5, 0.7, 0.9)) +
  ylab("Geographical space") +
  xlab("")

windows(15,7)
grid.arrange(txt.spec.1,
             txt.spec.5,
             txt.spec.10,
             txt.spec.Myrt,
             txt.spec.all,
             plot.env.spec.1.sd,
             plot.env.spec.5.sd,
             plot.env.spec.10.sd,
             plot.env.spec.all.sd,
             plot.env.spec.myrt.sd,
             plot.geo.spec.1.sd,
             plot.geo.spec.5.sd,
             plot.geo.spec.10.sd,
             plot.geo.spec.all.sd,
             plot.geo.spec.myrt.sd,
             ncol = 5,
             layout_matrix = lout.mtx,
             bottom = "Proportion sampled from the list of specialists",
             left = "Standard deviation of the remaning area")




# complete list of specialists (histograms) -------------------------------

# load results from complete list filtering
spec.1.occ <- readRDS(here("output", "spec_1_occ_classified.rds"))
spec.5.occ <- readRDS(here("output", "spec_5_occ_classified.rds"))
spec.10.occ <- readRDS(here("output", "spec_10_occ_classified.rds"))
spec.myrt.occ <- readRDS(here("output", "spec_Myrt_occ_classified.rds"))
spec.all.occ <- readRDS(here("output", "spec_all_occ_classified.rds"))

# area in enviromental space

env.sp.area.1 <-  as.numeric(spec.1.occ[[1]])/as.numeric(all.occ$env.area)
names(env.sp.area.1) <- names(all.occ$env.area)

env.sp.area.5 <-  as.numeric(spec.5.occ[[1]])/as.numeric(all.occ$env.area)
names(env.sp.area.5) <- names(all.occ$env.area)

env.sp.area.10 <-  as.numeric(spec.10.occ[[1]])/as.numeric(all.occ$env.area)
names(env.sp.area.10) <- names(all.occ$env.area)

env.sp.area.myrt <-  as.numeric(spec.myrt.occ[[1]])/as.numeric(all.occ$env.area)
names(env.sp.area.myrt) <- names(all.occ$env.area)

env.sp.area.all <-  as.numeric(spec.all.occ[[1]])/as.numeric(all.occ$env.area)
names(env.sp.area.myrt) <- names(all.occ$env.area)


gr.env <- rep(c("spec_1", "spec_5", "spec_10", "spec_myrt", "spec_all"), 
              each = length(all.occ$env.area)) 
gr.env <- factor(gr.env,
                 levels = c("spec_1", "spec_5", "spec_10", "spec_myrt", "spec_all"))

env.hist.df <- data.frame(area = c(env.sp.area.1, 
                                   env.sp.area.5,
                                   env.sp.area.10,
                                   env.sp.area.myrt,
                                   env.sp.area.all),
                          list.type = gr.env)


env.hist.df %>% 
  group_by(list.type) %>% 
  summarise(sum(area < 0.3))

# area in geographic space
geo.sp.area.1 <-  as.numeric(spec.1.occ[[2]])/as.numeric(all.occ$geo.area)
names(geo.sp.area.1) <- names(all.occ$geo.area)

geo.sp.area.5 <-  as.numeric(spec.5.occ[[2]])/as.numeric(all.occ$geo.area)
names(geo.sp.area.5) <- names(all.occ$geo.area)

geo.sp.area.10 <-  as.numeric(spec.10.occ[[2]])/as.numeric(all.occ$geo.area)
names(geo.sp.area.10) <- names(all.occ$geo.area)

geo.sp.area.myrt <-  as.numeric(spec.myrt.occ[[2]])/as.numeric(all.occ$geo.area)
names(geo.sp.area.myrt) <- names(all.occ$geo.area)

geo.sp.area.all <-  as.numeric(spec.all.occ[[2]])/as.numeric(all.occ$geo.area)
names(geo.sp.area.myrt) <- names(all.occ$geo.area)


gr.geo <- rep(c("spec_1", "spec_5", "spec_10", "spec_myrt", "spec_all"), 
              each = length(all.occ$geo.area)) 
gr.geo <- factor(gr.geo,        
                 levels = c("spec_1", "spec_5", "spec_10", "spec_myrt", "spec_all"))

geo.hist.df <- data.frame(area = c(geo.sp.area.1, 
                                   geo.sp.area.5,
                                   geo.sp.area.10,
                                   geo.sp.area.myrt,
                                   geo.sp.area.all),
                          list.type = gr.geo)


# Plot histograms ---------------------------------------------------------
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
                                   
env.hist <- ggplot(env.hist.df, aes(area, colour = list.type)) +
  geom_density(size = 1) +
  scale_x_continuous(limits = c(0,1)) +
  scale_color_paletteer_d("ggthemes::gdoc", 
                          name =  "Type of List of Specialist",
                          labels = c("1% most frequent",
                                     "5% most frequent",
                                     "10% most frequent",
                                     "Provided by a specialist",
                                     "Lists united")) +
  ylim(c(0,2.5)) + 
  xlab("Remaning environmental area [0-1]") +
  theme_classic() 

geo.hist <- ggplot(geo.hist.df, aes(area, colour = list.type)) +
  geom_density(size = 1) +
  scale_x_continuous(limits = c(0,1)) +
  ylim(c(0,2.5)) + 
  xlab("Remaning geographical area [0-1]") +
  scale_color_paletteer_d("ggthemes::gdoc") +
  theme_classic() +
  theme(legend.position="none")

leg <- get_legend(env.hist)

env.hist <- env.hist + theme(legend.position="none")

windows(7,7)
grid.arrange(env.hist, geo.hist, leg, ncol = 3,
             layout_matrix = cbind(c(1,2), c(1,2), c(3,3)))


# results from richness ---------------------------------------------------

# spec_1
rich.res.spec_1 <- summ_results_richness(spec_1, all.occ)

# spec_5
rich.res.spec_5 <- summ_results_richness(spec_5, all.occ)

# spec_10
rich.res.spec_10 <- summ_results_richness(spec_10, all.occ)

# spec_Myrt
rich.res.spec_Myrt <- summ_results_richness(spec_Myrt, all.occ)

# spec_all
rich.res.spec_all <- summ_results_richness(spec_all, all.occ)


## plots richness

## plots with title text

sz <- 4

txt.spec.1 <- ggplot(data.frame(x = 0, y = 1)) +
  geom_text(aes(x,y, 
                label = "           1% most frequent string"), size = sz) +
  theme_void()

txt.spec.5 <- ggplot(data.frame(x = 0, y = 1)) +
  geom_text(aes(x,y, 
                label = "           5% most frequent string"), size = sz) +
  theme_void()

txt.spec.10 <- ggplot(data.frame(x = 0, y = 1)) +
  geom_text(aes(x,y, 
                label = "           10% most frequent string"), size = sz) +
  theme_void()

txt.spec.Myrt <- ggplot(data.frame(x = 1, y = 1)) +
  geom_text(aes(x,y, 
                label = "           Provided by a specialist"), size = sz) +
  theme_void()

txt.spec.all <- ggplot(data.frame(x = 1, y = 1)) +
  geom_text(aes(x,y, 
                label = "           Lists united"), size = sz) +
  theme_void()

lout.mtx <- matrix(c(1,2,3,4,5,
                     6,7,8,9,10,
                     6,7,8,9,10,
                     6,7,8,9,10,
                     6,7,8,9,10,
                     6,7,8,9,10,
                     11,12,13,14,15,
                     11,12,13,14,15,
                     11,12,13,14,15,
                     11,12,13,14,15,
                     11,12,13,14,15),11,5, byrow = T)


# spec_1
rich.plot.spec_1.pol <- ggplot(rich.res.spec_1$data.rich.pol, 
                               aes(prop, mean.rich)) +
  geom_path(size = 1, alpha = 0.7) +
  scale_x_continuous(breaks =  c(0.1,0.3,0.5,0.7,0.9)) +
  scale_y_continuous(limits = c(0,1)) +
  geom_errorbar(aes(ymin = lower.rich, ymax = upper.rich, 
                    width = 0.05), alpha = 0.7) +
  ylab("") +
  xlab("")

rich.plot.spec_1.pt <- ggplot(rich.res.spec_1$data.rich.pt, 
                               aes(prop, mean.rich)) +
  geom_path(size = 1, alpha = 0.7) +
  scale_x_continuous(breaks =  c(0.1,0.3,0.5,0.7,0.9)) +
  scale_y_continuous(limits = c(0,1)) +
  geom_errorbar(aes(ymin = lower.rich, ymax = upper.rich, 
                    width = 0.05), alpha = 0.7) +
  ylab("") +
  xlab("")

# spec_5
rich.plot.spec_5.pol <- ggplot(rich.res.spec_5$data.rich.pol, 
                               aes(prop, mean.rich)) +
  geom_path(size = 1, alpha = 0.7) +
  scale_x_continuous(breaks =  c(0.1,0.3,0.5,0.7,0.9)) +
  scale_y_continuous(limits = c(0,1)) +
  geom_errorbar(aes(ymin = lower.rich, ymax = upper.rich, 
                    width = 0.05), alpha = 0.7) +
  ylab("") +
  xlab("")

rich.plot.spec_5.pt <- ggplot(rich.res.spec_5$data.rich.pt, 
                              aes(prop, mean.rich)) +
  geom_path(size = 1, alpha = 0.7) +
  scale_x_continuous(breaks =  c(0.1,0.3,0.5,0.7,0.9)) +
  scale_y_continuous(limits = c(0,1)) +
  geom_errorbar(aes(ymin = lower.rich, ymax = upper.rich, 
                    width = 0.05), alpha = 0.7) +
  ylab("") +
  xlab("")

# spec_10
rich.plot.spec_10.pol <- ggplot(rich.res.spec_10$data.rich.pol, 
                               aes(prop, mean.rich)) +
  geom_path(size = 1, alpha = 0.7) +
  scale_x_continuous(breaks =  c(0.1,0.3,0.5,0.7,0.9)) +
  scale_y_continuous(limits = c(0,1)) +
  geom_errorbar(aes(ymin = lower.rich, ymax = upper.rich, 
                    width = 0.05), alpha = 0.7) +
  ylab("") +
  xlab("")

rich.plot.spec_10.pt <- ggplot(rich.res.spec_10$data.rich.pt, 
                              aes(prop, mean.rich)) +
  geom_path(size = 1, alpha = 0.7) +
  scale_x_continuous(breaks =  c(0.1,0.3,0.5,0.7,0.9)) +
  scale_y_continuous(limits = c(0,1)) +
  geom_errorbar(aes(ymin = lower.rich, ymax = upper.rich, 
                    width = 0.05), alpha = 0.7) +
  ylab("") +
  xlab("")

# spec_Myrt
rich.plot.spec_Myrt.pol <- ggplot(rich.res.spec_Myrt$data.rich.pol, 
                               aes(prop, mean.rich)) +
  geom_path(size = 1, alpha = 0.7) +
  scale_x_continuous(breaks =  c(0.1,0.3,0.5,0.7,0.9)) +
  scale_y_continuous(limits = c(0,1)) +
  geom_errorbar(aes(ymin = lower.rich, ymax = upper.rich, 
                    width = 0.05), alpha = 0.7) +
  ylab("") +
  xlab("")

rich.plot.spec_Myrt.pt <- ggplot(rich.res.spec_Myrt$data.rich.pt, 
                              aes(prop, mean.rich)) +
  geom_path(size = 1, alpha = 0.7) +
  scale_x_continuous(breaks =  c(0.1,0.3,0.5,0.7,0.9)) +
  scale_y_continuous(limits = c(0,1)) +
  geom_errorbar(aes(ymin = lower.rich, ymax = upper.rich, 
                    width = 0.05), alpha = 0.7) +
  ylab("") +
  xlab("")

# spec_all
rich.plot.spec_all.pol <- ggplot(rich.res.spec_all$data.rich.pol, 
                                  aes(prop, mean.rich)) +
  geom_path(size = 1, alpha = 0.7) +
  scale_x_continuous(breaks =  c(0.1,0.3,0.5,0.7,0.9)) +
  scale_y_continuous(limits = c(0,1)) +
  geom_errorbar(aes(ymin = lower.rich, ymax = upper.rich, 
                    width = 0.05), alpha = 0.7) +
  ylab("") +
  xlab("")

rich.plot.spec_all.pt <- ggplot(rich.res.spec_all$data.rich.pt, 
                                 aes(prop, mean.rich)) +
  geom_path(size = 1, alpha = 0.7) +
  scale_x_continuous(breaks =  c(0.1,0.3,0.5,0.7,0.9)) +
  scale_y_continuous(limits = c(0,1)) +
  geom_errorbar(aes(ymin = lower.rich, ymax = upper.rich, 
                    width = 0.05), alpha = 0.7) +
  ylab("") +
  xlab("")

windows(15,7)
grid.arrange(txt.spec.1,
             txt.spec.5,
             txt.spec.10,
             txt.spec.Myrt,
             txt.spec.all,
             rich.plot.spec_1.pol,
             rich.plot.spec_5.pol,
             rich.plot.spec_10.pol,
             rich.plot.spec_Myrt.pol, 
             rich.plot.spec_all.pol,
             rich.plot.spec_1.pt,
             rich.plot.spec_5.pt,
             rich.plot.spec_10.pt,
             rich.plot.spec_Myrt.pt, 
             rich.plot.spec_all.pt,
             ncol = 5,
             layout_matrix = lout.mtx,
             bottom = "Proportion sampled from the list of specialists",
             left = "Correlation of richness from full and filtered dataset")

