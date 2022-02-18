# load packages -----------------------------------------------------------

library(naturaList)
library(tidyverse)
library(here)



# load data ---------------------------------------------------------------

# species occurrence data
# occ.df.kew
load(here("data", "occ_df_kew.RData"))

# specialist files
spec.files <- grep("especialistas", 
                   list.files(here("data"), full.names = T), 
                   value = T)
l.spec.df <- lapply(spec.files, read.csv2)
names(l.spec.df) <- substr(spec.files, 99, nchar(spec.files)-4)


# classify based on each list ---------------------------------------------

# Specialist list: 1% most frequent strings
occ.cl.spec.1 <- classify_occ(occ.df.kew,
                              l.spec.df$especialistas_1porcento,
                              spec.ambiguity = "not.spec")

saveRDS(occ.cl.spec.1, here("output", "occ_cl_spec_1.rds"))

# Specialist list: 5% most frequent strings
occ.cl.spec.5 <- classify_occ(occ.df.kew,
                              l.spec.df$especialistas_5porcento,
                              spec.ambiguity = "not.spec")

saveRDS(occ.cl.spec.5, here("output", "occ_cl_spec_5.rds"))

# Specialist list: 10% most frequent strings
occ.cl.spec.10 <- classify_occ(occ.df.kew,
                              l.spec.df$especialistas_10porcento,
                              spec.ambiguity = "not.spec")

saveRDS(occ.cl.spec.10, here("output", "occ_cl_spec_10.rds"))

# Specialist list: provided by a specialist in Myrtaceae
occ.cl.spec.myrt <- classify_occ(occ.df.kew,
                              l.spec.df$especialistas_Myrtaceae,
                              spec.ambiguity = "not.spec")

saveRDS(occ.cl.spec.myrt, here("output", "occ_cl_spec_myrt.rds"))

# Specialist list: provided by a specialist in Myrtaceae
occ.cl.spec.all <- classify_occ(occ.df.kew,
                                 l.spec.df$especialistas_todos,
                                 spec.ambiguity = "not.spec")

saveRDS(occ.cl.spec.all, here("output", "occ_cl_spec_all.rds"))

