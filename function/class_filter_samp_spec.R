class_filter_samp_spec <- function(occ.df, 
                                   spec.list,
                                   sample.spec = 0.1 #value between 0 and 1
                                   ) {
  
  spec.ID <- unique(spec.list$ID)
  smp.size <- round(length(spec.ID)*sample.spec, 0)
  
  smp.spec <- sample(spec.ID, smp.size)
  
  new.spec.list <- spec.list[spec.list$ID %in% smp.spec,]
  
  cl.occ <- classify_occ(occ.df, 
                         new.spec.list, 
                         spec.ambiguity = "not.spec")
  
 ft.occ <- filter(cl.occ, naturaList_levels == "1_det_by_spec")

 ft.occ
}
