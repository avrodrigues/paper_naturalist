bind_spec_df <- function(df1, df2) {
  list.df <- list(df1, df2)
  
  l1 <- length(names(df1))
  l2 <- length(names(df2))
  
  max.df <- which.max(c(l1, l2))
  min.df <- ifelse(max.df == 1, 2, 1)
  
  add <- !names(list.df[[max.df]]) %in% names(list.df[[min.df]])
  
  add.names <- names(list.df[[max.df]])[add]
  
  list.df[[min.df]][,add.names] <- rep("", nrow(list.df[[min.df]]))
  list.df[[min.df]] <- list.df[[min.df]][,names(list.df[[max.df]])]
  
  do.call(rbind, list.df)
}
