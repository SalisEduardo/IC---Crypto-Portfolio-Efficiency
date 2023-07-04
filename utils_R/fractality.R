library(MFDFA)



calcMDM <- function(x, N= dim(x)[1],scale=10:(N/4),q=-4:4,m=1){
  b <- MFDFA(x, scale, m, q)
  
  #effic <-  max(b[["Hq"]]) - min(b[["Hq"]])
  mdm <- (abs(b[["Hq"]][1] - 0.5) + abs(b[["Hq"]][9]-0.5))/2
  
  return(mdm)
  
}

calcDeltaH <- function(x, N= dim(x)[1],scale=10:(N/4),q=-4:4,m=1){
  b <- MFDFA(x, scale, m, q)
  
  #effic <-  max(b[["Hq"]]) - min(b[["Hq"]])
  deltaH <- max(b[["Hq"]]) - min(b[["Hq"]]) #Delta h
  
  return(deltaH)
  
}

table.assets.fractality <- function(R,fractality_function, name_fractality){
  frac <- sapply(R, fractality_function)
  frac <- frac %>%  as.data.frame()
  
  colnames(frac) <- c(name_fractality) 
  frac$Ticker <- rownames(frac)
  rownames(frac) <- NULL
  
  return(frac[c("Ticker",name_fractality)])
  
  
}

get_top_effic_names <- function(df_ranks,rankCol,top_effics){
  df_ranks <- df_ranks[c("Ticker",rankCol)] %>%  top_n(-top_effics)   # top in de descending order
  
  names <- df_ranks$Ticker
  
  return(names)
}


generate_brownian_motion <- function(n) {
  dt <- 1 / n
  dz <- rnorm(n)
  w <- cumsum(sqrt(dt) * dz)
  return(w)
}






