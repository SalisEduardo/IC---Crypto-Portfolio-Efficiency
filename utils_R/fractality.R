library(MFDFA)
library(foreach)
library(doParallel)



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


generate_random_walk_returns <- function(N) {
  # Initialize the vector to store prices
  prices <- numeric(N)
  prices[1] <- 300 # initial price
  
  # Generate random walk process
  for (i in 2:N) {
    prices[i] <- prices[i - 1] + rnorm(1, 0, 1)
  }
  
  # Compute returns
  returns <- diff(log(prices))
  
  return(returns)
}


generate_experiments <-  function(time_series_length,n_simulations = 1000){
  
  left_simulations <- n_simulations

  
  vec_deltaH <- vector("numeric", n_simulations)
  
  # each iteration creates a  time series with a Brownian Motion generative process
  
  for(i in 1:n_simulations){
    brownian_motion <- generate_brownian_motion(time_series_length)     

    deltaH_Brownian_motion <- calcDeltaH(brownian_motion,N=time_series_length)
    vec_deltaH[i] <- deltaH_Brownian_motion # saving its delta H
    left_simulations <- left_simulations - 1
    print(paste("The are more", as.character(left_simulations), "to go"))
    print(paste(round(((n_simulations-left_simulations)/n_simulations) * 100,2), "%","Completed"))
    
  }
  
  return(vec_deltaH)
}


generate_experiments_parallel <-  function(time_series_length,n_simulations = 1000){
  
  # Set the number of cores you want to use for parallel processing
  # Change 'num_cores' to the desired number of cores
  # Get the total number of cores
  numOfCores <- detectCores()
  
 
  # Register all the cores
  registerDoParallel(numOfCores)
  
  left_simulations <- n_simulations
  vec_deltaH <- vector("numeric", n_simulations)
  
  vec_deltaH <- foreach(i = 1:n_simulations, .combine = 'c') %dopar% {
    brownian_motion <- generate_brownian_motion(time_series_length)     
    deltaH_Brownian_motion <- calcDeltaH(brownian_motion, N = time_series_length)
    vec_deltaH[i] <- deltaH_Brownian_motion # saving its delta H
    left_simulations <- left_simulations - 1
    print(paste("There are more", as.character(left_simulations), "to go"))
    print(paste(round(((n_simulations - left_simulations) / n_simulations) * 100, 2), "%", "Completed"))
    return(deltaH_Brownian_motion)
  }
  
  # Stop the parallel backend and clean up
  
  return(vec_deltaH)
}


generate_experiments_parallel_v2 <-  function(time_series_length,n_simulations = 1000){
  
  # Set the number of cores you want to use for parallel processing
  # Change 'num_cores' to the desired number of cores
  # Get the total number of cores
  numOfCores <- detectCores()
  
  
  # Register all the cores
  registerDoParallel(numOfCores)
  
  left_simulations <- n_simulations
  
  vec_deltaH <- vector("numeric", n_simulations)
  vec_deltaH <- foreach(i = 1:n_simulations, .combine = 'c') %dopar% {
    brownian_motion <- generate_random_walk_returns(time_series_length) # Different methodology to generate RW returns
    deltaH_Brownian_motion <- calcDeltaH(brownian_motion, N = time_series_length)
    vec_deltaH[i] <- deltaH_Brownian_motion # saving its delta H
    left_simulations <- left_simulations - 1
    print(paste("There are more", as.character(left_simulations), "to go"))
    print(paste(round(((n_simulations - left_simulations) / n_simulations) * 100, 2), "%", "Completed"))
    return(deltaH_Brownian_motion)
  }
  
  # Stop the parallel backend and clean up
  
  return(vec_deltaH)
}



