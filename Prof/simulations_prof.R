
load("Prof/resSim.RData")

generate_experiments_parallel_v2 <-  function(){
  
  # Set the number of cores you want to use for parallel processing
  # Change 'num_cores' to the desired number of cores
  # Get the total number of cores
  numOfCores <- detectCores()
  
  n_simulations <- dim(retornos)[2]

  # Register all the cores
  registerDoParallel(numOfCores)
  
  left_simulations <- n_simulations
  
  vec_deltaH <- foreach(i = 1:n_simulations, .combine = 'c') %dopar% {
        
    deltaH_Brownian_motion <- calcDeltaH(retornos[,i], N = length(retornos[,i]))
    vec_deltaH[i] <- deltaH_Brownian_motion # saving its delta H
    left_simulations <- left_simulations - 1
    print(paste("There are more", as.character(left_simulations), "to go"))
    print(paste(round(((n_simulations - left_simulations) / n_simulations) * 100, 2), "%", "Completed"))
    return(deltaH_Brownian_motion)
  }
  
  # Stop the parallel backend and clean up
  
  return(vec_deltaH)
}



