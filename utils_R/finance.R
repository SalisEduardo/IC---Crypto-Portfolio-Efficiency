
get_totalReturn <- function(dailyRets){
  total_rets <- prod(1 + dailyRets) - 1
  return(total_rets)
}



get_cumulative_returns <- function(dailyRets){
  cum_rets <- cumprod(1 + dailyRets) - 1
  return(cum_rets)
}

get_cumulative_returns_window <- function(window_dailyRets) {
  cum_rets <- cumprod(1 + window_dailyRets) - 1
  return(cum_rets[length(cum_rets)])  # Return the last cumulative return in the window
}



# Function to track changes in portfolio weights over time
track_portfolio_weights <- function(asset_returns, initial_weights) {
  # Calculate the final portfolio weights at each time step
  portfolio_weights <- matrix(NA, nrow = nrow(asset_returns) + 1, ncol = ncol(asset_returns))
  portfolio_weights[1, ] <- initial_weights
  
  for (i in 1:nrow(asset_returns)) {
    portfolio_weights[i + 1, ] <- portfolio_weights[i, ] * (1 + asset_returns[i, ])
    portfolio_weights[i + 1, ] <- portfolio_weights[i + 1, ] / sum(portfolio_weights[i + 1, ])
  }
  
  
  portfolio_weights <- portfolio_weights |> as.data.frame()
  colnames(portfolio_weights) <- colnames(asset_returns)
  
  dates_Seq <- index(outsample_data[,crypto_names] ) |> min() -1
  dates_Seq <- c(dates_Seq,index(outsample_data[,crypto_names] ))
  
  portfolio_weights <- xts(portfolio_weights,order.by = dates_Seq)
  

  
  # Return the portfolio weights
  return(portfolio_weights)
}


