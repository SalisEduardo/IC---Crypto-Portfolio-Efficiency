library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(ROI.plugin.quadprog)
library(ROI.plugin.glpk)
library(doParallel)
library(tidyverse)
library(quantmod)
library(parallel)
library(plot3D)
library(readxl)
library(dplyr)
library(GA)
library(xts)
library(zoo)

source('utils_R/fractality.R')


crypto_prices <- read.csv("data/Crypto_prices.csv") 
crypto_prices <- na.omit(crypto_prices)
crypto_prices$date <- as.Date(crypto_prices$date)

crypto_prices <- xts(crypto_prices[,-1], order.by = crypto_prices$date)
crypto_returns <- PerformanceAnalytics::Return.calculate(crypto_prices,method = 'log')
crypto_returns_selected <- crypto_returns['2019-06/',c("BTC","ETH","XRP","ADA")]

insample_data <- crypto_returns_selected['2019-06/2022-06']
outsample_data <- crypto_returns_selected['2022-07/']


gmvp_specs <-portfolio.spec(colnames(insample_data)) |>
  #add.constraint(type = "long_only") |> 
  add.constraint(type = "full_investment") |> 
  add.constraint(type = 'box',min=0,max=0.6) |>
  add.objective(type = "risk", name = "StdDev",risk_aversion=9999)



gmvp_ROI <- optimize.portfolio(R = insample_data, 
                               portfolio = gmvp_specs, 
                               optimize_method = "ROI",trace = TRUE)

weights_gmv_roi <- extractWeights(gmvp_ROI)
weights_gmv_roi


portfolio_InSample <- Return.portfolio(R = insample_data,
                                       weights = weights_gmv_roi)


deltaH_insample <- calcDeltaH(portfolio_InSample)



n_simulations <- 10000
left_simulations <- n_simulations
time_series_length <- dim(insample_data)[1]

#hurst_exponents <- vector("numeric", n_simulations)

vec_deltaH <- vector("numeric", n_simulations)

for (i in 1:n_simulations) {
  brownian_motion <- generate_brownian_motion(time_series_length)
  #print(brownian_motion)
  N <- time_series_length
  # result <- MFDFA(brownian_motion,scale=10:(N/4),q=-4:4,m=1)
  # hurst_exponents[i] <- result$Hq[2]
  
  deltaH_Brownian_motion <- calcDeltaH(brownian_motion,N=N)
  vec_deltaH[i] <- deltaH_Brownian_motion
  left_simulations <- left_simulations - 1
  print(paste("The are more", as.character(left_simulations), "to go"))
  print(paste(round(((n_simulations-left_simulations)/n_simulations) * 100,2), "%","Completed"))
  
}

lower_ci <- quantile(vec_deltaH, 0.025)
upper_ci <- quantile(vec_deltaH, 0.975)

print(paste("Lower Confidence Interval:", round(lower_ci, 4)))
print(paste("Upper Confidence Interval:", round(upper_ci, 4)))

q90 <- quantile(vec_deltaH, 0.90)
q95 <- quantile(vec_deltaH, 0.95)
q99 <- quantile(vec_deltaH, 0.99)


print(paste("DeltaH In-sample Portfolio:", round(deltaH_insample, 4)))
print(paste("0.9 quantile:", round(q90, 4)))
print(paste("0.95 quantile:", round(q95, 4)))
print(paste("0.99 quantile:", round(q99, 4)))





portfolio_OutSample <- Return.portfolio(R = outsample_data,
                                       weights = weights_gmv_roi)

size_outsample <- dim(portfolio_InSample)[1]




data_rolling_deltaH <- rbind(tail(portfolio_InSample,size_outsample),portfolio_OutSample) 
rolling_deltaH <- rollapply(data_rolling_deltaH,size_outsample, calcDeltaH)

rolling_deltaH <- rolling_deltaH |> na.omit()

colnames(rolling_deltaH) <- c("deltaH")

class(plot(rolling_deltaH))





ggplot(data = fortify(rolling_deltaH)) +
  geom_line(aes(x = Index, y = deltaH))+
  geom_hline(yintercept = q90, linetype = "dashed", color = "red") + 
  geom_hline(yintercept = q95, linetype = "dashed", color = "blue") + 
  geom_hline(yintercept = q99, linetype = "dashed", color = "green") +
  annotate("text", x = min(index(rolling_deltaH)), y = q90, label = "q90", vjust = -0.5, color = "red") +
  annotate("text", x = min(index(rolling_deltaH)), y = q95, label = "q95", vjust = -0.5, color = "blue") +
  annotate("text", x = min(index(rolling_deltaH)), y = q99, label = "q99", vjust = -0.5, color = "green")
  


