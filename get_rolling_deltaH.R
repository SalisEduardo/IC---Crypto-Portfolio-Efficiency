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

setwd("~/Documents/Faculdade/IC - Crypto Portfolio Efficiency")

source('utils_R/fractality.R')

crypto_prices <- read.csv("data/Crypto_prices.csv") 
crypto_prices <- na.omit(crypto_prices)
crypto_prices$date <- as.Date(crypto_prices$date)

crypto_prices <- xts(crypto_prices[,-1], order.by = crypto_prices$date)
crypto_returns <- PerformanceAnalytics::Return.calculate(crypto_prices,method = 'log')  
crypto_returns <- na.omit(crypto_returns)

rw_deltaH_360_10000 <-readRDS('deltaH_simulations/rw_deltaH_360_10000.rds')
rw_deltaH_720_10000 <-readRDS('deltaH_simulations/rw_deltaH_720_10000.rds')
rw_deltaH_1080_10000 <-readRDS('deltaH_simulations/rw_deltaH_1080_10000.rds')

# selected_ticker <- 'BTC'
# selected_crypto <- crypto_returns[,selected_ticker]

# cryptos_rolling_360_deltaH <- rollapply(selected_crypto,360, calcDeltaH)
# cryptos_rolling_720_deltaH <- rollapply(selected_crypto,720, calcDeltaH)
# cryptos_rolling_1080_deltaH <- rollapply(selected_crypto,1080, calcDeltaH)


# Set the number of cores to use
num_cores <- detectCores()
registerDoParallel(cores = num_cores)

## Parallel loop using foreach
foreach(i = names(crypto_returns)) %dopar% {
  for (w in c(360, 720, 1080)) {
        
        
        rolling_deltaH <- rollapply(crypto_returns[,i],w, calcDeltaH)
        rolling_deltaH <- rolling_deltaH |> na.omit()
        rolling_deltaH <- data.frame(date = index(rolling_deltaH), coredata(rolling_deltaH))

        file_name <- paste('crypto_rolling_deltaH/',i,"_rolling_",w,"_deltaH.csv",sep='')
        
        rolling_deltaH |> write.csv(file = file_name)
  }
}

# Stop the parallel processing
stopImplicitCluster()
