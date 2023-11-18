library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(ROI.plugin.quadprog)
library(ROI.plugin.glpk)
library(doParallel)
library(tidyverse)
library(quantmod)
library(reshape2)
library(parallel)
library(plot3D)

library(readxl)
library(dplyr)
library(GA)
library(xts)
library(zoo)

setwd("~/Documents/Faculdade/IC - Crypto Portfolio Efficiency")

source('utils_R/fractality.R')
source('utils_R/rolling_efficiency.R')
source('utils_R/finance.R')
source('utils_R/trading.R')


library(rvest)

library(quantmod)

# Set the start and end date for the data
dti <- "2018-01-01"
dtf <- "2023-06-30"

# Define the commodity symbols you're interested in
commodity_symbols <- c('WTI','GC=F','BZ=F','ZC=F','ZS=F','KE=F','CT=F')
commodity_names <- c('WTI','GOLD','BRENT','CORN','SOYBEAN','WHEAT','COTTON')

commodity_key_value <- setNames( commodity_names,commodity_symbols)


data_list <- list()
# Loop through each ticker symbol and fetch data
for (symbol in commodity_symbols) {
  tryCatch({
    data <- Ad(getSymbols(symbol, from = dti, to = dtf, auto.assign = FALSE))
    
    colnames(data) <- c(commodity_key_value[symbol])
    
    #colnames(data) <- c(symbol)
    data_list[[symbol]] <- data
  }, error = function(e) {
    cat("Error for symbol", symbol, ":", conditionMessage(e), "\n")
    
  })
}


corn <- data_list$`ZC=F`
corn[is.na(corn)]
brent <- data_list$`BZ=F`
brent[is.na(brent)]
gold <- data_list$`GC=F`
gold[is.na(gold)]


# Combine data from all symbols into a single data frame
cmdt_prices <- do.call(merge, data_list)
cmdt_prices <- na.locf(cmdt_prices) #  some futures have NaN

cmdt_returns <- PerformanceAnalytics::Return.calculate(cmdt_prices,method = 'log') |>na.omit()
view(cmdt_returns)
 
#get_rolling_deltaH_series(stock_returns_bottom20  ,file_base_name = 'cmdt_rolling_deltaH/Bottom_components')


# Random Walk fractallity ------------------------------------------------------

rw_deltaH_360_10000 <-readRDS('deltaH_simulations/rw_deltaH_360_10000.rds')
rw_deltaH_720_10000 <-readRDS('deltaH_simulations/rw_deltaH_720_10000.rds')
rw_deltaH_1080_10000 <-readRDS('deltaH_simulations/rw_deltaH_1080_10000.rds')

q70_360 <- quantile(rw_deltaH_360_10000, 0.70)
q90_360 <- quantile(rw_deltaH_360_10000, 0.90)
q95_360 <- quantile(rw_deltaH_360_10000, 0.95)
q99_360 <- quantile(rw_deltaH_360_10000, 0.99)

q70_720 <- quantile(rw_deltaH_720_10000, 0.70)
q90_720 <- quantile(rw_deltaH_720_10000, 0.90)
q95_720 <- quantile(rw_deltaH_720_10000, 0.95)
q99_720 <- quantile(rw_deltaH_720_10000, 0.99)


q70_1080 <- quantile(rw_deltaH_1080_10000, 0.70)
q90_1080 <- quantile(rw_deltaH_1080_10000, 0.90)
q95_1080 <- quantile(rw_deltaH_1080_10000, 0.95)
q99_1080 <- quantile(rw_deltaH_1080_10000, 0.99)







# consolidate_backtests(deltaH_melt_360_top, threshold_column='q70',position='Short-Inefficiency-Long-Efficiency',tab_returns=returns_long)[[2]]
# consolidate_backtests(deltaH_melt_360_top, threshold_column='q70',position='Short-Inefficiency',tab_returns=returns_long)[[2]]
# consolidate_backtests(deltaH_melt_360_top, threshold_column='q70',position='Short-Efficiency-Long-Inefficiency',tab_returns=returns_long)[[2]]
# consolidate_backtests(deltaH_melt_360_top, threshold_column='q70',position='Short-Efficiency',tab_returns=returns_long)[[2]]
# 




