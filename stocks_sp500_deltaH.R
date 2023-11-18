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
source('utils_R/stocks.R')

library(rvest)


# # # Get SP500 components
# webpage <- read_html("https://www.slickcharts.com/sp500")
# 
# table_node <- html_nodes(webpage, "table")
# 
# sp500_components <- html_table(table_node)[[1]]
# 
# sp500_components$`#` <- NULL
# 
# names(sp500_components) <- c('Company','Symbol',"Weight","Price",'Chg', 'Pct_chg')#
# sp500_components |> write.csv(paste("data/SP500_weights",Sys.Date(),".csv",sep='_'))
# 
# 
# sp500_components$Weight |> as.numeric()
# 
# top_20_sp500 <- sp500_components |>
#   mutate(Weight= as.numeric(gsub("%", "", Weight)) ) |>
#   dplyr::arrange(desc(Weight)) |>
#   dplyr::top_n(20) |>
#   dplyr::pull(Symbol)
# 
# data_list <- list()
# 
# # Loop through each ticker symbol and fetch data
# for (symbol in top_20_sp500) {
#   tryCatch({
#     data <- quantmod::Ad(getSymbols(symbol, from = "2018-01-01", to = "2023-06-30", auto.assign = FALSE))
#     colnames(data) <- c(symbol)
#     data_list[[symbol]] <- data
#   })
# 
# }
# 
# # Combine data from all symbols into a single data frame
# stocks_prices <- do.call(merge, data_list)
# stocks_returns <- PerformanceAnalytics::Return.calculate(stocks_prices,method = 'log') |> na.omit()
# 
# 
# returns_df <- data.frame(date = index(stocks_returns), coredata(stocks_returns))
# 
# returns_long <- melt(returns_df, id.vars = "date", variable.name = "Ticker", value.name = "return")
# returns_long$date <- as.Date(returns_long$date)
# 

sp500_components <- get_sp500_current_components(F)



stock_returns_top20 <- get_sp500_components_returns(sp500_components,topN = 20,dataset_format = "wide")
returns_long_top20 <- get_sp500_components_returns(sp500_components,topN = 20)

stock_returns_bottom20 <- get_sp500_components_returns(sp500_components,topN = -20,dataset_format = "wide")
returns_long_bottom20 <- get_sp500_components_returns(sp500_components,topN = -20)



#get_rolling_deltaH_series(stock_returns ,file_base_name = 'stocks_rolling_deltaH/Top_components')
#get_rolling_deltaH_series(stock_returns_bottom20  ,file_base_name = 'stocks_rolling_deltaH/Bottom_components')


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




