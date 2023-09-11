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

crypto_prices <- read.csv("data/Crypto_prices.csv") 
crypto_prices <- na.omit(crypto_prices)
crypto_prices$date <- as.Date(crypto_prices$date)

crypto_prices <- xts(crypto_prices[,-1], order.by = crypto_prices$date)
crypto_returns <- PerformanceAnalytics::Return.calculate(crypto_prices,method = 'log')  
crypto_returns <- na.omit(crypto_returns)

crypto_returns_df <- data.frame(date = index(crypto_returns), coredata(crypto_returns))

crypto_returns_long <- melt(crypto_returns_df, id.vars = "date", variable.name = "Ticker", value.name = "return")
crypto_returns_long$date <- as.Date(crypto_returns_long$date)

rw_deltaH_360_10000 <-readRDS('deltaH_simulations/rw_deltaH_360_10000.rds')
rw_deltaH_720_10000 <-readRDS('deltaH_simulations/rw_deltaH_720_10000.rds')
rw_deltaH_1080_10000 <-readRDS('deltaH_simulations/rw_deltaH_1080_10000.rds')





q90_360 <- quantile(rw_deltaH_360_10000, 0.90)
q95_360 <- quantile(rw_deltaH_360_10000, 0.95)
q99_360 <- quantile(rw_deltaH_360_10000, 0.99)


q90_720 <- quantile(rw_deltaH_720_10000, 0.90)
q95_720 <- quantile(rw_deltaH_720_10000, 0.95)
q99_720 <- quantile(rw_deltaH_720_10000, 0.99)


q90_1080 <- quantile(rw_deltaH_1080_10000, 0.90)
q95_1080 <- quantile(rw_deltaH_1080_10000, 0.95)
q99_1080 <- quantile(rw_deltaH_1080_10000, 0.99)


deltaH_melt_360 <- read.csv("crypto_rolling_deltaH/deltaH_melt_360.csv") |>
  dplyr::mutate(q90=quantile(rw_deltaH_360_10000, 0.90),
                q95=quantile(rw_deltaH_360_10000, 0.95),
                q99=quantile(rw_deltaH_360_10000, 0.99))
deltaH_melt_720 <- read.csv("crypto_rolling_deltaH/deltaH_melt_720.csv") |>
  dplyr::mutate(q90=quantile(rw_deltaH_720_10000, 0.90),
                q95=quantile(rw_deltaH_720_10000, 0.95),
                q99=quantile(rw_deltaH_720_10000, 0.99))

deltaH_melt_1080 <- read.csv("crypto_rolling_deltaH/deltaH_melt_1080.csv")  |>
  dplyr::mutate(q90=quantile(rw_deltaH_1080_10000, 0.90),
                q95=quantile(rw_deltaH_1080_10000, 0.95),
                q99=quantile(rw_deltaH_1080_10000, 0.99))


plot_time_varying_efficiency <- function(df, ticker,window_lenght='360'){
  
  q90 <- unique(df$q90)[1]
  q95 <- unique(df$q95)[1]
  q99 <- unique(df$q99)[1]
  
  df_plot <- df |> 
    dplyr::filter(Ticker == ticker) |>
    dplyr::mutate(date = as.Date(date))
  
  dt_min <- as.Date(min(df_plot$date))

  plt <- df_plot |>
    ggplot(aes(x=date,y=deltaH,group=1)) + 
    geom_line() +
    geom_hline(yintercept = q90, linetype = "dashed", color = "red") + 
    geom_hline(yintercept = q95, linetype = "dashed", color = "blue") + 
    geom_hline(yintercept = q99, linetype = "dashed", color = "green") +
    annotate("text", x = dt_min, y = q90, label = paste("q90:",q90), vjust = -0.5,hjust=-0.05, color = "red") +
    annotate("text", x = dt_min, y = q95, label = paste("q95:",q95), vjust = -0.5,hjust=-0.05, color = "blue") +
    annotate("text", x =  dt_min, y = q99, label = paste("q99:",q99), vjust = -0.5,hjust=-0.05, color = "green") + 
    labs(title = paste(ticker,window_lenght, 'Rolling efficiency',sep=' '), x = 'Date',y= 'Delta H') 
  return(plt)
}


summaryize_status_efficiency <- function(df){
  df_summmary <- df |>
    group_by(Ticker) |>
    summarise(
      days_above_q90 = sum(deltaH > q90),
      days_above_q95 = sum(deltaH > q95),
      days_above_q99 = sum(deltaH > q99),
      ticker_appearances = n()
    ) |>
    mutate(
      prop_above_q90 = days_above_q90 / ticker_appearances,
      prop_above_q95 = days_above_q95 / ticker_appearances,
      prop_above_q99 = days_above_q99 / ticker_appearances
    )
  return(df_summmary)
}

backtest_strategy <- function(df, ticker, threshold_column,position_deltaH='short') {
  # Filter the dataframe for the specified ticker
  ticker_df <- df|> 
    dplyr::filter(Ticker == ticker) |>
    dplyr::left_join(crypto_returns_long,by='date') |>
    dplyr::mutate(signals = lag(ifelse(ticker_df$deltaH > ticker_df[[threshold_column]], 1, 0),default = 0))

  
  
  
  #returns <- ifelse(signals == 1, ticker_df$deltaH / lag(ticker_df$deltaH) - 1, 0)
  
  return(returns)
}


status_efficiency_crypto_360 <- summaryize_status_efficiency(deltaH_melt_360)
status_efficiency_crypto_720 <- summaryize_status_efficiency(deltaH_melt_720)
status_efficiency_crypto_1080 <- summaryize_status_efficiency(deltaH_melt_1080)





  