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
source('utils_R/finance.R')


plot_time_varying_efficiency <- function(df, ticker,window_lenght='360'){
  q70 <- unique(df$q70)[1]
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
    geom_hline(yintercept = q70, linetype = "dashed", color = "darkorange") + 
    geom_hline(yintercept = q90, linetype = "dashed", color = "red") + 
    geom_hline(yintercept = q95, linetype = "dashed", color = "blue") + 
    geom_hline(yintercept = q99, linetype = "dashed", color = "green") +
    annotate("text", x = dt_min, y = q70, label = paste("q70:",q70), vjust = -0.5,hjust=-0.05, color = "darkorange") +
    annotate("text", x = dt_min, y = q90, label = paste("q90:",q90), vjust = -0.5,hjust=-0.05, color = "red") +
    annotate("text", x = dt_min, y = q95, label = paste("q95:",q95), vjust = -0.5,hjust=-0.05, color = "blue") +
    annotate("text", x =  dt_min, y = q99, label = paste("q99:",q99), vjust = -0.5,hjust=-0.05, color = "green") + 
    labs(title = paste(ticker,window_lenght, 'Rolling efficiency',sep=' '), x = 'Date',y= 'Delta H') 
  return(plt)
}

summarise_status_efficiency <- function(df){
  df_summmary <- df |>
    group_by(Ticker) |>
    summarise(
      days_above_q70 = sum(deltaH > q70),
      days_above_q90 = sum(deltaH > q90),
      days_above_q95 = sum(deltaH > q95),
      days_above_q99 = sum(deltaH > q99),
      ticker_appearances = n()
    ) |>
    mutate(
      prop_above_q70 = days_above_q70 / ticker_appearances,
      prop_above_q90 = days_above_q90 / ticker_appearances,
      prop_above_q95 = days_above_q95 / ticker_appearances,
      prop_above_q99 = days_above_q99 / ticker_appearances
    )
  return(df_summmary)
}

backtest_strategy <- function(df, ticker, threshold_column='q70',position='Short-Inefficiency') {
  # Filter the dataframe for the specified ticker
  df$date <- as.Date(df$date)
  ticker_df <- df|> 
    dplyr::filter(Ticker == ticker) |>
    dplyr::left_join(crypto_returns_long,by=c('date','Ticker')) 
  
  ticker_df$signals <- lag(ifelse(ticker_df$deltaH > ticker_df[[threshold_column]], 1, 0),default = 0)
  
  if(position == 'Short-Inefficiency'){
    ticker_df <-ticker_df |> 
      dplyr::mutate(strat_returns = ifelse(signals == 1, return*-1, return*0))
    
  }else if(position == 'Short-Inefficiency-Long-Efficiency'){
    ticker_df <-ticker_df |> 
      dplyr::mutate(strat_returns = ifelse(signals == 1, return*-1, return*1))
  }else if (position == 'Short-Efficiency'){
    ticker_df <-ticker_df |> 
      dplyr::mutate(strat_returns = ifelse(signals == 0, return*-1, return*0))
  
  }else if (position == 'Short-Efficiency-Long-Inefficiency'){
    ticker_df <-ticker_df |> 
      dplyr::mutate(strat_returns = ifelse(signals == 0, return*-1, return*1))
  }else(

    stop('invalid position')
  )

  ticker_df<-ticker_df |>
    dplyr::mutate(strategy_compound = cumprod(1+strat_returns) - 1) # or cumsum
  ticker_df$position <- position
  return(ticker_df)
}




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


deltaH_melt_360 <- read.csv("crypto_rolling_deltaH/deltaH_melt_360.csv") |>
  dplyr::mutate(q70=quantile(rw_deltaH_360_10000, 0.70),
                q90=quantile(rw_deltaH_360_10000, 0.90),
                q95=quantile(rw_deltaH_360_10000, 0.95),
                q99=quantile(rw_deltaH_360_10000, 0.99))
deltaH_melt_720 <- read.csv("crypto_rolling_deltaH/deltaH_melt_720.csv") |>
  dplyr::mutate(q70=quantile(rw_deltaH_720_10000, 0.70),
                q90=quantile(rw_deltaH_720_10000, 0.90),
                q95=quantile(rw_deltaH_720_10000, 0.95),
                q99=quantile(rw_deltaH_720_10000, 0.99))

deltaH_melt_1080 <- read.csv("crypto_rolling_deltaH/deltaH_melt_1080.csv")  |>
  dplyr::mutate(q70=quantile(rw_deltaH_1080_10000, 0.70),
                q90=quantile(rw_deltaH_1080_10000, 0.90),
                q95=quantile(rw_deltaH_1080_10000, 0.95),
                q99=quantile(rw_deltaH_1080_10000, 0.99))



status_efficiency_crypto_360 <- summarise_status_efficiency(deltaH_melt_360)
status_efficiency_crypto_720 <- summarise_status_efficiency(deltaH_melt_720)
status_efficiency_crypto_1080 <- summarise_status_efficiency(deltaH_melt_1080)


# Strategies------------------------------------------------------------------------------


consolidate_backtests <- function(deltah_melt,...){
  backtest_list <- list()
  kpis_list <- list()
  
  for(t in unique(deltah_melt$Ticker)){
    backtest_ticker <- backtest_strategy(deltah_melt,ticker=t,...)
    
    backtest_ticker_xts <- xts(backtest_ticker['strat_returns'],order.by = backtest_ticker$date)
    colnames(backtest_ticker_xts) <- t
    
    ann_rets <- backtest_ticker_xts |> 
      PerformanceAnalytics::table.AnnualizedReturns() |>
      as.data.frame() |>
      rownames_to_column(var = "KPIs")
    
    #total_ret = data.frame(KPIs='TotalReturn', t = get_totalReturn(backtest_ticker_xts$BTC_short_Inefficiency))
    
    total_ret <- data.frame(KPIs='TotalReturn')
    total_ret[t] = get_totalReturn(backtest_ticker_xts)
    
    
    KPIs_df <- rbind(ann_rets,total_ret)
    KPIs_df_wide <- KPIs_df |>
      pivot_wider(names_from = KPIs, values_from = t ) |>
      dplyr::mutate(Ticker = t)

    backtest_list[[t]] <- backtest_ticker
    kpis_list[[t]] <- KPIs_df_wide
  }
  
  concatenated_backtests <- do.call(rbind, backtest_list)
  rownames(concatenated_backtests) <- NULL
  
  position_kpis <- unique(concatenated_backtests$position)[1] 
  concatenated_kpis <- do.call(rbind, kpis_list)
  concatenated_kpis$position <- position_kpis

  return(list(concatenated_backtests, concatenated_kpis))
  
}

consolidate_backtests(deltaH_melt_360, threshold_column='q70',position='Short-Inefficiency-Long-Efficiency')[[2]]
consolidate_backtests(deltaH_melt_360, threshold_column='q70',position='Short-Inefficiency')[[2]]
consolidate_backtests(deltaH_melt_360, threshold_column='q70',position='Short-Efficiency-Long-Inefficiency')[[2]]
consolidate_backtests(deltaH_melt_360, threshold_column='q70',position='Short-Efficiency')[[2]]

