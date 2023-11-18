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


get_rolling_deltaH_series <- function(wide_returns_df,windows = c(360, 720, 1080),file_base_name='crypto_rolling_deltaH/'){
  # Set the number of cores to use
  num_cores <- detectCores()
  registerDoParallel(cores = num_cores)
  
  ## Parallel loop using foreach
  foreach(i = names(wide_returns_df)) %dopar% {
    for (w in windows) {
      
      
      rolling_deltaH <- rollapply(wide_returns_df[,i],w, calcDeltaH)
      rolling_deltaH <- rolling_deltaH |> na.omit()
      rolling_deltaH <- data.frame(date = index(rolling_deltaH), coredata(rolling_deltaH))
      
      file_name <- paste(file_base_name,i,"_rolling_",w,"_deltaH.csv",sep='')
      
      rolling_deltaH |> write.csv(file = file_name)
    }
  }
  
  # Stop the parallel processing
  stopImplicitCluster()
}



