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

library(rvest)


read_most_recent_csv <- function(folder_path) {
  # List all CSV files in the folder
  csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)
  
  # Check if there are any CSV files in the folder
  if (length(csv_files) == 0) {
    cat("No CSV files found in the folder.\n")
    return(NULL)
  } else {
    # Find the most recently modified CSV file
    latest_csv <- csv_files[which.max(file.info(csv_files)$mtime)]
    
    # Read the most recently modified CSV file
    data <- read.csv(file = latest_csv)
    
    # Print the file name and the data
    cat("Most recently updated CSV file:", latest_csv, "\n")
    return(data)
  }
}



get_sp500_current_components <- function(update_database=TRUE,export_new_data=TRUE){
  if(update_database){
    # Get SP500 components
    webpage <- read_html("https://www.slickcharts.com/sp500")
    
    table_node <- html_nodes(webpage, "table")
    
    sp500_components <- html_table(table_node)[[1]]
    
    sp500_components$`#` <- NULL
    
    names(sp500_components) <- c('Company','Symbol',"Weight","Price",'Chg', 'Pct_chg')
    
    if(export_new_data){
      sp500_components |> write.csv(paste("sp500_weights/weights_",Sys.Date(),".csv",sep=''))
      
    }
    
  }else{
    sp500_components <- read_most_recent_csv('sp500_weights/')
  }
  
  

  return(sp500_components)

  
}

get_sp500_components_returns <- function(sp500_components,topN=20,dti="2018-01-01",dtf="2023-06-30",dataset_format="long"){
  
  top_N_sp500 <- sp500_components |>
    dplyr::mutate(Weight = as.numeric(gsub("%", "", Weight))) |>
    dplyr::arrange(desc(Weight)) |>
    top_n(topN,Weight) |>
    dplyr::pull(Symbol)
  
  data_list <- list()
  
  # Loop through each ticker symbol and fetch data
  for (symbol in top_N_sp500) {
    tryCatch({
      data <- quantmod::Ad(getSymbols(symbol, from = dti, to = dtf, auto.assign = FALSE))
      colnames(data) <- c(symbol)
      data_list[[symbol]] <- data
    }, error = function(e) {
      cat("Error for symbol", symbol, ":", conditionMessage(e), "\n")
      
    })
  }
  
  # Combine data from all symbols into a single data frame
  stocks_prices <- do.call(merge, data_list)
  stocks_returns <- PerformanceAnalytics::Return.calculate(stocks_prices,method = 'log') |> na.omit()
  
  
  returns_df <- data.frame(date = index(stocks_returns), coredata(stocks_returns))
  
  returns_long <- melt(returns_df, id.vars = "date", variable.name = "Ticker", value.name = "return")
  returns_long$date <- as.Date(returns_long$date)
  
  if(dataset_format=='long'){
    return(returns_long)
  }else{
    return(stocks_returns)
  }
  
}

