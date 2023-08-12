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
setwd("~/Documents/Faculdade/IC - Crypto Portfolio Efficiency")

crypto_prices <- read.csv("data/Crypto_prices.csv") 
crypto_prices <- na.omit(crypto_prices)
crypto_prices$date <- as.Date(crypto_prices$date)

crypto_prices <- xts(crypto_prices[,-1], order.by = crypto_prices$date)
crypto_returns <- PerformanceAnalytics::Return.calculate(crypto_prices,method = 'log')

crypto_returns_selected <- crypto_returns['2019-06/',c("BTC","ETH","XRP","ADA")]
#crypto_returns_selected <- crypto_returns['2019-06/',]
crypto_names <- colnames(crypto_returns_selected)


insample_data <- crypto_returns_selected['2019-06/2022-06']
outsample_data <- crypto_returns_selected['2022-07/']


base_specs <- portfolio.spec(colnames(insample_data)) |>
  add.constraint(type = "full_investment") 

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


portfolio_InSample_EW <-  Return.portfolio(R = insample_data,
                                           weights = rep(1/length(crypto_names),length(crypto_names)))



deltaH_insample <- calcDeltaH(portfolio_InSample)



# n_simulations <- 1000
# left_simulations <- n_simulations
# time_series_length <- dim(insample_data)[1]
# 
# #hurst_exponents <- vector("numeric", n_simulations)
# 
# 
# 
# vec_deltaH <- vector("numeric", n_simulations)
# 
# for (i in 1:n_simulations) {
#   brownian_motion <- generate_brownian_motion(time_series_length)
#   #print(brownian_motion)
#   N <- time_series_length
#   # result <- MFDFA(brownian_motion,scale=10:(N/4),q=-4:4,m=1)
#   # hurst_exponents[i] <- result$Hq[2]
#   
#   deltaH_Brownian_motion <- calcDeltaH(brownian_motion,N=N)
#   vec_deltaH[i] <- deltaH_Brownian_motion
#   left_simulations <- left_simulations - 1
#   print(paste("The are more", as.character(left_simulations), "to go"))
#   print(paste(round(((n_simulations-left_simulations)/n_simulations) * 100,2), "%","Completed"))
#   
# }
# 
# 
# save(vec_deltaH,file = "deltaH_simulations/vec_1000_deltaH.RData")


n_simulations <- 10000
time_series_length <- dim(insample_data)[1]


# Uncomend if its is need to run new simulations - takes a lott of time !!!

# vec_deltaH <- generate_experiments(time_series_length,n_simulations=n_simulations )
# save(vec_deltaH,file = "deltaH_simulations/vec_10000_deltaH.RData")

vec_deltaH <- load('deltaH_simulations/vec_10000_deltaH.RData')
load('deltaH_simulations/vec_10000_deltaH.RData')


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

size_outsample <- dim(portfolio_OutSample)[1]
data_rolling_deltaH <- rbind(tail(portfolio_InSample,size_outsample),portfolio_OutSample) 
rolling_deltaH <- rollapply(data_rolling_deltaH,size_outsample, calcDeltaH)
rolling_deltaH <- rolling_deltaH |> na.omit()
colnames(rolling_deltaH) <- c("deltaH")


crypto_names_string <- paste(crypto_names,collapse = " ")

ggplot(data = fortify(rolling_deltaH)) +
  geom_line(aes(x = Index, y = deltaH))+
  geom_hline(yintercept = q90, linetype = "dashed", color = "red") + 
  geom_hline(yintercept = q95, linetype = "dashed", color = "blue") + 
  geom_hline(yintercept = q99, linetype = "dashed", color = "green") +
  annotate("text", x = min(index(rolling_deltaH)), y = q90, label = paste("q90:",q90), vjust = -0.5,hjust=-0.05, color = "red") +
  annotate("text", x = min(index(rolling_deltaH)), y = q95, label = paste("q95:",q95), vjust = -0.5,hjust=-0.05, color = "blue") +
  annotate("text", x = min(index(rolling_deltaH)), y = q99, label = paste("q99:",q99), vjust = -0.5,hjust=-0.05, color = "green") + 
  labs(title = paste('Delta H of Global Minimum Variance - Long Only',crypto_names_string), x = 'Date',y= 'Delta H') 



portfolio_OutSample_EW <- Return.portfolio(R = outsample_data,
                                           weights = rep(1/length(crypto_names),length(crypto_names)))

size_outsample_EW <- dim(portfolio_OutSample_EW)[1]

data_rolling_deltaH_EW <- rbind(tail(portfolio_InSample_EW  ,size_outsample_EW),portfolio_OutSample_EW) 
rolling_deltaH_EW <- rollapply(data_rolling_deltaH_EW,size_outsample_EW, calcDeltaH)
rolling_deltaH_EW <- rolling_deltaH_EW |> na.omit()
colnames(rolling_deltaH_EW) <- c("deltaH")

ggplot(data = fortify(rolling_deltaH_EW)) +
  geom_line(aes(x = Index, y = deltaH))+
  geom_hline(yintercept = q90, linetype = "dashed", color = "red") + 
  geom_hline(yintercept = q95, linetype = "dashed", color = "blue") + 
  geom_hline(yintercept = q99, linetype = "dashed", color = "green") +
  annotate("text", x = min(index(rolling_deltaH)), y = q90, label = paste("q90:",q90), vjust = -0.5,hjust=-0.05, color = "red") +
  annotate("text", x = min(index(rolling_deltaH)), y = q95, label = paste("q95:",q95), vjust = -0.5,hjust=-0.05, color = "blue") +
  annotate("text", x = min(index(rolling_deltaH)), y = q99, label = paste("q99:",q99), vjust = -0.5,hjust=-0.05, color = "green") + 
  labs(title = paste('Delta H of Equal Weight',crypto_names_string), x = 'Date',y= 'Delta H') 






# Analyzing Efficency levels and portfolio returns  -------------------------------------------------


gmv_roi_infos <- cbind(portfolio_OutSample,rolling_deltaH) 

gmv_roi_infos <-  gmv_roi_infos |> na.omit() #first day NA
gmv_roi_infos$q90 <- q90
gmv_roi_infos$q95 <- q95
gmv_roi_infos$q99 <- q99



source('utils_R/finance.R')


gmv_roi_infos$cumulative_returns <- get_cumulative_returns(gmv_roi_infos$portfolio.returns) 

gmv_roi_infos$std_15d <- runSD(gmv_roi_infos$portfolio.returns, n=15)
gmv_roi_infos$mean_returns_15d <- rollapply(gmv_roi_infos$portfolio.returns, width = 15, FUN = function(x) mean(x))



gmv_roi_infos$performance_15d <- rollapply(gmv_roi_infos$portfolio.returns, width = 15, FUN = get_cumulative_returns_window,align = "right")




df_gmv_roi_infos <- gmv_roi_infos |>
  as.data.frame() |>
  dplyr::mutate(status_q90 = ifelse(deltaH < q90,"Bellow_q90","Above_q90")) |>
  dplyr::mutate(status_q95 = ifelse(deltaH < q95,"Bellow_q95","Above_q95")) |>
  dplyr::mutate(status_q99 = ifelse(deltaH < q99,"Bellow_q99","Above_q99")) 


kpis_status_q90 <- df_gmv_roi_infos |>

  dplyr::group_by(status_q90) |>
  dplyr::summarise(mean_return = mean(portfolio.returns), 
                   sd_returns = sd(portfolio.returns)) |>
  dplyr::mutate(sharp = mean_return/sd_returns)  |>
  dplyr::rename("status" = status_q90)

kpis_status_q95 <- df_gmv_roi_infos |>
  dplyr::group_by(status_q95) |>
  dplyr::summarise(mean_return = mean(portfolio.returns), 
                   sd_returns = sd(portfolio.returns)) |>
  dplyr::mutate(sharp = mean_return/sd_returns)  |>
  dplyr::rename("status" = status_q95)

kpis_status_q99 <- df_gmv_roi_infos |>
  dplyr::group_by(status_q99) |>
  dplyr::summarise(mean_return = mean(portfolio.returns), 
                   sd_returns = sd(portfolio.returns)) |>
  dplyr::mutate(sharp = mean_return/sd_returns)  |>
  dplyr::rename("status" = status_q99)

kpis_status <- rbind(kpis_status_q90,kpis_status_q95,kpis_status_q99)


df_gmv_roi_infos |> 
  ggplot(aes(x=deltaH,y=mean_returns_15d)) +
  geom_point() + 
  geom_smooth(method=lm , color="red", se=FALSE) + 
  labs(y = 'Rolling 15 days Return',x= "Delta H",title=paste('Return x DeltaH for GMV- ',crypto_names_string)   )


df_gmv_roi_infos |> 
  ggplot(aes(x=deltaH,y=std_15d)) +
  geom_point() + 
  geom_smooth(method=lm , color="red", se=FALSE) +
  labs(y = 'Rolling 15 days Std',x= "Delta H",title=paste('Volatility x DeltaH for GMV- ',crypto_names_string)   )

  
df_gmv_roi_infos |> 
  ggplot(aes(x=deltaH,y=performance_15d)) +
  geom_point() + 
  geom_smooth(method=lm , color="red", se=FALSE) +
  labs(y = 'Rolling 15 days Cumulative Return',x= "Delta H",title=paste('Performance x DeltaH for GMV- ',crypto_names_string)   )

exposure_gmvROI <- track_portfolio_weights(outsample_data[,crypto_names],weights_gmv_roi)  |> 
  as.data.frame() |>
  tibble::rownames_to_column(var = "date") |>
  pivot_longer(cols = -date, names_to = "Assets", values_to = "Exposure")


weights_gmv_roi |> 
  as.data.frame() |>
  tibble::rownames_to_column(var = "Assets") |>
  mutate(rowname = NULL) |>
  dplyr::right_join(exposure_gmvROI,by='Assets')




# REBALANCING SYSTEM -------------------------------------------------------------------------------


lookback_period_rebal <- 360 * 3

day_above <-  df_gmv_roi_infos |>
  tibble::rownames_to_column(var = "date") |>
  dplyr::filter(status_q90 == "Above_q90") |>
  dplyr::filter(date == min(date)) |>
  dplyr::pull(date)  
  

day_above <-  df_gmv_roi_infos |>
  tibble::rownames_to_column(var = "date") |>
  dplyr::filter(status_q95 == "Above_q95") |>
  dplyr::filter(date == min(date)) |>
  dplyr::pull(date)  

day_above <-  df_gmv_roi_infos |>
  tibble::rownames_to_column(var = "date") |>
  dplyr::filter(status_q99 == "Above_q99") |>
  dplyr::filter(date == min(date)) |>
  dplyr::pull(date)  

look_back_start_date <- as.Date(day_above) -  lookback_period_rebal

first_day_new_portfolio <- as.Date(day_above) +  1
 
returns_before_rebal <- portfolio_OutSample[paste("/",day_above,sep='')]
deltaH_before_rebal <- rolling_deltaH[paste("/",day_above,sep='')]
reb_returns <- list(returns_before_rebal)

estimation_dataset <- crypto_returns_selected[paste(look_back_start_date,"/",day_above,sep='')]

new_gmv_roi <- optimize.portfolio(R = estimation_dataset, 
                   portfolio = gmvp_specs, 
                   optimize_method = "ROI",trace = TRUE)

new_weights_gmv_roi <- extractWeights(new_gmv_roi)

new_portfolio_insample <- Return.portfolio(R = estimation_dataset,
                                           weights = new_weights_gmv_roi)

new_portfolio_OutSample <- Return.portfolio(R = outsample_data[paste(first_day_new_portfolio,"/",sep='')],
                                            weights = new_weights_gmv_roi)

new_theorical_fractallity  <- rbind(tail(new_portfolio_insample  ,length(new_portfolio_insample)),new_portfolio_OutSample) 
new_theorical_rolling_deltaH <- rollapply(new_theorical_fractallity,length(new_portfolio_insample), calcDeltaH)
new_theorical_rolling_deltaH <- new_theorical_rolling_deltaH |> na.omit()
colnames(new_theorical_rolling_deltaH) <- c("deltaH")




before_rebal_deltaH_longtable <- fortify(deltaH_before_rebal) |>
  mutate(Situation="Before Rebalincing")
after_rebal_deltaH_longtable <- fortify(new_theorical_rolling_deltaH)|>
  mutate(Situation="After Rebalincing")

comparison_deltaH_rebal <- rbind(before_rebal_deltaH_longtable,after_rebal_deltaH_longtable)


#ggplot(data = fortify(new_theorical_rolling_deltaH)) +
ggplot(data = comparison_deltaH_rebal) +
  geom_line(aes(x = Index, y = deltaH,color=Situation))+
  geom_hline(yintercept = q90, linetype = "dashed", color = "red") + 
  geom_hline(yintercept = q95, linetype = "dashed", color = "blue") + 
  geom_hline(yintercept = q99, linetype = "dashed", color = "green") +
  annotate("text", x = min(index(rolling_deltaH)), y = q90, label = paste("q90:",q90), vjust = -0.5,hjust=-0.05, color = "red") +
  annotate("text", x = min(index(rolling_deltaH)), y = q95, label = paste("q95:",q95), vjust = -0.5,hjust=-0.05, color = "blue") +
  annotate("text", x = min(index(rolling_deltaH)), y = q99, label = paste("q99:",q99), vjust = -0.5,hjust=-0.05, color = "green") + 
  labs(title = paste('Rebal delta H',crypto_names_string), x = 'Date',y= 'Delta H') 


consolidated_rebalancing = rbind(returns_before_rebal,new_portfolio_OutSample)


# With Rebalancing and without rebalancing -----------------------------------------------------------------------------

table.AnnualizedReturns(portfolio_OutSample) |>
  cbind(table.AnnualizedReturns(consolidated_rebalancing)) |>
  cbind(table.AnnualizedReturns(returns_before_rebal)) |> 
  cbind(table.AnnualizedReturns(new_portfolio_OutSample))
  



PerformanceAnalytics::charts.PerformanceSummary(cbind(portfolio_OutSample,consolidated_rebalancing), colorset=rich6equal,
                                                lwd=2, cex.legend = 1.0, event.labels = TRUE, main = "")

